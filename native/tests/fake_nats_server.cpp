// Deterministic localhost NATS lifecycle fixture for Haiku recovery tests.

#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/select.h>
#include <sys/socket.h>
#include <unistd.h>

#include <chrono>
#include <cstdlib>
#include <iostream>
#include <string>

static bool SendAll(int fd, const std::string& value) {
  size_t offset = 0;
  while (offset < value.size()) {
    ssize_t count = send(fd, value.data() + offset, value.size() - offset, 0);
    if (count <= 0) return false;
    offset += static_cast<size_t>(count);
  }
  return true;
}

int main(int argc, char** argv) {
  int port = argc > 1 ? std::atoi(argv[1]) : 44222;
  int lifetime = argc > 2 ? std::atoi(argv[2]) : 30;
  int server = socket(AF_INET, SOCK_STREAM, 0);
  int reuse = 1;
  setsockopt(server, SOL_SOCKET, SO_REUSEADDR, &reuse, sizeof(reuse));
  sockaddr_in address{};
  address.sin_family = AF_INET;
  address.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
  address.sin_port = htons(static_cast<uint16_t>(port));
  if (bind(server, reinterpret_cast<sockaddr*>(&address), sizeof(address)) < 0 ||
      listen(server, 1) < 0) {
    std::perror("fake-nats listen");
    return 1;
  }
  std::cout << "listening " << port << std::endl;
  int client = accept(server, nullptr, nullptr);
  if (client < 0) return 1;
  SendAll(client, "INFO {\"server_id\":\"haiku-test\",\"version\":\"0.1\","
                  "\"max_payload\":1048576}\r\n");
  auto deadline = std::chrono::steady_clock::now() + std::chrono::seconds(lifetime);
  char buffer[4096];
  std::string input;
  bool fixture_sent = false;
  while (std::chrono::steady_clock::now() < deadline) {
    fd_set reads;
    FD_ZERO(&reads);
    FD_SET(client, &reads);
    timeval timeout{1, 0};
    int ready = select(client + 1, &reads, nullptr, nullptr, &timeout);
    if (ready < 0) break;
    if (ready > 0) {
      ssize_t count = recv(client, buffer, sizeof(buffer), 0);
      if (count <= 0) break;
      input.append(buffer, static_cast<size_t>(count));
      std::cout.write(buffer, count);
      std::cout.flush();
      const std::string prefix = "SUB chat.room.lobby ";
      size_t start = input.find(prefix);
      if (!fixture_sent && start != std::string::npos) {
        start += prefix.size();
        size_t end = input.find("\r\n", start);
        if (end != std::string::npos) {
          const std::string sid = input.substr(start, end - start);
          const std::string messages[] = {
              "{\"id\":\"visual-aurora\",\"sender\":\"aurora\","
              "\"text\":\"The room recovered its durable signal.\"}",
              "{\"id\":\"visual-cobalt\",\"sender\":\"cobalt\","
              "\"text\":\"A second observer joined without changing the room.\"}",
              "{\"id\":\"visual-coral\",\"sender\":\"coral\","
              "\"text\":\"Color names the path; transport still names authority.\"}",
          };
          for (const std::string& payload : messages) {
            if (!SendAll(client, "MSG chat.room.lobby " + sid + " " +
                                  std::to_string(payload.size()) + "\r\n" +
                                  payload + "\r\n")) {
              break;
            }
          }
          fixture_sent = true;
        }
      }
    } else if (!SendAll(client, "PING\r\n")) {
      break;
    }
  }
  shutdown(client, SHUT_RDWR);
  close(client);
  close(server);
  return 0;
}
