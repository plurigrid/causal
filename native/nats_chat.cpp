// Minimal libroot/BSD-socket NATS chat probe.
// Build on Haiku with: c++ -std=c++17 -O2 -o nats-chat native/nats_chat.cpp

#include <arpa/inet.h>
#include <netdb.h>
#include <sys/socket.h>
#include <unistd.h>

#include <cstdlib>
#include <cstring>
#include <iostream>
#include <string>

static bool send_all(int fd, const std::string& s) {
  size_t off = 0;
  while (off < s.size()) {
    ssize_t n = send(fd, s.data() + off, s.size() - off, 0);
    if (n <= 0) return false;
    off += static_cast<size_t>(n);
  }
  return true;
}

static bool flush_server(int fd) {
  if (!send_all(fd, "PING\r\n")) return false;
  std::string input;
  char buf[4096];
  while (input.size() < 64 * 1024) {
    ssize_t n = recv(fd, buf, sizeof(buf), 0);
    if (n <= 0) return false;
    input.append(buf, static_cast<size_t>(n));
    if (input.find("-ERR") != std::string::npos) return false;
    if (input.find("PING\r\n") != std::string::npos &&
        !send_all(fd, "PONG\r\n")) {
      return false;
    }
    if (input.find("PONG\r\n") != std::string::npos) return true;
  }
  return false;
}

static int connect_tcp(const char* host, const char* port) {
  addrinfo hints{};
  hints.ai_socktype = SOCK_STREAM;
  hints.ai_family = AF_UNSPEC;
  addrinfo* result = nullptr;
  if (getaddrinfo(host, port, &hints, &result) != 0) return -1;
  int fd = -1;
  for (addrinfo* p = result; p; p = p->ai_next) {
    fd = socket(p->ai_family, p->ai_socktype, p->ai_protocol);
    if (fd >= 0 && connect(fd, p->ai_addr, p->ai_addrlen) == 0) break;
    if (fd >= 0) close(fd);
    fd = -1;
  }
  freeaddrinfo(result);
  return fd;
}

static int run(int argc, char** argv) {
  if (argc < 3 || (std::string(argv[1]) != "tail" && argc < 4)) {
    std::cerr << "usage: nats-chat tail SUBJECT... | say SUBJECT TEXT\n";
    return 2;
  }
  const char* host = std::getenv("NATS_HOST");
  const char* port = std::getenv("NATS_PORT");
  host = host ? host : "nonlocal.info";
  port = port ? port : "4222";
  int fd = connect_tcp(host, port);
  if (fd < 0) { std::perror("connect"); return 1; }
  char buf[4096];
  ssize_t n = recv(fd, buf, sizeof(buf) - 1, 0); // INFO
  if (n <= 0) { close(fd); return 1; }
  std::string connect_cmd = "CONNECT {\"lang\":\"haiku-libroot\",\"version\":\"0.1\"}\r\n";
  if (!send_all(fd, connect_cmd)) { close(fd); return 1; }
  const std::string subject = argv[2];
  if (std::string(argv[1]) == "tail") {
    for (int i = 2; i < argc; ++i) {
      if (!send_all(fd, "SUB " + std::string(argv[i]) + " " + std::to_string(i - 1) + "\r\n")) {
        close(fd); return 1;
      }
      std::cout << "listening " << argv[i] << "\n";
    }
    while ((n = recv(fd, buf, sizeof(buf) - 1, 0)) > 0) {
      buf[n] = 0;
      std::string frame(buf, static_cast<size_t>(n));
      if (frame.find("PING\r\n") != std::string::npos &&
          !send_all(fd, "PONG\r\n")) break;
      std::cout << frame << std::flush;
    }
  } else {
    const std::string body = argv[3];
    if (!send_all(fd, "PUB " + subject + " " + std::to_string(body.size()) + "\r\n" + body + "\r\n")) {
      close(fd); return 1;
    }
    if (!flush_server(fd)) {
      std::cerr << "server did not acknowledge the publish flush\n";
      close(fd); return 1;
    }
  }
  close(fd);
  return 0;
}

int main(int argc, char** argv) { return run(argc, argv); }
