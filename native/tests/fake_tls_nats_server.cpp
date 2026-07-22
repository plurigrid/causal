// Local TLS NATS fixture for certificate and hostname verification tests.

#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/select.h>
#include <sys/socket.h>
#include <unistd.h>

#include <openssl/err.h>
#include <openssl/ssl.h>

#include <chrono>
#include <csignal>
#include <cstdlib>
#include <iostream>
#include <string>

static bool SendAll(SSL* ssl, const std::string& value) {
  size_t offset = 0;
  while (offset < value.size()) {
    int count = SSL_write(ssl, value.data() + offset,
                          static_cast<int>(value.size() - offset));
    if (count <= 0) return false;
    offset += static_cast<size_t>(count);
  }
  return true;
}

int main(int argc, char** argv) {
  if (argc != 5) {
    std::cerr << "usage: fake-tls-nats PORT LIFETIME CERT KEY\n";
    return 2;
  }
  std::signal(SIGPIPE, SIG_IGN);
  const int port = std::atoi(argv[1]);
  const int lifetime = std::atoi(argv[2]);
  SSL_CTX* context = SSL_CTX_new(TLS_server_method());
  if (!context || SSL_CTX_use_certificate_chain_file(context, argv[3]) != 1 ||
      SSL_CTX_use_PrivateKey_file(context, argv[4], SSL_FILETYPE_PEM) != 1) {
    ERR_print_errors_fp(stderr);
    return 1;
  }
  SSL_CTX_set_min_proto_version(context, TLS1_2_VERSION);
  int server = socket(AF_INET, SOCK_STREAM, 0);
  int reuse = 1;
  setsockopt(server, SOL_SOCKET, SO_REUSEADDR, &reuse, sizeof(reuse));
  sockaddr_in address{};
  address.sin_family = AF_INET;
  address.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
  address.sin_port = htons(static_cast<uint16_t>(port));
  if (bind(server, reinterpret_cast<sockaddr*>(&address), sizeof(address)) < 0 ||
      listen(server, 1) < 0) {
    std::perror("fake-tls-nats listen");
    return 1;
  }
  std::cout << "listening " << port << std::endl;
  int client = accept(server, nullptr, nullptr);
  SSL* ssl = SSL_new(context);
  SSL_set_fd(ssl, client);
  if (SSL_accept(ssl) != 1) {
    ERR_print_errors_fp(stderr);
    return 1;
  }
  SendAll(ssl, "INFO {\"server_id\":\"haiku-tls-test\",\"version\":\"0.1\","
               "\"tls_required\":true,\"max_payload\":1048576}\r\n");
  auto deadline = std::chrono::steady_clock::now() + std::chrono::seconds(lifetime);
  char buffer[4096];
  std::string input;
  bool proof_sent = false;
  while (std::chrono::steady_clock::now() < deadline) {
    fd_set reads;
    FD_ZERO(&reads);
    FD_SET(client, &reads);
    timeval timeout{1, 0};
    int ready = SSL_pending(ssl) > 0
        ? 1 : select(client + 1, &reads, nullptr, nullptr, &timeout);
    if (ready < 0) break;
    if (ready > 0) {
      int count = SSL_read(ssl, buffer, sizeof(buffer));
      if (count <= 0) break;
      input.append(buffer, static_cast<size_t>(count));
      std::cout.write(buffer, count);
      std::cout.flush();
      const std::string prefix = "SUB chat.room.lobby ";
      size_t start = input.find(prefix);
      if (!proof_sent && start != std::string::npos) {
        start += prefix.size();
        size_t end = input.find("\r\n", start);
        if (end != std::string::npos) {
          std::string sid = input.substr(start, end - start);
          std::string payload =
              "{\"id\":\"tls-proof-1\",\"sender\":\"verified\","
              "\"text\":\"Certificate and hostname checks passed.\"}";
          SendAll(ssl, "MSG chat.room.lobby " + sid + " " +
                       std::to_string(payload.size()) + "\r\n" + payload + "\r\n");
          proof_sent = true;
        }
      }
    } else if (!SendAll(ssl, "PING\r\n")) {
      break;
    }
  }
  SSL_shutdown(ssl);
  SSL_free(ssl);
  close(client);
  close(server);
  SSL_CTX_free(context);
  return 0;
}
