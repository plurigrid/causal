#include "../connection_profile.h"

#include <Application.h>

#include <iostream>
#include <cstring>
#include <string>

int main() {
  BApplication app("application/x-vnd.plurigrid-causal-chat-keystore-test");
  ConnectionProfile profile;
  profile.host = "127.0.0.1";
  profile.port = "46931";
  profile.tls = true;
  profile.user = "fixture";
  const std::string expected = "non-sensitive-causal-keystore-fixture";

  ConnectionSecretStore::Remove(profile);
  status_t result = ConnectionSecretStore::Store(profile, expected);
  if (result != B_OK) {
    std::cerr << "keystore store failed: " << strerror(result) << std::endl;
    return 1;
  }
  std::string actual;
  result = ConnectionSecretStore::Retrieve(profile, actual);
  if (result != B_OK || actual != expected) {
    std::cerr << "keystore retrieve mismatch: " << strerror(result) << std::endl;
    ConnectionSecretStore::Remove(profile);
    SecureClear(actual);
    return 1;
  }
  SecureClear(actual);
  result = ConnectionSecretStore::Remove(profile);
  if (result != B_OK) {
    std::cerr << "keystore remove failed: " << strerror(result) << std::endl;
    return 1;
  }
  result = ConnectionSecretStore::Retrieve(profile, actual);
  SecureClear(actual);
  if (result != B_ENTRY_NOT_FOUND) {
    std::cerr << "removed key remained retrievable: " << strerror(result)
              << std::endl;
    return 1;
  }
  std::cout << "connection KeyStore checks passed" << std::endl;
  return 0;
}
