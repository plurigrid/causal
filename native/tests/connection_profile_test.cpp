#include "../connection_profile.h"

#include <sys/stat.h>
#include <unistd.h>

#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iostream>
#include <iterator>
#include <string>

namespace {

int Fail(const std::string& message) {
  std::cerr << "profile-test: " << message << std::endl;
  return 1;
}

}  // namespace

int main(int argc, char** argv) {
  if (argc != 2) return Fail("expected an isolated profile path");
  setenv("CAUSAL_PROFILE_PATH", argv[1], 1);

  ConnectionProfile profile;
  profile.host = "chat.example.test";
  profile.port = "4443";
  profile.tls = true;
  profile.tls_name = "chat.example.test";
  profile.ca_file = "/tmp/test-ca.pem";
  profile.token = "must-not-persist-profile-secret";
  profile.remember_secret = true;
  profile.jetstream = true;
  profile.stream = "CAUSAL";
  profile.consumer = "profile_test";
  profile.petname = "profile-test";
  profile.history = false;

  std::string error;
  if (ValidateConnectionProfile(profile, error) != B_OK)
    return Fail("valid TLS profile rejected: " + error);
  ConnectionProfile unsafe = profile;
  unsafe.tls = false;
  if (ValidateConnectionProfile(unsafe, error) != B_NOT_ALLOWED)
    return Fail("plaintext credentials were not rejected");
  unsafe.token.clear();
  if (ValidateConnectionProfile(unsafe, error) != B_NOT_ALLOWED)
    return Fail("plaintext durable replay was not rejected");

  status_t result = ConnectionProfileStore::Save(profile);
  if (result != B_OK)
    return Fail(std::string("save failed: ") + strerror(result));

  struct stat info{};
  if (stat(argv[1], &info) != 0) return Fail("saved profile is missing");
  if ((info.st_mode & 0777) != 0600) return Fail("profile mode is not 0600");

  std::ifstream raw(argv[1], std::ios::binary);
  std::string bytes((std::istreambuf_iterator<char>(raw)),
                    std::istreambuf_iterator<char>());
  if (bytes.find("must-not-persist-profile-secret") != std::string::npos)
    return Fail("secret appeared in flattened profile bytes");

  ConnectionProfile loaded;
  result = ConnectionProfileStore::Load(loaded);
  if (result != B_OK)
    return Fail(std::string("load failed: ") + strerror(result));
  if (loaded.host != profile.host || loaded.port != profile.port ||
      loaded.tls != profile.tls || loaded.tls_name != profile.tls_name ||
      loaded.ca_file != profile.ca_file || loaded.user != profile.user ||
      loaded.remember_secret != profile.remember_secret ||
      loaded.jetstream != profile.jetstream ||
      loaded.stream != profile.stream || loaded.consumer != profile.consumer ||
      loaded.petname != profile.petname || loaded.history != profile.history)
    return Fail("round-trip changed a non-secret profile field");
  if (!loaded.token.empty() || !loaded.password.empty())
    return Fail("load materialized a secret from the profile file");

  result = ConnectionProfileStore::Remove();
  if (result != B_OK || access(argv[1], F_OK) == 0)
    return Fail("isolated profile cleanup failed");
  std::cout << "connection profile checks passed" << std::endl;
  return 0;
}
