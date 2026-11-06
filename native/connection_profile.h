#ifndef CAUSAL_CONNECTION_PROFILE_H
#define CAUSAL_CONNECTION_PROFILE_H

#include <SupportDefs.h>

#include <string>

struct ConnectionProfile {
  std::string host{"nonlocal.info"};
  std::string port{"4222"};
  bool tls{false};
  std::string tls_name;
  std::string ca_file;
  std::string token;
  std::string user;
  std::string password;
  bool remember_secret{false};
  bool jetstream{false};
  std::string stream{"CAUSAL"};
  std::string consumer;
  std::string petname{"haiku"};
  bool history{true};
  std::string history_path;
};

bool HasConnectionEnvironment();
ConnectionProfile ConnectionProfileFromEnvironment();
status_t ValidateConnectionProfile(const ConnectionProfile& profile,
                                   std::string& error);
void SecureClear(std::string& value);

class ConnectionProfileStore {
 public:
  static status_t Load(ConnectionProfile& profile);
  static status_t Save(const ConnectionProfile& profile);
  static status_t Remove();
  static std::string Path();
};

class ConnectionSecretStore {
 public:
  static status_t Retrieve(const ConnectionProfile& profile,
                           std::string& secret);
  static status_t Store(const ConnectionProfile& profile,
                        const std::string& secret);
  static status_t Remove(const ConnectionProfile& profile);

  static std::string Identifier(const ConnectionProfile& profile);
  static std::string SecondaryIdentifier(const ConnectionProfile& profile);
};

#endif
