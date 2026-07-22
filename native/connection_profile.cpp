#include "connection_profile.h"

#include <Directory.h>
#include <Entry.h>
#include <File.h>
#include <FindDirectory.h>
#include <Key.h>
#include <KeyStore.h>
#include <Message.h>
#include <Path.h>

#include <cerrno>
#include <cctype>
#include <cstdlib>
#include <cstring>

namespace {

constexpr int32 kProfileVersion = 1;
const char* const kKeyringName = "CausalChat";

std::string EnvOr(const char* name, const char* fallback) {
  const char* value = std::getenv(name);
  return value && *value ? value : fallback;
}

bool EnvBool(const char* name, bool fallback) {
  const char* value = std::getenv(name);
  if (!value || !*value) return fallback;
  return std::strcmp(value, "1") == 0 || std::strcmp(value, "true") == 0 ||
         std::strcmp(value, "yes") == 0;
}

bool HasEnv(const char* name) {
  const char* value = std::getenv(name);
  return value && *value;
}

bool ValidNatsName(const std::string& value) {
  if (value.empty() || value.size() > 48) return false;
  for (unsigned char c : value) {
    if (!(std::isalnum(c) || c == '-' || c == '_')) return false;
  }
  return true;
}

status_t ProfileLocation(BPath& directory, std::string& leaf) {
  std::string profilePath = ConnectionProfileStore::Path();
  if (profilePath.empty()) return B_ERROR;
  BPath profile(profilePath.c_str());
  status_t result = profile.GetParent(&directory);
  if (result != B_OK) return result;
  leaf = profile.Leaf();
  if (leaf.empty()) return B_BAD_VALUE;
  if (create_directory(directory.Path(), 0700) != B_OK && errno != EEXIST)
    return B_ERROR;
  BDirectory node(directory.Path());
  if (node.InitCheck() == B_OK) node.SetPermissions(0700);
  return B_OK;
}

status_t FindString(const BMessage& message, const char* name,
                    std::string& value) {
  const char* text = nullptr;
  status_t result = message.FindString(name, &text);
  if (result == B_OK) value = text ? text : "";
  return result;
}

}  // namespace

bool HasConnectionEnvironment() {
  constexpr const char* names[] = {
      "NATS_HOST",          "NATS_PORT",       "NATS_TLS",
      "NATS_TLS_NAME",      "NATS_CA_FILE",    "NATS_TOKEN",
      "NATS_USER",          "NATS_PASSWORD",   "NATS_JETSTREAM",
      "NATS_STREAM",        "NATS_CONSUMER",   "CAUSAL_PETNAME",
      "CAUSAL_HISTORY",     "CAUSAL_HISTORY_PATH"};
  for (const char* name : names)
    if (HasEnv(name)) return true;
  return false;
}

ConnectionProfile ConnectionProfileFromEnvironment() {
  ConnectionProfile profile;
  profile.host = EnvOr("NATS_HOST", "nonlocal.info");
  profile.port = EnvOr("NATS_PORT", "4222");
  profile.tls = EnvBool("NATS_TLS", false);
  profile.tls_name = EnvOr("NATS_TLS_NAME", profile.host.c_str());
  profile.ca_file = EnvOr("NATS_CA_FILE", "");
  profile.token = EnvOr("NATS_TOKEN", "");
  profile.user = EnvOr("NATS_USER", "");
  profile.password = EnvOr("NATS_PASSWORD", "");
  profile.jetstream = EnvBool("NATS_JETSTREAM", false);
  profile.stream = EnvOr("NATS_STREAM", "CAUSAL");
  profile.consumer = EnvOr("NATS_CONSUMER", "");
  profile.petname = EnvOr("CAUSAL_PETNAME", "haiku");
  profile.history = EnvBool("CAUSAL_HISTORY", true);
  profile.history_path = EnvOr("CAUSAL_HISTORY_PATH", "");
  return profile;
}

status_t ValidateConnectionProfile(const ConnectionProfile& profile,
                                   std::string& error) {
  if (profile.host.empty() || profile.host.size() > 253) {
    error = "Host is required and must be at most 253 characters.";
    return B_BAD_VALUE;
  }
  if (profile.port.empty()) {
    error = "Port is required.";
    return B_BAD_VALUE;
  }
  unsigned long port = 0;
  for (unsigned char c : profile.port) {
    if (!std::isdigit(c)) {
      error = "Port must be a number from 1 through 65535.";
      return B_BAD_VALUE;
    }
    port = port * 10 + static_cast<unsigned long>(c - '0');
    if (port > 65535) break;
  }
  if (port == 0 || port > 65535) {
    error = "Port must be a number from 1 through 65535.";
    return B_BAD_VALUE;
  }
  const bool has_token = !profile.token.empty();
  const bool has_user = !profile.user.empty() || !profile.password.empty();
  if (has_token && has_user) {
    error = "Choose token authentication or user/password, not both.";
    return B_BAD_VALUE;
  }
  if (has_user && (profile.user.empty() || profile.password.empty())) {
    error = "User and password must be supplied together.";
    return B_BAD_VALUE;
  }
  if ((has_token || has_user) && !profile.tls) {
    error = "Credentials are refused until verified TLS is enabled.";
    return B_NOT_ALLOWED;
  }
  if (profile.jetstream && !profile.tls) {
    error = "Durable replay requires verified TLS.";
    return B_NOT_ALLOWED;
  }
  if (profile.jetstream &&
      (!ValidNatsName(profile.stream) || !ValidNatsName(profile.consumer))) {
    error = "Stream and consumer must use letters, digits, dash or underscore.";
    return B_BAD_VALUE;
  }
  if (profile.petname.empty() || profile.petname.size() > 48) {
    error = "Display name is required and must be at most 48 characters.";
    return B_BAD_VALUE;
  }
  error.clear();
  return B_OK;
}

void SecureClear(std::string& value) {
  volatile char* bytes = value.empty() ? nullptr : &value[0];
  for (size_t i = 0; i < value.size(); ++i) bytes[i] = 0;
  value.clear();
}

std::string ConnectionProfileStore::Path() {
  const char* custom = std::getenv("CAUSAL_PROFILE_PATH");
  if (custom && *custom) return custom;
  BPath directory;
  if (find_directory(B_USER_SETTINGS_DIRECTORY, &directory) != B_OK)
    return {};
  directory.Append("CausalChat");
  directory.Append("profile");
  return directory.Path();
}

status_t ConnectionProfileStore::Load(ConnectionProfile& profile) {
  std::string path = Path();
  if (path.empty()) return B_ERROR;
  BFile file(path.c_str(), B_READ_ONLY);
  status_t result = file.InitCheck();
  if (result != B_OK) return result;
  BMessage message;
  result = message.Unflatten(&file);
  if (result != B_OK) return result;
  int32 version = 0;
  if (message.FindInt32("version", &version) != B_OK ||
      version != kProfileVersion)
    return B_BAD_DATA;

  ConnectionProfile loaded;
  if (FindString(message, "host", loaded.host) != B_OK ||
      FindString(message, "port", loaded.port) != B_OK ||
      message.FindBool("tls", &loaded.tls) != B_OK ||
      FindString(message, "tls_name", loaded.tls_name) != B_OK ||
      FindString(message, "ca_file", loaded.ca_file) != B_OK ||
      FindString(message, "user", loaded.user) != B_OK ||
      message.FindBool("remember_secret", &loaded.remember_secret) != B_OK ||
      message.FindBool("jetstream", &loaded.jetstream) != B_OK ||
      FindString(message, "stream", loaded.stream) != B_OK ||
      FindString(message, "consumer", loaded.consumer) != B_OK ||
      FindString(message, "petname", loaded.petname) != B_OK ||
      message.FindBool("history", &loaded.history) != B_OK)
    return B_BAD_DATA;
  profile = std::move(loaded);
  return B_OK;
}

status_t ConnectionProfileStore::Save(const ConnectionProfile& profile) {
  BPath directory;
  std::string leaf;
  status_t result = ProfileLocation(directory, leaf);
  if (result != B_OK) return result;

  BMessage message;
  message.AddInt32("version", kProfileVersion);
  message.AddString("host", profile.host.c_str());
  message.AddString("port", profile.port.c_str());
  message.AddBool("tls", profile.tls);
  message.AddString("tls_name", profile.tls_name.c_str());
  message.AddString("ca_file", profile.ca_file.c_str());
  message.AddString("user", profile.user.c_str());
  message.AddBool("remember_secret", profile.remember_secret);
  message.AddBool("jetstream", profile.jetstream);
  message.AddString("stream", profile.stream.c_str());
  message.AddString("consumer", profile.consumer.c_str());
  message.AddString("petname", profile.petname.c_str());
  message.AddBool("history", profile.history);

  BPath next(directory);
  next.Append((leaf + ".next").c_str());
  {
    BFile file(next.Path(), B_WRITE_ONLY | B_CREATE_FILE | B_ERASE_FILE);
    result = file.InitCheck();
    if (result != B_OK) return result;
    result = file.SetPermissions(0600);
    if (result != B_OK) return result;
    result = message.Flatten(&file);
    if (result != B_OK) return result;
    result = file.Sync();
    if (result != B_OK) return result;
  }
  BEntry nextEntry(next.Path());
  result = nextEntry.Rename(leaf.c_str(), true);
  if (result != B_OK) nextEntry.Remove();
  return result;
}

status_t ConnectionProfileStore::Remove() {
  std::string path = Path();
  if (path.empty()) return B_ERROR;
  BEntry entry(path.c_str());
  status_t result = entry.InitCheck();
  if (result == B_ENTRY_NOT_FOUND) return B_OK;
  if (result != B_OK) return result;
  return entry.Remove();
}

std::string ConnectionSecretStore::Identifier(
    const ConnectionProfile& profile) {
  return "nats://" + profile.host + ":" + profile.port;
}

std::string ConnectionSecretStore::SecondaryIdentifier(
    const ConnectionProfile& profile) {
  return profile.user.empty() ? "token" : "user:" + profile.user;
}

status_t ConnectionSecretStore::Retrieve(const ConnectionProfile& profile,
                                         std::string& secret) {
  BKeyStore store;
  BPasswordKey key;
  status_t result = store.GetKey(
      kKeyringName, B_KEY_TYPE_PASSWORD, Identifier(profile).c_str(),
      SecondaryIdentifier(profile).c_str(), false, key);
  if (result == B_OK) secret = key.Password();
  return result;
}

status_t ConnectionSecretStore::Store(const ConnectionProfile& profile,
                                      const std::string& secret) {
  if (secret.empty()) return B_BAD_VALUE;
  BKeyStore store;
  status_t result = store.AddKeyring(kKeyringName);
  if (result != B_OK && result != B_NAME_IN_USE) return result;

  BPasswordKey existing;
  result = store.GetKey(kKeyringName, B_KEY_TYPE_PASSWORD,
                        Identifier(profile).c_str(),
                        SecondaryIdentifier(profile).c_str(), false, existing);
  if (result == B_OK) {
    result = store.RemoveKey(kKeyringName, existing);
    if (result != B_OK) return result;
  } else if (result != B_ENTRY_NOT_FOUND && result != B_BAD_VALUE) {
    return result;
  }

  BPasswordKey key(secret.c_str(), B_KEY_PURPOSE_NETWORK,
                   Identifier(profile).c_str(),
                   SecondaryIdentifier(profile).c_str());
  return store.AddKey(kKeyringName, key);
}

status_t ConnectionSecretStore::Remove(const ConnectionProfile& profile) {
  BKeyStore store;
  BPasswordKey key;
  status_t result = store.GetKey(
      kKeyringName, B_KEY_TYPE_PASSWORD, Identifier(profile).c_str(),
      SecondaryIdentifier(profile).c_str(), false, key);
  if (result == B_ENTRY_NOT_FOUND || result == B_BAD_VALUE) return B_OK;
  if (result != B_OK) return result;
  return store.RemoveKey(kKeyringName, key);
}
