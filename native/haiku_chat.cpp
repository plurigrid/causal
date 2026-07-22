// Causal Chat: a small native Haiku multichannel NATS client.
//
// Build on Haiku:
//   c++ -std=c++17 -O2 -Wall -Wextra haiku_chat.cpp -lbe -lnetwork -o CausalChat

#include <Application.h>
#include <Button.h>
#include <Directory.h>
#include <FindDirectory.h>
#include <Font.h>
#include <GroupView.h>
#include <GroupLayout.h>
#include <InterfaceDefs.h>
#include <LayoutBuilder.h>
#include <ListItem.h>
#include <ListView.h>
#include <Message.h>
#include <Messenger.h>
#include <OS.h>
#include <Path.h>
#include <ScrollView.h>
#include <StringItem.h>
#include <StringView.h>
#include <TextControl.h>
#include <TextView.h>
#include <Window.h>

#include <netdb.h>
#include <fcntl.h>
#include <sys/select.h>
#include <sys/socket.h>
#include <unistd.h>

#include <openssl/err.h>
#include <openssl/ssl.h>

#include <atomic>
#include <algorithm>
#include <cctype>
#include <chrono>
#include <condition_variable>
#include <csignal>
#include <deque>
#include <fstream>
#include <iostream>
#include <cerrno>
#include <cstdlib>
#include <cstring>
#include <map>
#include <mutex>
#include <set>
#include <sstream>
#include <string>
#include <thread>
#include <vector>

namespace {

constexpr uint32 kJoin = 'join';
constexpr uint32 kSend = 'send';
constexpr uint32 kSelectChannel = 'slct';
constexpr uint32 kIncoming = 'nmsg';
constexpr uint32 kStatus = 'nsta';

constexpr rgb_color kInk{24, 34, 52, 255};
constexpr rgb_color kNavy{25, 45, 78, 255};
constexpr rgb_color kCanvas{242, 239, 233, 255};
constexpr rgb_color kSurface{255, 254, 251, 255};
constexpr rgb_color kMuted{101, 111, 126, 255};
constexpr rgb_color kLine{221, 218, 211, 255};
constexpr rgb_color kBlue{48, 125, 208, 255};
constexpr rgb_color kTeal{35, 154, 139, 255};
constexpr rgb_color kAmber{218, 126, 55, 255};
constexpr rgb_color kCoral{219, 83, 82, 255};

enum class StatusTone : int32 {
  kConnecting,
  kOnline,
  kRetrying,
  kError,
};

rgb_color ToneColor(StatusTone tone) {
  switch (tone) {
    case StatusTone::kOnline: return kTeal;
    case StatusTone::kRetrying: return kAmber;
    case StatusTone::kError: return kCoral;
    case StatusTone::kConnecting: return kBlue;
  }
  return kBlue;
}

rgb_color Mix(rgb_color from, rgb_color to, float amount) {
  auto blend = [amount](uint8 a, uint8 b) {
    return static_cast<uint8>(a + (b - a) * amount);
  };
  return {blend(from.red, to.red), blend(from.green, to.green),
          blend(from.blue, to.blue), 255};
}

std::string FitText(BView* view, std::string text, float width) {
  if (view->StringWidth(text.c_str()) <= width) return text;
  const std::string suffix = "...";
  while (!text.empty() &&
         view->StringWidth((text + suffix).c_str()) > width)
    text.pop_back();
  return text + suffix;
}

std::vector<std::string> WrapText(BView* view, const std::string& source,
                                  float width, size_t max_lines) {
  std::string normalized = source;
  std::replace(normalized.begin(), normalized.end(), '\n', ' ');
  std::istringstream words(normalized);
  std::vector<std::string> lines;
  for (std::string word; words >> word;) {
    if (lines.empty()) lines.emplace_back();
    std::string candidate = lines.back().empty() ? word : lines.back() + " " + word;
    if (view->StringWidth(candidate.c_str()) <= width) {
      lines.back() = std::move(candidate);
      continue;
    }
    if (lines.back().empty()) lines.back() = FitText(view, word, width);
    if (lines.size() == max_lines) {
      lines.back() = FitText(view, lines.back() + " " + word, width);
      return lines;
    }
    lines.push_back(view->StringWidth(word.c_str()) <= width
                        ? word : FitText(view, word, width));
  }
  if (lines.empty()) lines.emplace_back();
  if (lines.size() > max_lines) lines.resize(max_lines);
  return lines;
}

std::string EnvOr(const char* name, const char* fallback) {
  const char* value = std::getenv(name);
  return value && *value ? value : fallback;
}

bool ValidChannel(const std::string& value) {
  if (value.empty() || value.size() > 48) return false;
  for (unsigned char c : value) {
    if (!(std::isalnum(c) || c == '-' || c == '_')) return false;
  }
  return true;
}

bool ValidNatsName(const std::string& value) {
  if (value.empty() || value.size() > 48) return false;
  for (unsigned char c : value) {
    if (!(std::isalnum(c) || c == '-' || c == '_')) return false;
  }
  return true;
}

std::string JsonEscape(const std::string& value) {
  std::string out;
  out.reserve(value.size() + 8);
  for (unsigned char c : value) {
    switch (c) {
      case '\\': out += "\\\\"; break;
      case '"': out += "\\\""; break;
      case '\n': out += "\\n"; break;
      case '\r': out += "\\r"; break;
      case '\t': out += "\\t"; break;
      default:
        if (c >= 0x20) out += static_cast<char>(c);
    }
  }
  return out;
}

std::string JsonField(const std::string& json, const char* name) {
  std::string key = std::string("\"") + name + "\":\"";
  size_t begin = json.find(key);
  if (begin == std::string::npos) return {};
  begin += key.size();
  std::string out;
  bool escaped = false;
  for (size_t i = begin; i < json.size(); ++i) {
    char c = json[i];
    if (escaped) {
      if (c == 'n') out += '\n';
      else if (c == 't') out += '\t';
      else if (c == 'r') out += '\r';
      else out += c;
      escaped = false;
    } else if (c == '\\') {
      escaped = true;
    } else if (c == '"') {
      break;
    } else {
      out += c;
    }
  }
  return out;
}

rgb_color ChannelColor(const std::string& channel) {
  if (channel == "lobby") return kBlue;
  if (channel == "meta") return {121, 91, 190, 255};
  if (channel == "games") return {218, 126, 55, 255};
  uint32 hash = 2166136261u;
  for (unsigned char c : channel) hash = (hash ^ c) * 16777619u;
  constexpr rgb_color palette[] = {
      {48, 125, 208, 255}, {121, 91, 190, 255}, {218, 126, 55, 255},
      {35, 154, 139, 255}, {207, 79, 116, 255}};
  return palette[hash % (sizeof(palette) / sizeof(palette[0]))];
}

struct ChatMessage {
  std::string id;
  std::string sender;
  std::string text;
};

class HistoryStore {
 public:
  HistoryStore() {
    enabled_ = EnvOr("CAUSAL_HISTORY", "1") != "0";
    if (!enabled_) return;
    const char* custom_path = std::getenv("CAUSAL_HISTORY_PATH");
    if (custom_path && *custom_path) {
      path_ = custom_path;
      return;
    }
    BPath settings;
    if (find_directory(B_USER_SETTINGS_DIRECTORY, &settings) != B_OK) {
      enabled_ = false;
      return;
    }
    settings.Append("CausalChat");
    if (create_directory(settings.Path(), 0755) != B_OK && errno != EEXIST) {
      enabled_ = false;
      return;
    }
    settings.Append("history.ndjson");
    path_ = settings.Path();
  }

  std::vector<std::pair<std::string, ChatMessage>> Load() const {
    std::vector<std::pair<std::string, ChatMessage>> result;
    if (!enabled_) return result;
    std::ifstream input(path_);
    for (std::string line; std::getline(input, line);) {
      std::string channel = JsonField(line, "channel");
      ChatMessage message{JsonField(line, "id"), JsonField(line, "sender"),
                          JsonField(line, "text")};
      if (!ValidChannel(channel) || message.text.empty()) continue;
      result.emplace_back(std::move(channel), std::move(message));
    }
    return result;
  }

  bool Append(const std::string& channel, const ChatMessage& message) const {
    if (!enabled_) return false;
    bool needs_newline = false;
    {
      std::ifstream existing(path_, std::ios::binary | std::ios::ate);
      if (existing && existing.tellg() > 0) {
        existing.seekg(-1, std::ios::end);
        char last = 0;
        existing.get(last);
        needs_newline = last != '\n';
      }
    }
    std::ofstream output(path_, std::ios::app);
    if (!output) return false;
    if (needs_newline) output << '\n';
    output << "{\"channel\":\"" << JsonEscape(channel)
           << "\",\"id\":\"" << JsonEscape(message.id)
           << "\",\"sender\":\"" << JsonEscape(message.sender)
           << "\",\"text\":\"" << JsonEscape(message.text) << "\"}\n";
    output.flush();
    return output.tellp() > static_cast<std::streamoff>(8 * 1024 * 1024);
  }

  void Compact(const std::map<std::string, std::vector<ChatMessage>>& rooms) const {
    if (!enabled_) return;
    std::string next = path_ + ".next";
    std::ofstream output(next, std::ios::trunc);
    if (!output) return;
    for (const auto& [channel, messages] : rooms) {
      for (const auto& message : messages) {
        output << "{\"channel\":\"" << JsonEscape(channel)
               << "\",\"id\":\"" << JsonEscape(message.id)
               << "\",\"sender\":\"" << JsonEscape(message.sender)
               << "\",\"text\":\"" << JsonEscape(message.text) << "\"}\n";
      }
    }
    output.close();
    if (output) rename(next.c_str(), path_.c_str());
  }

  bool enabled() const { return enabled_; }

 private:
  bool enabled_{false};
  std::string path_;
};

class HeaderView : public BView {
 public:
  HeaderView(std::string endpoint, bool tls)
      : BView("header", B_WILL_DRAW | B_FULL_UPDATE_ON_RESIZE),
        endpoint_(std::move(endpoint)), tls_(tls) {
    SetExplicitMinSize(BSize(B_SIZE_UNSET, 104));
    SetExplicitMaxSize(BSize(B_SIZE_UNLIMITED, 104));
    SetViewColor(kNavy);
    SetLowColor(kNavy);
  }

  void SetRoom(const std::string& room) {
    room_ = room;
    Invalidate();
  }

  void Draw(BRect /*update*/) override {
    BRect bounds = Bounds();
    rgb_color accent = ChannelColor(room_.empty() ? "lobby" : room_);
    SetHighColor(kNavy);
    FillRect(bounds);

    SetHighColor(Mix(kNavy, accent, 0.25f));
    FillRect(BRect(bounds.left, bounds.bottom - 28, bounds.right,
                   bounds.bottom));
    SetHighColor(Mix(kNavy, accent, 0.68f));
    StrokeEllipse(BPoint(bounds.right - 60, bounds.top + 46), 58, 58);
    StrokeEllipse(BPoint(bounds.right - 60, bounds.top + 46), 39, 39);
    StrokeEllipse(BPoint(bounds.right - 60, bounds.top + 46), 20, 20);
    FillEllipse(BPoint(bounds.right - 99, bounds.top + 46), 3, 3);
    SetHighColor(Mix(kNavy, accent, 0.75f));
    FillRect(BRect(bounds.left, bounds.bottom - 4, bounds.right,
                   bounds.bottom));

    SetHighColor(accent);
    FillEllipse(BPoint(30, 34), 9, 9);
    SetHighColor(Mix(kNavy, kSurface, 0.75f));
    StrokeEllipse(BPoint(30, 34), 17, 17);
    SetHighColor(Mix(kNavy, accent, 0.55f));
    StrokeEllipse(BPoint(30, 34), 13, 13);

    SetHighColor(kSurface);
    BFont title(*be_bold_font);
    title.SetSize(20);
    SetFont(&title);
    DrawString("CAUSAL", BPoint(59, 38));
    float mark = 59 + StringWidth("CAUSAL") + 8;
    SetHighColor(accent);
    DrawString("/", BPoint(mark, 38));
    SetHighColor(kSurface);
    DrawString("CHAT", BPoint(mark + StringWidth("/") + 8, 38));
    BFont subtitle_font(*be_plain_font);
    subtitle_font.SetSize(11);
    SetFont(&subtitle_font);
    SetHighColor(Mix(kNavy, kSurface, 0.72f));
    std::string subtitle = endpoint_ + "   /   native Haiku";
    DrawString(subtitle.c_str(), BPoint(59, 61));

    const char* badge = tls_ ? "VERIFIED TLS" : "OPEN / PLAINTEXT";
    float badge_width = StringWidth(badge) + 24;
    BRect badge_frame(bounds.right - badge_width - 24, 19,
                      bounds.right - 24, 47);
    rgb_color transport = tls_ ? kTeal : kAmber;
    SetHighColor(Mix(kNavy, transport, 0.58f));
    FillRoundRect(badge_frame, 13, 13);
    SetHighColor(kSurface);
    DrawString(badge, BPoint(badge_frame.left + 12, badge_frame.top + 18));

    std::string room = "# " + (room_.empty() ? std::string("lobby") : room_);
    room = FitText(this, room, 184);
    SetHighColor(Mix(kNavy, kSurface, 0.84f));
    DrawString(room.c_str(), BPoint(bounds.right - 208, 88));
    SetHighColor(accent);
    FillEllipse(BPoint(bounds.right - 219, 84), 3, 3);
  }

 private:
  std::string endpoint_;
  std::string room_;
  bool tls_;
};

class AccentButton : public BButton {
 public:
  AccentButton(const char* name, const char* label, BMessage* message,
               bool primary)
      : BButton(name, label, message), primary_(primary) {
    SetViewColor(B_TRANSPARENT_COLOR);
    SetLowColor(B_TRANSPARENT_COLOR);
    SetExplicitMinSize(BSize(primary ? 92 : 118, 34));
  }

  void Draw(BRect /*update*/) override {
    BRect bounds = Bounds();
    SetHighColor(kCanvas);
    FillRect(bounds);
    rgb_color fill = primary_ ? kBlue
                              : rgb_color{230, 226, 218, 255};
    if (!IsEnabled()) fill = Mix(fill, kCanvas, 0.65f);
    if (Value() == B_CONTROL_ON) fill = Mix(fill, kInk, 0.16f);
    BRect frame = bounds.InsetByCopy(1, 1);
    SetHighColor(fill);
    FillRoundRect(frame, 9, 9);
    if (IsFocus()) {
      SetHighColor(primary_ ? kSurface : ChannelColor("lobby"));
      StrokeRoundRect(frame.InsetByCopy(2, 2), 6, 6);
    }
    BFont font(*be_bold_font);
    font.SetSize(11);
    SetFont(&font);
    SetHighColor(primary_ ? kSurface : kInk);
    const char* label = Label();
    font_height metrics;
    font.GetHeight(&metrics);
    float x = frame.left + (frame.Width() - StringWidth(label)) / 2;
    float y = frame.top + (frame.Height() + metrics.ascent - metrics.descent) / 2;
    DrawString(label, BPoint(x, y));
  }

 private:
  bool primary_;
};

class RoomTitleView : public BView {
 public:
  RoomTitleView()
      : BView("room-title", B_WILL_DRAW | B_FULL_UPDATE_ON_RESIZE) {
    SetViewColor(kSurface);
    SetExplicitMinSize(BSize(B_SIZE_UNSET, 50));
    SetExplicitMaxSize(BSize(B_SIZE_UNLIMITED, 50));
  }

  void SetRoom(const std::string& room, size_t messages) {
    room_ = room;
    messages_ = messages;
    Invalidate();
  }

  void Draw(BRect /*update*/) override {
    BRect bounds = Bounds();
    SetHighColor(kSurface);
    FillRect(bounds);
    rgb_color accent = ChannelColor(room_.empty() ? "lobby" : room_);
    SetHighColor(accent);
    FillRoundRect(BRect(0, 0, 4, bounds.bottom - 1), 2, 2);
    FillEllipse(BPoint(20, bounds.Height() / 2), 4, 4);
    BFont title(*be_bold_font);
    title.SetSize(14);
    SetFont(&title);
    SetHighColor(kInk);
    std::string label = "# " + room_;
    DrawString(label.c_str(), BPoint(34, 31));
    BFont detail(*be_plain_font);
    detail.SetSize(10);
    SetFont(&detail);
    SetHighColor(kMuted);
    std::string count = std::to_string(messages_) +
                        (messages_ == 1 ? " SIGNAL" : " SIGNALS");
    DrawString(count.c_str(), BPoint(bounds.right - StringWidth(count.c_str()) - 16, 30));
    SetHighColor(kLine);
    StrokeLine(BPoint(0, bounds.bottom), BPoint(bounds.right, bounds.bottom));
  }

 private:
  std::string room_{"lobby"};
  size_t messages_{0};
};

class StatusBarView : public BView {
 public:
  StatusBarView(bool tls, bool history, bool durable)
      : BView("status", B_WILL_DRAW | B_FULL_UPDATE_ON_RESIZE), tls_(tls),
        history_(history), durable_(durable) {
    SetViewColor(kCanvas);
    SetExplicitMinSize(BSize(B_SIZE_UNSET, 40));
    SetExplicitMaxSize(BSize(B_SIZE_UNLIMITED, 40));
  }

  void SetStatus(std::string text, StatusTone tone) {
    text_ = std::move(text);
    tone_ = tone;
    Invalidate();
  }

  void Draw(BRect /*update*/) override {
    BRect bounds = Bounds();
    SetHighColor(kCanvas);
    FillRect(bounds);
    rgb_color tone = ToneColor(tone_);
    SetHighColor(Mix(kCanvas, tone, 0.17f));
    FillEllipse(BPoint(10, 20), 9, 9);
    SetHighColor(tone);
    FillEllipse(BPoint(10, 20), 4, 4);
    BFont font(*be_plain_font);
    font.SetSize(10);
    SetFont(&font);
    SetHighColor(Mix(kInk, tone, 0.30f));

    float right = bounds.right;
    DrawPill(tls_ ? "TLS" : "OPEN", tls_ ? kTeal : kAmber, right);
    if (durable_) DrawPill("REPLAY", kBlue, right);
    DrawPill(history_ ? "HISTORY" : "EPHEMERAL", kMuted, right);
    std::string status = FitText(this, text_, right - 40);
    DrawString(status.c_str(), BPoint(25, 24));
  }

 private:
  void DrawPill(const char* label, rgb_color color, float& right) {
    float width = StringWidth(label) + 18;
    BRect pill(right - width, 9, right, 31);
    SetHighColor(Mix(kCanvas, color, 0.16f));
    FillRoundRect(pill, 10, 10);
    SetHighColor(Mix(kInk, color, 0.34f));
    DrawString(label, BPoint(pill.left + 9, 24));
    right = pill.left - 6;
  }

  bool tls_;
  bool history_;
  bool durable_;
  StatusTone tone_{StatusTone::kConnecting};
  std::string text_{"Connecting..."};
};

class RoomRailHeaderView : public BView {
 public:
  RoomRailHeaderView()
      : BView("room-rail-header", B_WILL_DRAW | B_FULL_UPDATE_ON_RESIZE) {
    SetViewColor(kSurface);
    SetExplicitMinSize(BSize(B_SIZE_UNSET, 50));
    SetExplicitMaxSize(BSize(B_SIZE_UNLIMITED, 50));
  }

  void SetCount(int32 count) {
    count_ = count;
    Invalidate();
  }

  void Draw(BRect /*update*/) override {
    BRect bounds = Bounds();
    SetHighColor(kSurface);
    FillRect(bounds);
    BFont label(*be_bold_font);
    label.SetSize(10);
    SetFont(&label);
    SetHighColor(kMuted);
    DrawString("ROOMS", BPoint(4, 30));
    std::string count = std::to_string(count_) + " JOINED";
    SetHighColor(Mix(kMuted, kBlue, 0.35f));
    DrawString(count.c_str(),
               BPoint(bounds.right - StringWidth(count.c_str()) - 5, 30));
    SetHighColor(kLine);
    StrokeLine(BPoint(0, bounds.bottom), BPoint(bounds.right, bounds.bottom));
  }

 private:
  int32 count_{0};
};

class ChannelItem : public BStringItem {
 public:
  explicit ChannelItem(const char* label) : BStringItem(label) { SetHeight(46); }

  void DrawItem(BView* owner, BRect frame, bool complete) override {
    rgb_color color = ChannelColor(Text());
    if (IsSelected()) {
      owner->SetHighColor(Mix(kSurface, color, 0.15f));
      owner->FillRoundRect(frame.InsetByCopy(4, 3), 9, 9);
      owner->SetHighColor(color);
      owner->FillRoundRect(BRect(frame.left + 4, frame.top + 9,
                                 frame.left + 8, frame.bottom - 9), 2, 2);
    } else if (complete) {
      owner->SetHighColor(kSurface);
      owner->FillRect(frame);
    }
    owner->SetHighColor(color);
    owner->FillEllipse(BPoint(frame.left + 21, frame.top + frame.Height() / 2), 4, 4);
    owner->SetHighColor(kInk);
    BFont font(*be_plain_font);
    if (IsSelected()) font.SetFace(B_BOLD_FACE);
    font.SetSize(11);
    owner->SetFont(&font);
    std::string label = std::string("# ") + Text();
    owner->DrawString(label.c_str(),
                      BPoint(frame.left + 35, frame.top + BaselineOffset()));
  }
};

class MessageItem : public BListItem {
 public:
  MessageItem(ChatMessage message, rgb_color accent, bool mine)
      : message_(std::move(message)), accent_(accent), mine_(mine) {
    size_t lines = std::max<size_t>(
        1, std::min<size_t>(5, (message_.text.size() + 63) / 64));
    height_ = std::max(72.0f, 52.0f + static_cast<float>(lines) * 16);
    SetHeight(height_);
  }

  void Update(BView* owner, const BFont* font) override {
    BListItem::Update(owner, font);
    SetHeight(height_);
  }

  void DrawItem(BView* owner, BRect frame, bool /*complete*/) override {
    owner->SetHighColor(kCanvas);
    owner->FillRect(frame);

    BRect bubble = frame.InsetByCopy(10, 5);
    if (mine_) bubble.left += frame.Width() * 0.18f;
    else bubble.right -= frame.Width() * 0.10f;
    owner->SetHighColor(mine_ ? Mix(kSurface, accent_, 0.14f) : kSurface);
    owner->FillRoundRect(bubble, 11, 11);
    owner->SetHighColor(IsSelected() ? accent_ : kLine);
    owner->StrokeRoundRect(bubble, 11, 11);
    if (!mine_) {
      owner->SetHighColor(accent_);
      owner->FillRoundRect(BRect(bubble.left, bubble.top + 9,
                                 bubble.left + 3, bubble.bottom - 8), 2, 2);
    }

    BFont sender(*be_bold_font);
    sender.SetSize(10);
    owner->SetFont(&sender);
    owner->SetHighColor(accent_);
    std::string sender_label = message_.sender + (mine_ ? "   /   YOU" : "");
    sender_label = FitText(owner, sender_label, bubble.Width() - 26);
    owner->DrawString(sender_label.c_str(),
                      BPoint(bubble.left + 13, bubble.top + 17));
    BFont body(*be_plain_font);
    body.SetSize(11);
    owner->SetFont(&body);
    owner->SetHighColor(kInk);
    auto lines = WrapText(owner, message_.text, bubble.Width() - 26, 5);
    for (size_t i = 0; i < lines.size(); ++i)
      owner->DrawString(lines[i].c_str(),
                        BPoint(bubble.left + 13, bubble.top + 38 + i * 15));
  }

 private:
  ChatMessage message_;
  rgb_color accent_;
  bool mine_;
  float height_{72};
};

class TranscriptListView : public BListView {
 public:
  TranscriptListView() : BListView("transcript", B_SINGLE_SELECTION_LIST) {
    SetFlags(Flags() | B_FULL_UPDATE_ON_RESIZE);
  }

  void Draw(BRect update) override {
    BListView::Draw(update);
    if (CountItems() != 0) return;
    BRect bounds = Bounds();
    rgb_color accent = ChannelColor(room_);
    BPoint center(bounds.left + bounds.Width() / 2,
                  bounds.top + bounds.Height() / 2 - 20);
    SetHighColor(Mix(kCanvas, accent, 0.20f));
    FillEllipse(center, 30, 30);
    SetHighColor(accent);
    StrokeEllipse(center, 20, 20);
    StrokeEllipse(center, 11, 11);
    FillEllipse(center, 4, 4);
    BFont title(*be_bold_font);
    title.SetSize(13);
    SetFont(&title);
    SetHighColor(kInk);
    std::string headline = "#" + room_ + " is quiet";
    DrawString(headline.c_str(),
               BPoint(center.x - StringWidth(headline.c_str()) / 2,
                      center.y + 51));
    BFont detail(*be_plain_font);
    detail.SetSize(10);
    SetFont(&detail);
    SetHighColor(kMuted);
    const char* prompt = "Send a signal. The room appears on first use.";
    DrawString(prompt,
               BPoint(center.x - StringWidth(prompt) / 2, center.y + 70));
  }

  void SetRoom(const std::string& room) {
    room_ = room;
    Invalidate();
  }

 private:
  std::string room_{"lobby"};
};

int ConnectTcp(const std::string& host, const std::string& port,
               const std::atomic<bool>* running) {
  addrinfo hints{};
  hints.ai_socktype = SOCK_STREAM;
  hints.ai_family = AF_UNSPEC;
  addrinfo* result = nullptr;
  if (getaddrinfo(host.c_str(), port.c_str(), &hints, &result) != 0) return -1;
  int fd = -1;
  for (addrinfo* p = result; p; p = p->ai_next) {
    fd = socket(p->ai_family, p->ai_socktype, p->ai_protocol);
    if (fd < 0) continue;
    int flags = fcntl(fd, F_GETFL, 0);
    fcntl(fd, F_SETFL, flags | O_NONBLOCK);
    int outcome = connect(fd, p->ai_addr, p->ai_addrlen);
    if (outcome < 0 && errno == EINPROGRESS) {
      for (int attempt = 0; attempt < 30 && (!running || *running); ++attempt) {
        fd_set writes;
        FD_ZERO(&writes);
        FD_SET(fd, &writes);
        timeval slice{0, 100000};
        int ready = select(fd + 1, nullptr, &writes, nullptr, &slice);
        if (ready > 0) {
          int error = 0;
          socklen_t size = sizeof(error);
          getsockopt(fd, SOL_SOCKET, SO_ERROR, &error, &size);
          outcome = error == 0 ? 0 : -1;
          break;
        }
        if (ready < 0) break;
      }
    }
    fcntl(fd, F_SETFL, flags);
    if (outcome == 0) {
      timeval deadline{3, 0};
      setsockopt(fd, SOL_SOCKET, SO_RCVTIMEO, &deadline, sizeof(deadline));
      setsockopt(fd, SOL_SOCKET, SO_SNDTIMEO, &deadline, sizeof(deadline));
      break;
    }
    if (fd >= 0) close(fd);
    fd = -1;
  }
  freeaddrinfo(result);
  return fd;
}

class NatsClient {
 public:
  explicit NatsClient(BMessenger target)
      : target_(target), host_(EnvOr("NATS_HOST", "nonlocal.info")),
        port_(EnvOr("NATS_PORT", "4222")),
        tls_(EnvOr("NATS_TLS", "0") == "1"),
        tls_name_(EnvOr("NATS_TLS_NAME", host_.c_str())),
        ca_file_(EnvOr("NATS_CA_FILE", "")),
        token_(EnvOr("NATS_TOKEN", "")), user_(EnvOr("NATS_USER", "")),
        password_(EnvOr("NATS_PASSWORD", "")),
        jetstream_(EnvOr("NATS_JETSTREAM", "0") == "1"),
        stream_(EnvOr("NATS_STREAM", "CAUSAL")),
        consumer_(EnvOr("NATS_CONSUMER", "")) {
    replay_inbox_ = "_INBOX.CAUSAL." +
                    (consumer_.empty() ? std::string("anonymous") : consumer_) +
                    "." + std::to_string(real_time_clock_usecs());
  }

  ~NatsClient() { Stop(); }

  void Start(const std::vector<std::string>& initial_subjects) {
    {
      std::lock_guard<std::mutex> lock(subjects_mutex_);
      requested_subjects_.insert(initial_subjects.begin(), initial_subjects.end());
    }
    running_ = true;
    reader_ = std::thread([this] { ReaderLoop(); });
  }

  void Stop() {
    running_ = false;
    retry_cv_.notify_all();
    int fd = fd_.exchange(-1);
    if (fd >= 0) {
      shutdown(fd, SHUT_RDWR);
      close(fd);
    }
    if (reader_.joinable()) reader_.join();
  }

  bool Subscribe(const std::string& subject) {
    {
      std::lock_guard<std::mutex> lock(subjects_mutex_);
      if (!requested_subjects_.insert(subject).second) return true;
    }
    if (jetstream_) return true;
    if (!connected_) return true;
    return QueueFrame("SUB " + subject + " " + std::to_string(next_sid_++) + "\r\n",
                      true);
  }

  bool SendSubscriptionDirect(const std::string& subject) {
    const uint64 sid = next_sid_++;
    return WireSendAll("SUB " + subject + " " + std::to_string(sid) + "\r\n");
  }

  bool Publish(const std::string& subject, const std::string& payload) {
    if (!connected_) return false;
    return QueueFrame("PUB " + subject + " " + std::to_string(payload.size()) +
                      "\r\n" + payload + "\r\n", false);
  }

  void Acknowledge(const std::string& reply) {
    if (reply.empty() || !connected_) return;
    QueueFrame("PUB " + reply + " 0\r\n\r\n", true);
  }

 private:
  struct Outbound {
    std::string bytes;
    bool control;
  };

  void ReportStatus(const std::string& text,
                    StatusTone tone = StatusTone::kConnecting) {
    if (tone == StatusTone::kError) std::cerr << text << std::endl;
    BMessage message(kStatus);
    message.AddString("text", text.c_str());
    message.AddInt32("tone", static_cast<int32>(tone));
    target_.SendMessage(&message);
  }

  bool QueueFrame(std::string bytes, bool control) {
    std::lock_guard<std::mutex> lock(outbound_mutex_);
    if (!control && outbound_.size() >= 256) return false;
    if (control) outbound_.push_front({std::move(bytes), true});
    else outbound_.push_back({std::move(bytes), false});
    return true;
  }

  bool WireSendAll(const std::string& bytes) {
    int fd = fd_;
    if (fd < 0) return false;
    size_t offset = 0;
    while (offset < bytes.size()) {
      ssize_t count = tls_
          ? SSL_write(ssl_, bytes.data() + offset,
                      static_cast<int>(bytes.size() - offset))
          : send(fd, bytes.data() + offset, bytes.size() - offset, 0);
      if (count <= 0) return false;
      offset += static_cast<size_t>(count);
    }
    return true;
  }

  ssize_t WireReceive(char* bytes, size_t capacity) {
    if (tls_) {
      int count = SSL_read(ssl_, bytes, static_cast<int>(capacity));
      if (count > 0) return count;
      int error = SSL_get_error(ssl_, count);
      if (error == SSL_ERROR_WANT_READ || error == SSL_ERROR_WANT_WRITE) return -2;
      return 0;
    }
    return recv(fd_, bytes, capacity, 0);
  }

  bool DrainOutbound() {
    while (true) {
      std::lock_guard<std::mutex> lock(outbound_mutex_);
      if (outbound_.empty()) return true;
      if (!WireSendAll(outbound_.front().bytes)) return false;
      outbound_.pop_front();
    }
  }

  void PurgeControlFrames() {
    std::lock_guard<std::mutex> lock(outbound_mutex_);
    std::deque<Outbound> publishes;
    for (auto& frame : outbound_)
      if (!frame.control) publishes.push_back(std::move(frame));
    outbound_.swap(publishes);
  }

  void Deliver(const std::string& subject, const std::string& payload,
               const std::string& reply) {
    BMessage message(kIncoming);
    message.AddString("subject", subject.c_str());
    message.AddString("payload", payload.c_str());
    if (!reply.empty()) message.AddString("reply", reply.c_str());
    target_.SendMessage(&message);
  }

  std::string PullFrame() const {
    const std::string request = "{\"batch\":500}";
    const std::string subject = "$JS.API.CONSUMER.MSG.NEXT." + stream_ + "." +
                                consumer_;
    return "PUB " + subject + " " + replay_inbox_ + " " +
           std::to_string(request.size()) + "\r\n" + request + "\r\n";
  }

  void ParseFrames(std::string& input) {
    while (!input.empty()) {
      if (input.rfind("PING\r\n", 0) == 0) {
        input.erase(0, 6);
        QueueFrame("PONG\r\n", true);
        continue;
      }
      size_t line_end = input.find("\r\n");
      if (line_end == std::string::npos) return;
      std::string line = input.substr(0, line_end);
      if (line.rfind("MSG ", 0) == 0) {
        std::istringstream words(line);
        std::vector<std::string> token;
        for (std::string word; words >> word;) token.push_back(word);
        if (token.size() < 4) {
          ReportStatus("Malformed NATS MSG header", StatusTone::kError);
          input.erase(0, line_end + 2);
          continue;
        }
        size_t bytes = 0;
        try {
          bytes = static_cast<size_t>(std::stoull(token.back()));
        } catch (...) {
          ReportStatus("Invalid NATS payload length", StatusTone::kError);
          input.erase(0, line_end + 2);
          continue;
        }
        const size_t payload_start = line_end + 2;
        if (input.size() < payload_start + bytes + 2) return;
        std::string reply = token.size() >= 5 ? token[token.size() - 2] : "";
        Deliver(token[1], input.substr(payload_start, bytes), reply);
        if (jetstream_ && !reply.empty() && --pull_remaining_ == 0) {
          pull_remaining_ = 500;
          QueueFrame(PullFrame(), true);
        }
        input.erase(0, payload_start + bytes + 2);
        continue;
      }
      if (line.rfind("-ERR", 0) == 0)
        ReportStatus(line, StatusTone::kError);
      input.erase(0, line_end + 2);
    }
  }

  void ReaderLoop() {
    if (!CredentialsAreSafe()) {
      running_ = false;
      return;
    }
    unsigned retry_seconds = 1;
    while (running_) {
      int fd = ConnectTcp(host_, port_, &running_);
      if (fd < 0) {
        ReportStatus("Offline; retrying " + host_ + ":" + port_ + " in " +
                         std::to_string(retry_seconds) + "s",
                     StatusTone::kRetrying);
        WaitBeforeRetry(retry_seconds);
        retry_seconds = std::min(retry_seconds * 2, 8u);
        continue;
      }
      fd_ = fd;
      if (tls_ && !StartTls(fd)) {
        CloseCurrent(fd);
        WaitBeforeRetry(retry_seconds);
        retry_seconds = std::min(retry_seconds * 2, 8u);
        continue;
      }
      if (!WireSendAll(ConnectFrame())) {
        CloseCurrent(fd);
        WaitBeforeRetry(retry_seconds);
        retry_seconds = std::min(retry_seconds * 2, 8u);
        continue;
      }
      {
        std::lock_guard<std::mutex> lock(subjects_mutex_);
        bool subscriptions_ok = true;
        if (jetstream_) {
          pull_remaining_ = 500;
          subscriptions_ok = SendSubscriptionDirect(replay_inbox_) &&
                             WireSendAll(PullFrame());
        } else {
          for (const auto& subject : requested_subjects_)
            subscriptions_ok = SendSubscriptionDirect(subject) && subscriptions_ok;
        }
        if (!subscriptions_ok) {
          CloseCurrent(fd);
          continue;
        }
        connected_ = true;
      }
      retry_seconds = 1;
      ReportStatus(std::string(tls_ ? "TLS connected to " : "Connected to ") +
                   host_ + ":" + port_ +
                   (jetstream_ ? " / durable replay" : ""),
                   StatusTone::kOnline);
      char chunk[8192];
      std::string input;
      while (running_) {
        if (!DrainOutbound()) break;
        fd_set reads;
        FD_ZERO(&reads);
        FD_SET(fd, &reads);
        timeval timeout{0, 100000};
        int ready = (tls_ && SSL_pending(ssl_) > 0)
            ? 1 : select(fd + 1, &reads, nullptr, nullptr, &timeout);
        if (ready < 0) break;
        if (ready > 0) {
          ssize_t count = WireReceive(chunk, sizeof(chunk));
          if (count == -2) continue;
          if (count <= 0) break;
          input.append(chunk, static_cast<size_t>(count));
          ParseFrames(input);
        }
      }
      connected_ = false;
      PurgeControlFrames();
      CloseCurrent(fd);
      if (running_) {
        ReportStatus("Connection lost; retrying in 1s", StatusTone::kRetrying);
        WaitBeforeRetry(1);
      }
    }
  }

  bool CredentialsAreSafe() {
    const bool has_token = !token_.empty();
    const bool has_user = !user_.empty() || !password_.empty();
    if (has_token && has_user) {
      ReportStatus("Choose either NATS_TOKEN or NATS_USER/NATS_PASSWORD",
                   StatusTone::kError);
      return false;
    }
    if (has_user && (user_.empty() || password_.empty())) {
      ReportStatus("NATS_USER and NATS_PASSWORD must be supplied together",
                   StatusTone::kError);
      return false;
    }
    if ((has_token || has_user) && !tls_) {
      ReportStatus("Refusing to send NATS credentials over plaintext",
                   StatusTone::kError);
      return false;
    }
    if (jetstream_ && (!ValidNatsName(stream_) || !ValidNatsName(consumer_))) {
      ReportStatus("NATS_STREAM and NATS_CONSUMER must be simple NATS names",
                   StatusTone::kError);
      return false;
    }
    if (jetstream_ && !tls_) {
      ReportStatus("Durable replay requires verified TLS", StatusTone::kError);
      return false;
    }
    return true;
  }

  std::string ConnectFrame() const {
    std::string json = "CONNECT {\"lang\":\"haiku-libroot\",\"version\":\"0.5\","
                       "\"verbose\":false,\"pedantic\":true";
    if (!token_.empty()) json += ",\"auth_token\":\"" + JsonEscape(token_) + "\"";
    if (!user_.empty()) {
      json += ",\"user\":\"" + JsonEscape(user_) + "\",\"pass\":\"" +
              JsonEscape(password_) + "\"";
    }
    return json + "}\r\n";
  }

  std::string LastTlsError() const {
    unsigned long code = ERR_get_error();
    if (!code) return "TLS handshake failed";
    char text[256];
    ERR_error_string_n(code, text, sizeof(text));
    return text;
  }

  bool StartTls(int fd) {
    ssl_ctx_ = SSL_CTX_new(TLS_client_method());
    if (!ssl_ctx_) {
      ReportStatus(LastTlsError(), StatusTone::kError);
      return false;
    }
    SSL_CTX_set_min_proto_version(ssl_ctx_, TLS1_2_VERSION);
    SSL_CTX_set_verify(ssl_ctx_, SSL_VERIFY_PEER, nullptr);
    int trust_ok = ca_file_.empty()
        ? SSL_CTX_set_default_verify_paths(ssl_ctx_)
        : SSL_CTX_load_verify_locations(ssl_ctx_, ca_file_.c_str(), nullptr);
    if (trust_ok != 1) {
      ReportStatus("Cannot load TLS trust roots: " + LastTlsError(),
                   StatusTone::kError);
      return false;
    }
    ssl_ = SSL_new(ssl_ctx_);
    if (!ssl_ || SSL_set_fd(ssl_, fd) != 1 ||
        SSL_set_tlsext_host_name(ssl_, tls_name_.c_str()) != 1 ||
        SSL_set1_host(ssl_, tls_name_.c_str()) != 1 || SSL_connect(ssl_) != 1) {
      ReportStatus("TLS verification failed for " + tls_name_ + ": " +
                       LastTlsError(),
                   StatusTone::kError);
      return false;
    }
    if (SSL_get_verify_result(ssl_) != X509_V_OK) {
      ReportStatus("TLS certificate chain rejected for " + tls_name_,
                   StatusTone::kError);
      return false;
    }
    return true;
  }

  void ResetTls() {
    if (ssl_) {
      if (fd_ >= 0) SSL_shutdown(ssl_);
      SSL_free(ssl_);
      ssl_ = nullptr;
    }
    if (ssl_ctx_) {
      SSL_CTX_free(ssl_ctx_);
      ssl_ctx_ = nullptr;
    }
  }

  void CloseCurrent(int expected_fd) {
    ResetTls();
    int current = fd_.exchange(-1);
    if (current >= 0) close(current);
    else if (expected_fd >= 0 && running_) close(expected_fd);
  }

  void WaitBeforeRetry(unsigned seconds) {
    std::unique_lock<std::mutex> lock(retry_mutex_);
    retry_cv_.wait_for(lock, std::chrono::seconds(seconds),
                       [this] { return !running_; });
  }

  BMessenger target_;
  std::string host_;
  std::string port_;
  bool tls_;
  std::string tls_name_;
  std::string ca_file_;
  std::string token_;
  std::string user_;
  std::string password_;
  bool jetstream_;
  std::string stream_;
  std::string consumer_;
  std::string replay_inbox_;
  std::set<std::string> requested_subjects_;
  std::atomic<bool> running_{false};
  std::atomic<bool> connected_{false};
  std::atomic<int> fd_{-1};
  std::atomic<uint64> next_sid_{1};
  std::atomic<unsigned> pull_remaining_{500};
  std::deque<Outbound> outbound_;
  std::mutex outbound_mutex_;
  std::mutex subjects_mutex_;
  std::mutex retry_mutex_;
  std::condition_variable retry_cv_;
  std::thread reader_;
  SSL_CTX* ssl_ctx_{nullptr};
  SSL* ssl_{nullptr};
};

class ChatWindow : public BWindow {
 public:
  ChatWindow()
      : BWindow(BRect(70, 70, 1170, 830), "Causal Chat - nonlocal.info",
                B_TITLED_WINDOW, B_QUIT_ON_WINDOW_CLOSE),
        host_(EnvOr("NATS_HOST", "nonlocal.info")),
        port_(EnvOr("NATS_PORT", "4222")),
        tls_(EnvOr("NATS_TLS", "0") == "1"),
        durable_(EnvOr("NATS_JETSTREAM", "0") == "1"),
        petname_(EnvOr("CAUSAL_PETNAME", "haiku")) {
    SetTitle(("Causal Chat - " + host_).c_str());
    SetSizeLimits(820, 1800, 560, 1400);
    header_ = new HeaderView(host_ + ":" + port_, tls_);
    petname_input_ = new BTextControl("petname", "IDENTITY", petname_.c_str(), nullptr);
    petname_input_->SetExplicitMaxSize(BSize(270, B_SIZE_UNLIMITED));
    channel_input_ = new BTextControl("channel", "ROOM", "lobby", nullptr);
    auto* join = new AccentButton("join", "JOIN / CREATE", new BMessage(kJoin), false);
    channels_ = new BListView("channels", B_SINGLE_SELECTION_LIST);
    channels_->SetExplicitMinSize(BSize(190, B_SIZE_UNSET));
    channels_->SetSelectionMessage(new BMessage(kSelectChannel));
    channels_->SetViewColor(kSurface);
    transcript_ = new TranscriptListView();
    transcript_->SetViewColor(kCanvas);
    input_ = new BTextControl("message", "TO #lobby", "", new BMessage(kSend));
    auto* send = new AccentButton("send", "SEND SIGNAL", new BMessage(kSend), true);
    room_title_ = new RoomTitleView();
    room_rail_header_ = new RoomRailHeaderView();
    status_ = new StatusBarView(tls_, history_.enabled(), durable_);

    StyleField(petname_input_, 62);
    StyleField(channel_input_, 46);
    StyleField(input_, 64);

    auto* backdrop = new BView("backdrop", B_WILL_DRAW);
    backdrop->SetViewColor(kCanvas);
    BLayoutBuilder::Group<>(this, B_VERTICAL, 0)
        .SetInsets(0)
        .Add(backdrop);
    BLayoutBuilder::Group<>(backdrop, B_VERTICAL, 12)
        .SetInsets(0)
        .Add(header_)
        .AddGroup(B_HORIZONTAL, 10)
          .SetInsets(20, 2, 20, 0)
          .Add(petname_input_)
          .Add(channel_input_, 1)
          .Add(join)
        .End()
        .AddGroup(B_HORIZONTAL, 12, 1)
          .SetInsets(20, 0, 20, 0)
          .AddGroup(B_VERTICAL, 0, 0.22)
            .Add(room_rail_header_)
            .Add(new BScrollView("channel-scroll", channels_, 0, false, true,
                                 B_NO_BORDER), 1)
          .End()
          .AddGroup(B_VERTICAL, 0, 0.77)
            .Add(room_title_)
            .Add(new BScrollView("transcript-scroll", transcript_, 0, false, true,
                                 B_NO_BORDER), 1)
          .End()
        .End()
        .AddGroup(B_HORIZONTAL, 10)
          .SetInsets(20, 0, 20, 0)
          .Add(input_, 1)
          .Add(send)
        .End()
        .AddGroup(B_HORIZONTAL, 0)
          .SetInsets(20, 0, 20, 8)
          .Add(status_)
        .End();

    Join("lobby", false);
    Join("meta", false);
    Join("games", false);
    RestoreHistory();
    channels_->Select(0);
    current_ = "lobby";
    Render();
    client_ = new NatsClient(BMessenger(this));
    client_->Start(Subjects());
  }

  ~ChatWindow() override {
    delete client_;
  }

  bool QuitRequested() override {
    client_->Stop();
    return true;
  }

  void MessageReceived(BMessage* message) override {
    switch (message->what) {
      case kJoin:
        Join(channel_input_->Text(), true);
        break;
      case kSelectChannel: {
        int32 index = channels_->CurrentSelection();
        auto* item = dynamic_cast<BStringItem*>(channels_->ItemAt(index));
        if (item) {
          current_ = item->Text();
          channel_input_->SetText(current_.c_str());
          Render();
        }
        break;
      }
      case kSend:
        SendCurrent();
        break;
      case kIncoming: {
        const char* subject = nullptr;
        const char* payload = nullptr;
        const char* reply = nullptr;
        message->FindString("reply", &reply);
        if (message->FindString("subject", &subject) == B_OK &&
            message->FindString("payload", &payload) == B_OK) {
          std::string prefix = "chat.room.";
          std::string channel = subject;
          if (channel.rfind(prefix, 0) == 0) channel.erase(0, prefix.size());
          if (!ValidChannel(channel)) {
            if (reply && client_) client_->Acknowledge(reply);
            status_->SetStatus("Ignored message on invalid room subject",
                               StatusTone::kError);
            break;
          }
          ChatMessage chat{JsonField(payload, "id"), JsonField(payload, "sender"),
                           JsonField(payload, "text")};
          if (!chat.id.empty() && !RememberMessageId(chat.id)) {
            if (reply && client_) client_->Acknowledge(reply);
            break;
          }
          if (chat.sender.empty()) chat.sender = "observer";
          if (chat.text.empty()) chat.text = payload;
          auto& room = messages_[channel];
          if (room.size() >= 500) room.erase(room.begin());
          room.push_back(std::move(chat));
          if (history_.Append(channel, room.back())) history_.Compact(messages_);
          if (channel == current_) Render();
          if (reply && client_) client_->Acknowledge(reply);
        }
        break;
      }
      case kStatus: {
        const char* text = nullptr;
        int32 tone = static_cast<int32>(StatusTone::kConnecting);
        message->FindString("text", &text);
        message->FindInt32("tone", &tone);
        if (tone < static_cast<int32>(StatusTone::kConnecting) ||
            tone > static_cast<int32>(StatusTone::kError))
          tone = static_cast<int32>(StatusTone::kError);
        status_->SetStatus(text ? text : "", static_cast<StatusTone>(tone));
        break;
      }
      default:
        BWindow::MessageReceived(message);
    }
  }

 private:
  void StyleField(BTextControl* field, float divider) {
    field->SetDivider(divider);
    field->SetViewColor(kCanvas);
    field->SetLowColor(kCanvas);
    field->SetHighColor(kMuted);
    field->TextView()->SetViewColor(kSurface);
    field->TextView()->SetLowColor(kSurface);
    field->TextView()->SetHighColor(kInk);
    BFont font(*be_plain_font);
    font.SetSize(11);
    field->TextView()->SetFontAndColor(&font, B_FONT_ALL, &kInk);
    field->SetExplicitMinSize(BSize(B_SIZE_UNSET, 34));
  }

  std::string Subject(const std::string& channel) const {
    return "chat.room." + channel;
  }

  std::vector<std::string> Subjects() const {
    std::vector<std::string> result;
    for (int32 i = 0; i < channels_->CountItems(); ++i) {
      auto* item = dynamic_cast<ChannelItem*>(channels_->ItemAt(i));
      if (item) result.push_back(Subject(item->Text()));
    }
    return result;
  }

  void Join(const std::string& channel, bool subscribe) {
    if (!ValidChannel(channel)) {
      status_->SetStatus("Room: letters, digits, dash or underscore; max 48",
                         StatusTone::kError);
      return;
    }
    for (int32 i = 0; i < channels_->CountItems(); ++i) {
      auto* item = dynamic_cast<ChannelItem*>(channels_->ItemAt(i));
      if (item && channel == item->Text()) {
        channels_->Select(i);
        current_ = channel;
        Render();
        return;
      }
    }
    channels_->AddItem(new ChannelItem(channel.c_str()));
    room_rail_header_->SetCount(channels_->CountItems());
    if (subscribe && client_ && !client_->Subscribe(Subject(channel)))
      status_->SetStatus("Room added; subscription waits for connection",
                         StatusTone::kRetrying);
    channels_->Select(channels_->CountItems() - 1);
    current_ = channel;
    Render();
  }

  void SendCurrent() {
    std::string text = input_->Text();
    if (text.empty() || current_.empty()) return;
    std::string petname = petname_input_->Text();
    if (petname.empty()) petname = "observer";
    std::string id = petname + "-" + std::to_string(real_time_clock_usecs()) + "-" +
                     std::to_string(++message_counter_);
    std::string payload = "{\"id\":\"" + JsonEscape(id) +
                          "\",\"sender\":\"" + JsonEscape(petname) +
                          "\",\"text\":\"" + JsonEscape(text) + "\"}";
    if (client_->Publish(Subject(current_), payload)) {
      input_->SetText("");
      input_->MakeFocus(true);
    } else {
      status_->SetStatus("Send failed: not connected", StatusTone::kError);
    }
  }

  void Render() {
    transcript_->MakeEmpty();
    rgb_color accent = ChannelColor(current_);
    std::string current_petname = petname_input_->Text();
    for (const auto& message : messages_[current_])
      transcript_->AddItem(new MessageItem(message, accent,
                                           message.sender == current_petname));
    header_->SetRoom(current_);
    room_title_->SetRoom(current_, messages_[current_].size());
    transcript_->SetRoom(current_);
    input_->SetLabel(("TO #" + current_).c_str());
    if (transcript_->CountItems() > 0) {
      BRect last = transcript_->ItemFrame(transcript_->CountItems() - 1);
      transcript_->ScrollTo(0, std::max(0.0f, last.bottom - transcript_->Bounds().Height()));
    }
  }

  void RestoreHistory() {
    for (auto& entry : history_.Load()) {
      const std::string& channel = entry.first;
      ChatMessage& message = entry.second;
      if (!message.id.empty() && !RememberMessageId(message.id)) continue;
      if (message.sender.empty()) message.sender = "observer";
      Join(channel, false);
      auto& room = messages_[channel];
      if (room.size() >= 500) room.erase(room.begin());
      room.push_back(std::move(message));
    }
  }

  bool RememberMessageId(const std::string& id) {
    if (!seen_ids_.insert(id).second) return false;
    seen_order_.push_back(id);
    if (seen_order_.size() > 4096) {
      seen_ids_.erase(seen_order_.front());
      seen_order_.pop_front();
    }
    return true;
  }

  BTextControl* petname_input_{};
  BTextControl* channel_input_{};
  HeaderView* header_{};
  RoomTitleView* room_title_{};
  RoomRailHeaderView* room_rail_header_{};
  BListView* channels_{};
  TranscriptListView* transcript_{};
  BTextControl* input_{};
  StatusBarView* status_{};
  std::string host_;
  std::string port_;
  bool tls_;
  bool durable_;
  std::string petname_;
  std::string current_;
  std::map<std::string, std::vector<ChatMessage>> messages_;
  std::set<std::string> seen_ids_;
  std::deque<std::string> seen_order_;
  uint64 message_counter_{0};
  HistoryStore history_;
  NatsClient* client_{};
};

class ChatApplication : public BApplication {
 public:
  ChatApplication() : BApplication("application/x-vnd.plurigrid-causal-chat") {}
  void ReadyToRun() override { (new ChatWindow())->Show(); }
};

}  // namespace

int main() {
  std::signal(SIGPIPE, SIG_IGN);
  ChatApplication app;
  app.Run();
  return 0;
}
