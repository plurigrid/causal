CXX ?= c++
CXXFLAGS ?= -std=c++17 -O2 -Wall -Wextra -pedantic
TLS_CXXFLAGS ?=
TLS_LIBS ?= -lssl -lcrypto

.PHONY: all clean

all: CausalChat nats-chat

CausalChat: haiku_chat.cpp
	$(CXX) $(CXXFLAGS) $(TLS_CXXFLAGS) $< -lbe -lnetwork $(TLS_LIBS) -o $@

nats-chat: nats_chat.cpp
	$(CXX) $(CXXFLAGS) $< -lnetwork -o $@

clean:
	rm -f CausalChat nats-chat
