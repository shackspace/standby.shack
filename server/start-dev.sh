rebar compile
erl -pa apps/*/ebin deps/*/ebin \
	-eval "application:start(ranch), application:start(crypto), application:start(cowboy), application:start(mimetypes), application:start(mnesia), application:start(erlydtl), application:start(lightcontrol)." \
	-eval "application:start(udpTester)."
