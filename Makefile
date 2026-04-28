LATEST_VER=0.10.5
JVM_OPTS=-Xms128m -Xmx192m -Xss256k -XX:MaxMetaspaceSize=96m -XX:ReservedCodeCacheSize=64m -XX:+UseSerialGC
NATIVE_IMAGE=

build:
	scala-cli --power package . --main-class scala_bot.main -o scala-bot-$(LATEST_VER).jar --assembly --preamble=false --force

build-native:
	$(NATIVE_IMAGE) --enable-url-protocols=https -jar ./scala-bot-$(LATEST_VER).jar

start:
	java $(JVM_OPTS) -jar scala-bot-$(LATEST_VER).jar index=$(INDEX)
