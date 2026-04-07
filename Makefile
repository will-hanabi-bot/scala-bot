LATEST_VER=0.9.3
JVM_OPTS=-Xms128m -Xmx192m -Xss256k -XX:MaxMetaspaceSize=96m -XX:ReservedCodeCacheSize=64m -XX:+UseSerialGC

build:
	scala-cli --power package . --main-class scala_bot.main -o scala-bot-$(LATEST_VER).jar --assembly --preamble=false --force

start:
	java $(JVM_OPTS) -jar scala-bot-$(LATEST_VER).jar index=$(INDEX)
