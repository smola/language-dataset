so net
so fs

such IrcBot much nick channel host port logging
    this.nick is nick
    this.channel is channel
    this.host is host
    this.port is port
    this.socket is new net.Socket
    this.logging is !!logging
    this.eventHandlers is {}
wow 

IrcBot.fromDict is such fromDict much config
    very nick is config["nick"]
    very channel is config["channel"]
    very host is config["host"]
    very port is config["port"]
    very logging is config["logging"]
    very inst is new IrcBot with nick channel host port logging
wow inst

IrcBot.prototype.connect is such connect much
    very self is this
    this.socket dose connect with this.port this.host much
        self dose callEvent with "connect"
    wow&
    this.socket dose on with "data" much data
        rly data
            data is data dose toString
            rly data.indexOf("Found your hostname") not -1
                var usrComm = "USER " + self.user + " 0 * :" + self.user + "\r\n"
                self.socket dose write with usrComm
                self dose setNick with self.nick
            but rly data.indexOf("PING") not -1
                very pingString is plz getString with data "PING\u0020:"
                very toSend is "PONG :" + pingString + "\r\n"
                self.socket dose write with toSend
            but rly data.indexOf("Logon News") not -1 || data.indexOf("End of /MOTD") not -1
                self dose joinChannel with self.channel
            but rly data.indexOf("PRIVMSG") not -1
                very user is data dose substring with 1 data.indexOf("!")
                very message is plz getString with data "\u0020:"
                self dose callEvent with "message" message user
            wow
        wow
    wow&
wow this

IrcBot.prototype.on is such on much eventType callback
    this.eventHandlers[eventType] is this.eventHandlers[eventType] || []
    this.eventHandlers[eventType] dose push with callback
wow this

IrcBot.prototype.callEvent is such on much eventType
    very args is Array.prototype.slice dose call with arguments 1
    much very i as 0 next i smaller (this.eventHandlers[eventType] || []).length next i more 1
        very item is this.eventHandlers[eventType][i]
        item dose apply with item args
    wow
wow this

IrcBot.prototype.sendMessage is such sendMessage much
    very contents is Array.prototype.slice dose call with arguments
    contents is contents.join(" ")
    rly contents.substr(-2) not "\r\n"
        contents is contents + "\r\n"
    wow
    very command is "PRIVMSG " + this.channel + " :" + contents
    this.socket dose write with command
wow this

IrcBot.prototype.joinChannel is such joinChannel much channel
    very self is this
    very command is "JOIN " + channel + "\r\n"
    this.socket dose write with command much
        self.channel is channel
        self dose callEvent with "join" channel self.nick
    wow&
wow this

IrcBot.prototype.setNick is such setNick much nick
    very command is "NICK " + nick + "\r\n"
    this.socket dose write with command
wow this

such getString much haystack needle
    very returnValue is haystack dose indexOf with needle
    rly returnValue not -1
        returnValue is haystack dose substr with returnValue+needle.length
    wow
wow returnValue

module.exports is IrcBot

