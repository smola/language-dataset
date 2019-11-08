require: "../lib/fancy_irc"
require: "mocks/client"

MockedClient = FancyIRC Testing MockedClient

Greeting_Bakkdoor = "Hey, how are you today, bakkdoor?"
Greeting_Other = "Hello, are you new to Fancy?"

Client = MockedClient new: {
  on: 'channel pattern: (/^hello/i) do: |msg| {
    match msg author {
      case "bakkdoor" -> msg reply: Greeting_Bakkdoor
      case _ -> msg reply: Greeting_Other
    }
  }
}

def message: message author: author channel: channel ("#fancy") {
  FancyIRC Message new: message author: author channel: channel timestamp: (Time now) client: Client
}

FancySpec describe: FancyIRC Message with: {
  it: "sends the expected reply when matching a pattern" when: {
    m1 = message: "Hello everyone!" author: "bakkdoor"
    m2 = message: "Hello to you too!" author: "brixen"
    m3 = message: "Woot" author: "evanphx"

    Client incoming: m1
    Client handle_incoming
    Client outgoing_on: "#fancy" . last is == Greeting_Bakkdoor
    Client incoming: m2
    Client handle_incoming
    Client outgoing_on: "#fancy" . last is == Greeting_Other
    Client clear_outgoing
    Client incoming: m3
    Client outgoing_on: "#fancy" . empty? is == true # no messages replied to at this point
  }
}