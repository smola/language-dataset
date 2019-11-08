Sender := Object clone do (
    name ::= nil
    receivers ::= nil
    init := method(setReceivers(list()))
    addReceiver := method(receiver, receivers append(receiver))
    
    sendMessage := method(msg, receivers foreach(r, r hasLocalSlot(msg) ifTrue(r perform(msg))))
)

Receiver := Object clone do (
    name ::= nil
)

receiver1 := Receiver clone setName("hi!") do (
    tacos := method("I like tacos" println)
    pizza := method("pizza is good too" println)
)

//doesn't have to clone from receiver
receiver2 := Object clone do (
    tacos := method("I do not like tacos" println)
)

sender := Sender clone setName("s") setReceivers(list(receiver1))
sender addReceiver(receiver2)
sender sendMessage("tacos")
sender sendMessage("pizza")