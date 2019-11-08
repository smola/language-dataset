REBOL [
    Title:      "Fetch - command"
    Name:       fetch-command
    Type:       module
    Role:       command
    Version:    1.0.0
    Needs:      [bot-api 1.0.0]
    Date:       16-June-2013
    Author:     "Graham Chiu"
    Options:    [private]
]

help-string: {fetch id "retrieves stored JSON message by its message-id"}

target: err: _

dialect-rule: [
    'fetch set target integer! (
        done: true
        either exists? join-of storage target [
            reply message-id to string! read join-of storage target
        ][
            reply message-id ajoin [ "Sorry mate, message " target " is not in my store" ]
        ]
    )
]
    