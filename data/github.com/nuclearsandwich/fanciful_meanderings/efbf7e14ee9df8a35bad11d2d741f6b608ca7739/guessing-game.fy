#!/usr/bin/env fancy
# A simple guessing game written in Fancy.

remaining-guesses = 6
Console println: "Welcone to the game!"

random-number = 100 random + 1

while: { remaining-guesses > 0 } do: {
  Console println: "You have #{remaining-guesses} guesses remaining."
  guess = Console readln: "Guess a number: " to_i
  Console println: "You guessed #{guess}."

  if: (random-number == guess) then: {
    Console println: "You won!"
  } else: {
    if: (random-number > guess) then: {
      Console println: "Too low."
    } else: {
      Console println: "Too high."
    }
  }
  remaining-guesses = remaining-guesses - 1
  Console newline
}

  Console println: "It was #{random-number}"
