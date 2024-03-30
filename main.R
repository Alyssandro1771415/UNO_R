library(R6)
source("Board.R")
source("Deck.R")
source("PlayerHand.R")

baralho <- Deck$new()

baralho$generateDeck()

baralho$shuffleDeck()

print(baralho$cards)
