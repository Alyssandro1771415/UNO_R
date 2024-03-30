library(R6)
source("Board.R")
source("Deck.R")
source("PlayerHand.R")

baralho <- Deck$new()
baralho$generateDeck()
baralho$shuffleDeck()
tabuleiro <- Board$new(baralho$setInitialCard())


while(tabuleiro$Winner == FALSE){

    print(tabuleiro$verifyTopDiscartStack())
    break

}
