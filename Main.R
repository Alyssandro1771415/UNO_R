library(R6)
source("Board.R")
source("Deck.R")
source("PlayerHand.R")

baralho <- Deck$new()
tabuleiro <- Board$new()
baralho$generateDeck()
baralho$shuffleDeck()


while(tabuleiro$Winner == FALSE){

    print(tabuleiro$verifyTopDiscartStack())
    break

}
