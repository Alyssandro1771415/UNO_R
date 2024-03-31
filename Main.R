library(R6)
source("Board.R")
source("Deck.R")
source("PlayerHand.R")

baralho <- Deck$new()
tabuleiro <- Board$new(baralho$setInitialCard())


while(tabuleiro$Winner == FALSE){

    print(tabuleiro$verifyTopDiscartStack())
    break

}
