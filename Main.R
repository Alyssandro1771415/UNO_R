library(R6)
source("Board.R")
source("Deck.R")
source("PlayerHand.R")

baralho <- Deck$new();
baralho$generateDeck();
baralho$shuffleDeck();


initialCard <- baralho$setInitialCard();
tabuleiro <- Board$new(initialCard[1], initialCard[2]);

while(tabuleiro$Winner == FALSE){

    print(tabuleiro$verifyTopDiscartStack());    
    break;
    

}
