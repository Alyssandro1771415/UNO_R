library(R6)
source("Board.R")
source("Deck.R")
source("PlayerHand.R")

baralho <- Deck$new();
baralho$generateDeck();
baralho$shuffleDeck();

initialCard <- baralho$setInitialCard();
tabuleiro <- Board$new(initialCard[1], initialCard[2]);

player_1 <- PlayerHand$new("Alyssandro");
player_2 <- PlayerHand$new("Maquina1");
player_3 <- PlayerHand$new("Maquina2");
player_4 <- PlayerHand$new("Maquina3");


while(tabuleiro$Winner == FALSE){

    baralho$toDistributeCards(player_1, player_2, player_3, player_4);

    #print(player_1$cards);
    #print(player_2$cards);
    #print(player_3$cards);
    #print(player_4$cards);
    #print(tabuleiro$DiscartStack);
    print(baralho$cards)    
    break;
    

}
