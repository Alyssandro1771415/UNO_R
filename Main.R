library(R6)
source("Board.R")
source("Deck.R")
source("PlayerHand.R")

system("clear")

deck <- Deck$new();
deck$generateDeck();
deck$shuffleDeck();

initialCard <- deck$setInitialCard();

if(!grepl("^[0-9]+$", initialCard[2])){

    initialCard <- deck$setInitialCard();

}

board <- Board$new(initialCard[1], initialCard[2]);
board$verifyTopDiscartStack()

player_1 <- PlayerHand$new("Alyssandro");
player_2 <- PlayerHand$new("Maquina1");
player_3 <- PlayerHand$new("Maquina2");
player_4 <- PlayerHand$new("Maquina3");
deck$toDistributeCards(player_1, player_2, player_3, player_4);

players <- c(player_1, player_2, player_3, player_4);
timeToPlay <- 1;
order <- 1;

while(board$Winner == FALSE){

    if(order == 1){

        print(board$DiscartStackTop);
        print(paste0("-----------", players[[timeToPlay]]$name, "------------"));
        print(players[[timeToPlay]]$cards);
        discartedCard <- players[[timeToPlay]]$playCard(board$DiscartStackTop, deck, board$DiscartStack);
        board$addCardToDiscartStack(discartedCard)
        board$verifyTopDiscartStack()

        if(timeToPlay == 4){
            timeToPlay <- 1;
        } else{
            timeToPlay <- timeToPlay + 1;
        }
    
    }

    if(order == -1){

        print(board$DiscartStackTop);
        print(paste0("-----------", players[[timeToPlay]]$name, "------------"));
        print(players[[timeToPlay]]$cards);
        discartedCard <- players[[timeToPlay]]$playCard(board$DiscartStackTop, deck, board$DiscartStack);
        board$addCardToDiscartStack(discartedCard)
        board$verifyTopDiscartStack()

        if(timeToPlay == 1){
            timeToPlay <- 4;
        } else{
            timeToPlay <- timeToPlay - 1;
        }
        
    }

    system("clear")

}
