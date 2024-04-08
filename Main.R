library(R6)
source("Board.R")
source("Deck.R")
source("PlayerHand.R")

system("clear")

deck <- Deck$new();
deck$generateDeck();
deck$shuffleDeck();

initialCard <- deck$setInitialCard();

while(!grepl("^[0-9]+$", initialCard[2])){

    initialCard <- deck$setInitialCard();

}

board <- Board$new(initialCard[1], initialCard[2]);
board$verifyTopDiscartStack()

playerName <- as.character(readline(prompt="Digite seu nome: "))
system("clear")

player_1 <- PlayerHand$new(playerName);
player_2 <- PlayerHand$new("Maquina1");
player_3 <- PlayerHand$new("Maquina2");
player_4 <- PlayerHand$new("Maquina3");
deck$toDistributeCards(player_1, player_2, player_3, player_4);

players <- c(player_1, player_2, player_3, player_4);
timeToPlay <- 1;
order <- 1;

while(board$Winner == FALSE){

    if(order == 1){

        nextPlayer <- NULL;

        if(timeToPlay+1 > 3){
            nextPlayer <- 1
        } else{
            nextPlayer <- timeToPlay+1
        }

        if(class(board$DiscartStackTop) == "list"){
            
            print(board$DiscartStackTop[[1]]);
            print(paste0("-----------", players[[timeToPlay]]$name, "------------"));
            print(players[[timeToPlay]]$cards);
            discartedCard <- players[[timeToPlay]]$playCard(board$DiscartStackTop[[1]], deck, board$DiscartStack, players[[nextPlayer]], timeToPlay, playerName);
            
            board$addCardToDiscartStack(discartedCard)
            board$verifyTopDiscartStack()

            if(class(discartedCard) == "NULL"){

                if(timeToPlay >= 4){
                    timeToPlay <- 1;
                } else{
                    timeToPlay <- timeToPlay + 1;
                }
                system("clear")
                next;   
            }

            if((names(discartedCard))[2] == "specialActionBlock"){
                timeToPlay <- discartedCard[[2]]
                if(timeToPlay >= 4){
                    timeToPlay <- 1
                }
            }
            if((names(discartedCard))[2] == "specialActionReverse"){
                if(timeToPlay == 1){
                    order <- discartedCard[[2]]
                    order <- discartedCard[[2]]
                    timeToPlay <- 4
                } else{
                    order <- discartedCard[[2]]
                    timeToPlay <- timeToPlay - 2
                }
            }
            if((names(discartedCard))[2] == "specialActionChangeColor"){
                board$DiscartStackTop <- discartedCard[[2]]
            }

            if(timeToPlay >= 4){
                timeToPlay <- 1;
            } else{
                timeToPlay <- timeToPlay + 1;
            }

            system("clear")
            next;

        } else{

            print(board$DiscartStackTop);
            print(paste0("-----------", players[[timeToPlay]]$name, "------------"));
            print(players[[timeToPlay]]$cards);
            discartedCard <- players[[timeToPlay]]$playCard(board$DiscartStackTop, deck, board$DiscartStack, players[[nextPlayer]], timeToPlay, playerName);
            
            board$addCardToDiscartStack(discartedCard)
            board$verifyTopDiscartStack()


            if(class(discartedCard) == "NULL"){

                if(timeToPlay >= 4){
                    timeToPlay <- 1;
                } else{
                    timeToPlay <- timeToPlay + 1;
                }
                system("clear")
                next;   
            }


            if((names(discartedCard))[2] == "specialActionBlock"){
                timeToPlay <- discartedCard[[2]]
                if(timeToPlay >= 4){
                    timeToPlay <- 1
                }
            }
            if((names(discartedCard))[2] == "specialActionReverse"){
                if(timeToPlay == 1){
                    order <- discartedCard[[2]]
                    timeToPlay <- 3
                } else{
                    order <- discartedCard[[2]]
                    timeToPlay <- timeToPlay - 2
                }
            }
            if((names(discartedCard))[2] == "specialActionChangeColor"){
                board$DiscartStackTop <- discartedCard[[2]]
            }

            if(timeToPlay >= 4){
                timeToPlay <- 1;
            } else{
                timeToPlay <- timeToPlay + 1;
            }

            system("clear")
            next;

        }
    }

    if(order == -1){

        nextPlayer <- NULL

        if(timeToPlay-1 < 1){
            nextPlayer <- 4
        } else{
            nextPlayer <- timeToPlay-1
        }

        if(class(board$DiscartStackTop) == "list"){
            
            print(board$DiscartStackTop[[1]]);
            print(paste0("-----------", players[[timeToPlay]]$name, "------------"));
            print(players[[timeToPlay]]$cards);
            discartedCard <- players[[timeToPlay]]$playCard(board$DiscartStackTop[[1]], deck, board$DiscartStack, players[[nextPlayer]], timeToPlay, playerName);
            
            board$addCardToDiscartStack(discartedCard)
            board$verifyTopDiscartStack()


            if(class(discartedCard) == "NULL"){

                if(timeToPlay <= 1){
                    timeToPlay <- 3;
                } else{
                    timeToPlay <- timeToPlay - 1;
                }
                system("clear")
                next;   
            }

            if((names(discartedCard))[2] == "specialActionBlock"){
                timeToPlay <- discartedCard[[2]]
                if(timeToPlay <= 1){
                    timeToPlay <- 4
                }
            }
            if((names(discartedCard))[2] == "specialActionReverse"){
                if(timeToPlay == 4){
                    order <- discartedCard[[2]]
                    timeToPlay <- 2
                }else{
                    order <- discartedCard[[2]]
                    timeToPlay <- timeToPlay + 2
                }
            }
            if((names(discartedCard))[2] == "specialActionChangeColor"){
                board$DiscartStackTop <- discartedCard[[2]]
            }

            if(timeToPlay <= 1){
                timeToPlay <- 4;
            } else{
                timeToPlay <- timeToPlay - 1;
            }

            system("clear")
            next;

        } else{

            print(board$DiscartStackTop);
            print(paste0("-----------", players[[timeToPlay]]$name, "------------"));
            print(players[[timeToPlay]]$cards);
            discartedCard <- players[[timeToPlay]]$playCard(board$DiscartStackTop, deck, board$DiscartStack, players[[nextPlayer]], timeToPlay, playerName);
            
            board$addCardToDiscartStack(discartedCard)
            board$verifyTopDiscartStack()

            if(class(discartedCard) == "NULL"){

                if(timeToPlay <= 1){
                    timeToPlay <- 4;
                } else{
                    timeToPlay <- timeToPlay - 1;
                }
                next;   
            }


            if((names(discartedCard))[2] == "specialActionBlock"){
                timeToPlay <- discartedCard[[2]]
                if(timeToPlay <= 1){
                    timeToPlay <- 4
                }
            }
            if((names(discartedCard))[2] == "specialActionReverse"){
                if(timeToPlay == 4){
                    order <- discartedCard[[2]]
                    timeToPlay <- 2
                }else{
                    order <- discartedCard[[2]]
                    timeToPlay <- timeToPlay + 2
                }
            }
            if((names(discartedCard))[2] == "specialActionChangeColor"){
                board$DiscartStackTop <- discartedCard[[2]]
            }

            if(timeToPlay <= 1){
                timeToPlay <- 4;
            } else{
                timeToPlay <- timeToPlay - 1;
            }

            board$Winner <- players[[timeToPlay]]$verifyWin();
            if(nrow(players[[timeToPlay]]) == 1){
                Sys.sleep(3)
            }

            system("clear")
            next;

        }
    }

}
