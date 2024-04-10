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
    
    # Caso de erro na block ou reverse, rever isso!!!
        nextPlayer <- NULL;
        if(timeToPlay >= 4){
            nextPlayer <- players[[1]]
        } else{
            nextPlayer <- players[[timeToPlay+1]]
        }

        if(class(board$DiscartStackTop) == "list"){
            
            print(board$DiscartStackTop[[1]]);
            print(paste0("-----------", players[[timeToPlay]]$name, "------------"));
            print(players[[timeToPlay]]$cards[1:(nrow(players[[timeToPlay]]$cards) - 1), ])
            discartedCard <- players[[timeToPlay]]$playCard(board$DiscartStackTop[[1]], deck, board$DiscartStack, nextPlayer, timeToPlay, playerName);

            board$addCardToDiscartStack(discartedCard)

            if(class(discartedCard) == "NULL"){

                if(class(board$DiscartStackTop[[2]]) == "NULL"){
                    board$setTopDiscartStack(board$DiscartStackTop)
                } else{
                    board$setTopDiscartStack(board$DiscartStackTop[[2]])
                }

                if((names(discartedCard))[2] == "specialActionBlock"){
                    if(discartedCard[[2]] >= 4){
                        timeToPlay <- 1
                    } else{
                        timeToPlay <- discartedCard[[2]]
                    }
                }

                if(timeToPlay >= 4){
                    timeToPlay <- 1;
                } else{
                    timeToPlay <- timeToPlay + 1;
                }

                system("clear"))
                next;   
            }  else{

                board$verifyTopDiscartStack();

            }

            if((names(discartedCard))[2] == "specialActionBlock"){
                if(discartedCard[[2]] >= 4){
                    timeToPlay <- 1
                } else{
                    timeToPlay <- discartedCard[[2]]
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

            board$verifyWin(nrow(players[[timeToPlay]]$cards))

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
            print(players[[timeToPlay]]$cards[1:(nrow(players[[timeToPlay]]$cards) - 1), ])
            discartedCard <- players[[timeToPlay]]$playCard(board$DiscartStackTop, deck, board$DiscartStack, nextPlayer, timeToPlay, playerName);

            board$addCardToDiscartStack(discartedCard)

            if(class(discartedCard) == "NULL"){

                if(class(board$DiscartStackTop[[2]]) == "NULL"){
                    board$setTopDiscartStack(board$DiscartStackTop)
                }

                if((names(discartedCard))[2] == "specialActionBlock"){
                    if(discartedCard[[2]] >= 4){
                        timeToPlay <- 1
                    } else{
                        timeToPlay <- discartedCard[[2]]
                    }
                }

                if(timeToPlay >= 4){
                    timeToPlay <- 1;
                } else{
                    timeToPlay <- timeToPlay + 1;
                }

                system("clear"))
                next;   
            }  else{

                board$verifyTopDiscartStack();

            }
            
            if((names(discartedCard))[2] == "specialActionBlock"){
                if(discartedCard[[2]] >= 4){
                    timeToPlay <- 1
                } else{
                    timeToPlay <- discartedCard[[2]]
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

            board$verifyWin(nrow(players[[timeToPlay]]$cards))

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

        # Caso de erro na block ou reverse, rever isso!!!
        nextPlayer <- NULL;
        if(timeToPlay <= 1){
            nextPlayer <- players[[4]]
        } else{
            nextPlayer <- players[[timeToPlay-1]]
        }

        if(class(board$DiscartStackTop) == "list"){
            
            print(board$DiscartStackTop[[1]]);
            print(paste0("-----------", players[[timeToPlay]]$name, "------------"));
            print(players[[timeToPlay]]$cards[1:(nrow(players[[timeToPlay]]$cards) - 1), ])
            discartedCard <- players[[timeToPlay]]$playCard(board$DiscartStackTop[[1]], deck, board$DiscartStack, nextPlayer, timeToPlay, playerName);
            
            board$addCardToDiscartStack(discartedCard)


            if(class(discartedCard) == "NULL"){

                if(class(board$DiscartStackTop[[2]]) == "NULL"){
                    board$setTopDiscartStack(board$DiscartStackTop)
                } else{
                    board$setTopDiscartStack(board$DiscartStackTop[[2]])
                }

                if((names(discartedCard))[2] == "specialActionBlock"){
                    if(discartedCard[[2]] <= 1){
                        timeToPlay <- 4
                    } else{
                        timeToPlay <- discartedCard[[2]]
                    }
                }

                if(timeToPlay <= 1){
                    timeToPlay <- 3;
                } else{
                    timeToPlay <- timeToPlay - 1;
                }

                system("clear"))
                next;   
            }  else{

                board$verifyTopDiscartStack();

            }

            if((names(discartedCard))[2] == "specialActionBlock"){
                if(discartedCard[[2]] <= 1){
                    timeToPlay <- 4
                } else{
                    timeToPlay <- discartedCard[[2]]
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

            board$verifyWin(nrow(players[[timeToPlay]]$cards))

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
            print(players[[timeToPlay]]$cards[1:(nrow(players[[timeToPlay]]$cards) - 1), ])
            discartedCard <- players[[timeToPlay]]$playCard(board$DiscartStackTop, deck, board$DiscartStack, nextPlayer, timeToPlay, playerName);
            
            board$addCardToDiscartStack(discartedCard)

            if(class(discartedCard) == "NULL"){

                if(class(board$DiscartStackTop[[2]]) == "NULL"){
                    board$setTopDiscartStack(board$DiscartStackTop)
                } else{
                    board$setTopDiscartStack(board$DiscartStackTop[[2]])
                }

                if((names(discartedCard))[2] == "specialActionBlock"){
                    if(discartedCard[[2]] <= 1){
                        timeToPlay <- 4
                    }
                }

                if(timeToPlay <= 1){
                    timeToPlay <- 3;
                } else{
                    timeToPlay <- timeToPlay - 1;
                }

                system("clear"))
                next;   
            }  else{

                board$verifyTopDiscartStack();

            }

            if((names(discartedCard))[2] == "specialActionBlock"){
                if(discartedCard[[2]] <= 1){
                    timeToPlay <- 4
                } else{
                    timeToPlay <- discartedCard[[2]]
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

            board$verifyWin(nrow(players[[timeToPlay]]$cards))

            if(timeToPlay <= 1){
                timeToPlay <- 4;
            } else{
                timeToPlay <- timeToPlay - 1;
            }

            system("clear")
            next;

        }
    }
}
