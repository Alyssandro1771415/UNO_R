library(R6)
source("Board.R")

PlayerHand <- R6Class("PlayerHand",
    public = list(

        name = NULL,
        cards = NULL,
        
        initialize = function(playerName = NA){
            self$cards <- matrix(ncol = 2, byrow = TRUE)
            colnames(self$cards) <- c("Tipo","Cor")
            self$name <- playerName;
        },

        drawCard = function(deck, quant, discardStack, IA){

            if (quant > nrow(deck$cards)){

                discartStackTop <- tail(discardStack, 1)
                deck$cards <- deck$cards[]
                drawCard(deck, quant, discardStack)

            }
                    
            switch(as.character(quant),
                "1" = {
                    cards_taken <- deck$cards[1,]
                    deck$cards <- deck$cards[-1,]
                    self$cards <- rbind(cards_taken[1:2], self$cards)
                    system("clear")
                    paste0("-----------", players[[timeToPlay]]$name, "------------")
                    print("Mão atual:")
                    print(self$cards[1:(nrow(self$cards)-1),])
                    Sys.sleep(3)   
                },
                "2" = {
                    cards_taken <- deck$cards[1:2,]
                    deck$cards <- deck$cards[-(1:2),]
                    self$cards <- rbind(cards_taken[1:2,], self$cards)
                },
                "4" = {
                    cards_taken <- deck$cards[1:4,]
                    deck$cards <- deck$cards[-(1:4),]
                    self$cards <- rbind(cards_taken[1:4,], self$cards)
                    return(self$changeColor(IA, "+4"))
                }
            )
     
        },

        playCard = function(DiscartStackTop, deck, discardStack, nextPlayer, timeToPlay, player_name, cardThisTurn) {

            print("================================")

            if(self$name != player_name){

                var <- self$IA(DiscartStackTop)
                print(self$cards[var,])
                Sys.sleep(3)

            }  else{

                var <- as.integer(readline(prompt="Digite o número da linha: "))

            }

            if(as.integer(var) > nrow(self$cards)){

                var <- as.integer(readline(prompt="Carta inválida! Digite o número da linha: "))
            
            }

            if(var == 0){

                self$drawCard(deck, 1, discardStack)

                return()
                
            }

            if(self$cards[as.integer(var),1] == head(DiscartStackTop,1) || self$cards[as.integer(var),2] == tail(DiscartStackTop, 1) || self$cards[as.integer(var),1] == "Preto" || tail(DiscartStackTop, 1) == "Preto"){

                DiscartStackTop <- self$cards[var,]
                actionOfTheCard <- NULL

                if (!(any(self$cards[var,2] %in% 0:9))) {

                    if(self$name != player_name){

                        actionOfTheCard <- self$useSpecialCard(self$cards[var,], deck, discardStack, nextPlayer, timeToPlay, TRUE)
                        
                    } else{

                        actionOfTheCard <- self$useSpecialCard(self$cards[var,], deck, discardStack, nextPlayer, timeToPlay, FALSE)
                        
                    }
                
                }
                
                if(self$cards[var, 2] == "Block"){
                    self$cards <- self$cards[-var,]
                    return(list(topDiscart = DiscartStackTop, specialActionBlock = actionOfTheCard))
                }
                if(self$cards[var, 2] == "Reverse"){
                    self$cards <- self$cards[-var,]
                    return(list(topDiscart = DiscartStackTop, specialActionReverse = actionOfTheCard))
                }
                if(self$cards[var, 2] == "trocaCor" || self$cards[var, 1] == "Preto"){
                    self$cards <- self$cards[-var,]
                    return(list(topDiscart = DiscartStackTop, specialActionChangeColor = actionOfTheCard))
                }

                self$cards <- self$cards[-var,]
                return(list(topDiscart = DiscartStackTop, actionOfTheCard = actionOfTheCard))

            } else{

                self$playCard(DiscartStackTop, deck, discardStack, nextPlayer, timeToPlay, player_name);

            }

            
        },

        blockCard = function(timeToPlay, order){
            if(order == 1){
                if(timeToPlay == 3){
                    timeToPlay <- 1;
                    return(timeToPlay)
                } else{
                    timeToPlay <- timeToPlay + 1;
                    return(timeToPlay)
                }
                
            }
            if(order == -1){
                if(timeToPlay == 2){
                    timeToPlay <- 4;
                    return(timeToPlay)
                } else{
                    timeToPlay <- timeToPlay - 1;
                    return(timeToPlay)
                }
            }
        },

        reverseCard = function(order){
            if (order == 1){
                order <- -1
                return(order)
            }
            
            if (order == -1) {
                order <- 1
                return(order)
            }
        },

        changeColor = function(IA, whoCall){

            if(IA == TRUE){

                cor <- sample(1:4, 1, replace = FALSE)
                cor <- as.character(cor)

            } else{
                print("Amarelo: 1")
                print("Verde: 2")
                print("Vermelho: 3")
                print("Azul: 4")

                cor <- as.character(readline(prompt="Digite o número da cor: "))
            }

            print(cor)
            Sys.sleep(3)

            switch  (cor,
                "1" = {
                    print("A cor é amarelo.");
                    card <- c("Amarelo", whoCall);
                    return(card);
                },
                "2" = {
                    print("A cor é verde.");
                    card <- c("Verde", whoCall);
                    return(card);
                },
                "3" = {
                    print("A cor é vermelho.");
                    card <- c("Vermelho", whoCall);
                    return(card);
                },
                "4" = {
                    print("A cor é azul.");
                    card <- c("Azul", whoCall);
                    return(card);
                },
                {
                    print("Cor desconhecida.");
                    self$changeColor(IA, whoCall);
                }
            )
        },

        useSpecialCard = function(card, deck, discardStack, nextPlayer, timeToPlay, IA) {

            switch (card[2],
              "+2" = {
                nextPlayer$drawCard(deck, 2, discardStack);
                return()
              },
              "+4" = {
                actionOfTheCard <- nextPlayer$drawCard(deck, 4, discardStack, IA);
                return(actionOfTheCard);
              },
              "Block" = {
                timeToPlay <- self$blockCard(timeToPlay, order);
                return(timeToPlay)
              },
              "Reverse" = {
                reverter <- self$reverseCard(order);
                return(reverter);
              },
              "trocaCor" = {
                carta <- self$changeColor(IA, "trocaCor");
                return(carta);
              }
            )
        },

        IA = function(DiscardStackTop) {
            validas <- which(self$cards[, 1] == DiscardStackTop[1] | self$cards[, 2] == DiscardStackTop[2])
            especiais <- which((self$cards[, 1] == DiscardStackTop[1] & self$cards[, 2] == "+2") |
                            (self$cards[, 1] == DiscardStackTop[1] & self$cards[, 2] == "Block") |
                            (self$cards[, 1] == DiscardStackTop[1] & self$cards[, 2] == "Reverse") |
                            self$cards[, 2] %in% c("+4", "trocaCor"))

            if (length(especiais) > 0) {
                Sys.sleep(3)
                return(especiais[1])
            }

            if (length(validas) > 0) {
                Sys.sleep(3)
                return(validas[1])
            }

            return(0)
        }
    )
)
