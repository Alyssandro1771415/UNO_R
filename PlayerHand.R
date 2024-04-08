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

        drawCard = function(deck, quant, discardStack){

            if (quant > nrow(deck$cards)){

                discartStackTop <- tail(discardStack, 1)
                deck$cards <- deck$cards[]
                drawCard(deck, quant, discardStack)

            }
                    
            switch(as.character(quant),
                "1" = {
                    cards_taken <- deck$cards[1,]
                    deck$cards <- deck$cards[-1,]
                    self$cards <- rbind(self$cards, cards_taken[1:2])
                    system("clear")
                    paste0("-----------", players[[timeToPlay]]$name, "------------")
                    print("Mão atual:")
                    print(self$cards)
                    Sys.sleep(3)   
                },
                "2" = {
                    cards_taken <- deck$cards[1:2,]
                    deck$cards <- deck$cards[-(1:2),]
                    self$cards <- rbind(self$cards, cards_taken[1:2,])
                },
                "4" = {
                    cards_taken <- deck$cards[1:4,]
                    deck$cards <- deck$cards[-(1:4),]
                    self$cards <- rbind(self$cards, cards_taken[1:4,])
                }
            )
     
        },

        playCard = function(DiscartStackTop, deck, discardStack, nextPlayer, timeToPlay, player_name) {

            self$verifyWin()

            if (self$verifyWin() == TRUE) {

                stop("O jogo terminou!")
            }

            if(self$name != player_name){

                var <- self$IA(DiscartStackTop)
                print(self$cards[var,])
                Sys.sleep(5)

            }  else{

                var <- as.integer(readline(prompt="Digite o número da linha: "))

            }

            if(var == 0){

                self$drawCard(deck, 1, discardStack)

                return()
                
            }

            if(self$cards[as.integer(var),1] == head(DiscartStackTop,1) || self$cards[as.integer(var),2] == tail(DiscartStackTop, 1) || self$cards[as.integer(var),1] == "Preto" || tail(DiscartStackTop, 1) == "Preto"){

                DiscartStackTop <- self$cards[var,]
                actionOfTheCard <- NULL

                if (!(any(self$cards[var,2] %in% 0:9))) {

                    actionOfTheCard <- self$useSpecialCard(self$cards[var,], deck, discardStack, nextPlayer, timeToPlay)

                }
                
                if(self$cards[var, 2] == "Block"){
                    self$cards <- self$cards[-var,]
                    return(list(topDiscart = DiscartStackTop, specialActionBlock = actionOfTheCard))
                }
                if(self$cards[var, 2] == "Reverse"){
                    self$cards <- self$cards[-var,]
                    return(list(topDiscart = DiscartStackTop, specialActionReverse = actionOfTheCard))
                }
                if(self$cards[var, 2] == "trocaCor"){
                    return(list(topDiscart = DiscartStackTop, specialActionChangeColor = actionOfTheCard))
                }

                self$cards <- self$cards[-var,]
                return(list(topDiscart = DiscartStackTop, actionOfTheCard = actionOfTheCard))

            } else{

                self$playCard(DiscartStackTop, deck, discardStack);

            }

            
        },

        blockCard = function(timeToPlay){
            timeToPlay <- timeToPlay + 1;
            return(timeToPlay)
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

        changeColor = function(){

            print("Amarelo: 1")
            print("Verde: 2")
            print("Vermelho: 3")
            print("Azul: 4")

            cor <- as.character(readline(prompt="Digite o número da cor: "))
        
            switch  (cor,
                "1" = {
                    print("A cor é amarelo.");
                    card <- c("Amarelo", "trocaCor");
                    return(card);
                },
                "2" = {
                    print("A cor é verde.");
                    card <- c("Verde", "trocaCor");
                    return(card);
                },
                "3" = {
                    print("A cor é vermelho.");
                    card <- c("Vermelho", "trocaCor");
                    return(card);
                },
                "4" = {
                    print("A cor é azul.");
                    card <- c("Azul", "trocaCor");
                    return(card);
                },
                {
                    print("Cor desconhecida.");
                    self$changeColor();
                }
            )
        },

        useSpecialCard = function(card, deck, discardStack, nextPlayer, timeToPlay) {

            switch (card[2],
              "+2" = {
                nextPlayer$drawCard(deck, 2, discardStack);
                return()
              },
              "+4" = {
                nextPlayer$drawCard(deck, 4, discardStack);
                return();
              },
              "Block" = {
                timeToPlay <- self$blockCard(timeToPlay);
                return(timeToPlay)
              },
              "Reverse" = {
                reverter <- self$reverseCard(order);
                return(reverter);
              },
              "trocaCor" = {
                carta <- self$changeColor();
                return(carta);
              }
            )
        },

        verifyWin = function(){
            if (nrow(self$cards)==0){
                return(TRUE)
            }
            else if (nrow(self$cards)==1) {
                print("UNO !!!")
                return(FALSE)
            }
            else {
                return(FALSE)
            }   
        },

        IA = function(DiscardStackTop) {
        validas <- which(self$cards[1, 1] == DiscardStackTop[1] || self$cards[2, 1] == DiscardStackTop[2])
        especiais <- which((self$cards[, 1] == DiscardStackTop[1] & self$cards[, 2] == "+2") |
                        (self$cards[, 1] == DiscardStackTop[1] & self$cards[, 2] == "Block") |
                        (self$cards[, 1] == DiscardStackTop[1] & self$cards[, 2] == "Reverse") |
                        self$cards[, 2] %in% c("+4", "trocaCor"))

            if (length(especiais) > 0) {
                print("especiais")
                print(especiais[1])
                Sys.sleep(5)
                return(especiais[1])
            }

            if (length(validas) > 0) {
                print("validas")
                print(validas[1])
                Sys.sleep(5)
                return(validas[1])
            }

            return(0)
        }
    )
)
