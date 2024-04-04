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

        #comprar carta do deck
        drawCard = function(deck, quant, discardStack){

            if (quant > nrow(deck$cards)){

                discartStackTop <- tail(discardStack, 1)
                deck$cards <- deck$cards[]
                drawCard(deck, quant, discardStack)

            }
                    
            switch(quant,
                "1" = {
                    cards_taken <- deck$cards[1,]
                    deck$cards <- deck$cards[-1,]
                    self$cards <- rbind(self$cards, cards_taken[1:2])
                },
                "2" = {
                    cards_taken <- deck$cards[1:2]
                    deck$cards <- deck$cards[-(1:2)]
                    self$cards <- rbind(self$cards, cards_taken[1:2])
                },
                "4" = {
                    cards_taken <- deck$cards[1:4]
                    deck$cards <- deck$cards[-(1:4)]
                    self$cards <- rbind(self$cards, cards_taken[1:2])
                }
            )
            system("clear")
            paste0("-----------", players[[timeToPlay]]$name, "------------")
            print("Mão atual:")
            print(self$cards)
            Sys.sleep(3)        
        },

        playCard = function(DiscartStackTop, deck, discardStack) {

            self$verifyWin()

            if (self$verifyWin() == TRUE) {

                stop("O jogo terminou!")
            }

            var <- as.integer(readline(prompt="Digite o número da linha: "))

            if(var == 0){

                self$drawCard(deck, 1, discardStack)

                return()
                
            }

            if(self$cards[as.integer(var),1] == head(DiscartStackTop,1) || self$cards[as.integer(var),2] == tail(DiscartStackTop, 1) || self$cards[as.integer(var),1] == "Preto"){

                DiscartStackTop <- self$cards[var,]
                self$cards <- self$cards[-var,]
                return(DiscartStackTop)

            } else{

                self$playCard(DiscartStackTop, deck, discardStack);

            }

            
        },

        blockCard = function(timeToPlay){
            timeToPlay <- timeToPlay + 1;
        },

        reverseCard = function(order){
            if (order == 1){order <- -1;}
            
            if (order == -1) {order <- 1;}
        },

        changeColor = function(cor){
          switch  (cor,
            "Amarelo" = {
                print("A cor é amarelo.");
                card <- c(cor, "trocaCor");
                return(card);
            },
            "Verde" = {
                print("A cor é verde.")
                card <- c(cor, "trocaCor");
                return(card);
            },
            "Vermelho" = {
                print("A cor é vermelho.")
                card <- c(cor, "trocaCor");
                return(card);
            },
            "Azul" = {
                print("A cor é azul.")
                card <- c(cor, "trocaCor");
                return(card);
            },
            {
                print("Cor desconhecida.")
            })
        },

        useSpecialCard = function(card, deck, discardStack) {
            if (any(card[2,] %in% 0:9)) {
                return()
            }

            switch (card[2,],
              "+2" = {
                drawCard(deck, 2, discardStack);
                return()
              },
              "+4" = {
                drawCard(deck, 4, discardStack);
                return();
              },
              "Block" = {
                blockCard(timeToPlay);
                return();
              },
              "Reverse" = {
                reverseCard(order);
                return();
              },
              "trocaCor" = {
                trocaCor(cor);
                return();
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
        }
    )
)
