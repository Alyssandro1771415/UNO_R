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
        drawCard = function(deck,quant){

            if (quant > nrow(deck$cards)){
                # falta implementar adicionar a pilha de descarte no deck menos a carta do topo e embaralhar

            }
                    
            switch(quant,
                "1" = {
                    cards_taken <- deck$cards[1,]
                    deck$cards <- deck$cards[-1,]
                    self$cards <- rbind(self$cards, cards_taken)
                },
                "2" = {
                    cards_taken <- deck$cards[1:2]
                    deck$cards <- deck$cards[-(1:2)]
                    self$cards <- rbind(self$cards, cards_taken)
                },
                "4" = {
                    cards_taken <- deck$cards[1:4]
                    deck$cards <- deck$cards[-(1:4)]
                    self$cards <- rbind(self$cards, cards_taken)
                }
            )           
        },

        playCard = function(DiscartStackTop) {

            self$verifyWin()

            if (self$verifyWin() == TRUE) {

                stop("O jogo terminou!")
            }

            var <- as.integer(readline(prompt="Digite o número da linha: "))

            if(self$cards[as.integer(var),1] == head(DiscartStackTop,1) || self$cards[as.integer(var),2] == tail(DiscartStackTop, 1)){

                DiscartStackTop <- self$cards[var,]
                self$cards <- self$cards[-var,]
                return(DiscartStackTop)

            } else{

                self$playCard(DiscartStackTop);

            }

            
        },

        blockCard = function(timeToPlay){
            timeToPlay <- timeToPlay + 2;
        },

        reverseCard = function(order){
            order <- -1;
        },

        changeColor = function(){
            var <- readLine("Digite a cor: ");
            
            if(var == "amarelo") {

            }
            if(var == "azul") {
                
            }
            if(var == "verde") {
                
            }
            if(var == "vermelho") {
                
            }
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