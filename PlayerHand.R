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

        playCard = function(playerHand, DiscartStackTop) {

            self$verifyWin()

            if (self$verifyWin() == TRUE) {

                stop("O jogo terminou!")
            }

            var <- as.integer(readLine("Digite o número da linha: "))
            DiscartStackTop <- playerHand[-var]
            return(list(DiscartStackTop = DiscartStackTop, playerHand = playerHand))
        },

        blockCard = function(){
            
        },

        reverseCard = function(){
            
        },

        changeColor = function(){

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