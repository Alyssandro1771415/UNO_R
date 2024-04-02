library(R6)
source("Board.R")

PlayerHand <- R6Class("PlayerHand",
    public = list(
        cards = NULL,
        initialize = function(){
            self$cards <- matrix(ncol = 2, nrow = 0)
            colnames(self$cards) <- c("Tipo","Cor")
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
      # falta implementar o tabuleiro pra verificar a carta do topo
      self$verifyWin()

      if (verifyWin == TRUE) {
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
           #quem saber printar UNO
           return(FALSE)
        }
        else {
           return(FALSE)
        }   
    }
)
)