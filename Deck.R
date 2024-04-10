library(R6)
source("Cards.R")

Deck <- R6Class("Deck",
  
  public = list(
    
    cards = NULL,

    initialize = function() {

      self$cards <- matrix(c(
        "Azul", "Reverse"
        ,"Azul", "Reverse"
        ,"Verde", "Reverse"
        ,"Verde", "Reverse"
        ,"Vermelho", "Reverse"
        ,"Vermelho", "Reverse"
        ,"Amarelo", "Reverse"
        ,"Amarelo", "Reverse"
        ,"Azul", "Block"
        ,"Azul", "Block"
        ,"Verde", "Block"
        ,"Verde", "Block"
        ,"Vermelho", "Block"
        ,"Vermelho", "Block"
        ,"Amarelo", "Block"
        ,"Amarelo", "Block"
        ,"Azul", "+2"
        , "Azul", "+2"
        ,"Verde", "+2"
        ,"Verde", "+2"
        ,"Vermelho", "+2"
        ,"Vermelho", "+2"
        ,"Amarelo", "+2"
        ,"Amarelo", "+2"
        ,"Preto", "+4"
        #,"Preto"#, "+4"
        #,"Preto"#, "+4"
        #,"Preto"#, "+4"
        #,"Preto"#, "trocaCor"
        #,"Preto"#, "trocaCor"
        #,"Preto"#, "trocaCor"
        #,"Preto"#, "trocaCor"
      ), ncol = 2, byrow = TRUE)
    },
    
    generateDeck = function() {
      # Cores disponíveis
      cores <- c("Azul", "Vermelho", "Verde", "Amarelo") 

      # Números disponíveis
      numeros <- 0:9

      # Loop para criar todas as cartas
      for (i in 1:2) {
        for (cor in cores) {
          for (num in numeros) {
              
              nova_linha <- matrix(c(
                cor, num
              ), ncol = 2, byrow = TRUE)

              self$cards <- rbind(self$cards, nova_linha)

          }
        }
      }
    },
    
    shuffleDeck = function() {

      self$cards <- self$cards[sample(nrow(self$cards)), ]
          
    },

    toDistributeCards = function(player_1 = NA, player_2 = NA, player_3 = NA, player_4 = NA) {

      players <- c(player_1, player_2, player_3, player_4);
        
      for(player in players){
      
        cartas <- self$cards[1:7, ]

        player$cards <- cartas

        for(i in 1:7){
          self$cards <- self$cards[-1,]
        }
        
      }
    },

    setInitialCard = function(){

      startercard <- self$cards[1, ]

      if (startercard[2] == 10 || startercard[2] == 11 || startercard[2] == 12 || startercard[2] == 13 || startercard[2] == 14) {
        
        self$shuffleDeck()
        self$setInitialCard()
        
      } else {
        
        self$cards <- self$cards[-1,]
        return(startercard)

      }

    },

    reStackDeck = function(discardStack = NA) {
      
      if(nrow(self$cards) == 0) {
        self$cards <- discardStack
        # Descobrir como implementar!!! - (testar com loop removendo carta a carta)
        #pilhaDescarte <- pilhaDescarte[0:-nrow(pilhaDescarte),]
      }
    }
    
  )
)