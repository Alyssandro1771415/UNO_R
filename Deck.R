library(R6)
source("Cards.R")

Deck <- R6Class("Deck",
  
  public = list(
    
    cards = NULL,

    initialize = function() {

      self$cards <- matrix(c(
        "azul", 0,
        "amarelo", 0,
        "vermelho", 0,
        "verde", 0,
        "reverse Azul", 10,
        "reverse Azul", 10,
        "reverse Verde", 10,
        "reverse Verde", 10,
        "reverse Vermelho", 10,
        "reverse Vermelho", 10,
        "reverse Amarelo", 10,
        "reverse Amarelo", 10,
        "block Azul", 11,
        "block Azul", 11,
        "block Verde", 11,
        "block Verde", 11,
        "block Vermelho", 11,
        "block Vermelho", 11,
        "block Amarelo", 11,
        "block Amarelo", 11,
        "+2 Azul", 12,
        "+2 Azul", 12,
        "+2 Verde", 12,
        "+2 Verde", 12,
        "+2 Vermelho", 12,
        "+2 Vermelho", 12,
        "+2 Amarelo", 12,
        "+2 Amarelo", 12,
        "+4", 13,
        "+4", 13,
        "+4", 13,
        "+4", 13,
        "trocaCor", 14,
        "trocaCor", 14,
        "trocaCor", 14,
        "trocaCor", 14
      ), ncol = 2, byrow = TRUE)
    },
    
    generateDeck = function() {
      # Cores disponíveis
      cores <- c("azul", "vermelho", "verde", "amarelo")

      # Números disponíveis
      numeros <- 1:9

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

        players <- c(player_1$cards, player_2$cards, player_3$cards, player_4$cards)

        for (i in 1:7) {
    
          carta <- self$cards[i, ]

          players[[i]]$cards <- rbind(players[[i]]$cards, carta)

          self$cards <- self$cards[-i, ]

        }

    },

    setInitialCard = function(){

      startercard <- self$cards[1, ]

      if (startercard[2] == 10 || startercard[2] == 11) {
        
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
        # Descobrir como implementar!!!
        #pilhaDescarte <- pilhaDescarte[0:-nrow(pilhaDescarte),]
      }
    }
    
  )
)