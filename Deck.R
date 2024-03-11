library(R6)

Deck <- R6Class("Deck",
  
  public = list(
    
    cards = NULL,

    initialize = function() {

        # Aqui deixei só um exemplo, tentar formular pra ele criar as cartas automaticamente
        self$cards <- matrix(c(
            "reverse", 11,
            "azul", 7,
            "vermelho", 11,
            "verde", 3,
            "amarelo", 8
        ), ncol = 2, byrow = TRUE)

        for (cor in c("azul", "vermelho", "verde", "amarelo")) {
           
          for (num in 1:9) {
            


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

    }

  )

)
