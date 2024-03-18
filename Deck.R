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

    
    generateDeck = function() {
      # Cores disponíveis
    cores <- c("azul", "vermelho", "verde", "amarelo")

    # Números disponíveis
    numeros <- 1:9
    numerosEspeciais <- 10:11

    # Lista para armazenar os objetos Cards
    objetos_cards <- list()

    # Loop para criar os objetos
    for (cor in cores) {
        for (num in numeros) {
            novo_card <- Cards$new(color = cor, number = num)
            objetos_cards[[paste(cor, num, sep = "_")]] <- novo_card
        }
    }

    for (cor in cores)
        for (num in numerosEspeciais) {
          novo_card <- Cards$new(color = cor, number = num)
          objetos_cards[[paste(cor, num, sep = "_")]] <- novo_card
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
      }

    }
  )

)
