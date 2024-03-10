library(R6)

Deck <- R6Class("Deck",
  
  public = list(
    
    cards = NULL,

    initialize = function() {

        # Aqui deixei só um exemplo, tentar formular pra ele criar as cartas automaticamente
        self$cards <- matrix(c(
            "azul", 7,
            "vermelho", 11,
            "verde", 3,
            "amarelo", 8
        ), ncol = 2, byrow = TRUE)

    },
    
    shuffle = function() {

      self$cards <- self$cards[sample(nrow(self$cards)), ]
          
    },

    toDistributecards = function(player_1 = NA, player_2 = NA, player_3 = NA, player_4 = NA) {

        # Aqui fico no aguardo da classe Mão do Jogador para usar o método de inicialização dele para atribuir as cartas iniciais

    }

  )

)

deck <- Deck$new("Alyssandro", "Joana", "João", "Tássio")

