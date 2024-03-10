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

    }
    
    suffle = function() {

      self$cards <- self$cards[sample(nrow(self$cards)), ]
          
    }

  )

)

deck <- Deck$new()

