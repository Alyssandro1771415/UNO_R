library(R6)

Deck <- R6Class(
  "Deck",
  public = list(
    
    cards = NULL,
    
    initialize = function() {
      
      self$cards <- rbind(
        c("azul", 7),
        c("vermelho", 11),
        c("verde", 3),
        c("amarelo", 8)
      )
      
      self$cards <- sample(self$cards)
      print(self$cards)
    
    }
  )
)

deck <- Deck$new()
