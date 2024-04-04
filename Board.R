library(R6)

Board <- R6Class("Board",
  public = list(
    Winner = FALSE,
    DiscartStack = NULL,
    DiscartStackTop = NULL,

    initialize = function(initialCardColor = NA, initialCardNumber = NA) {
      self$DiscartStack <- matrix(c(
        initialCardColor, initialCardNumber
      ), ncol = 2, byrow = TRUE)
    },

    verifyTopDiscartStack = function() {
      self$DiscartStackTop <- self$DiscartStack[nrow(self$DiscartStack),]
    },

    addCardToDiscartStack = function(carta = NA){
      self$DiscartStack <- rbind(self$DiscartStack, carta)
      self$verifyTopDiscartStack()
    }

  )
)
