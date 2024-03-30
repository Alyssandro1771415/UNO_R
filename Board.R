library(R6)

Board <- R6Class("Board",
  public = list(
    Winner = FALSE,
    DiscartStack = NULL,
    DiscartStackTop = NULL,


    initialize = function() {
      self$DiscartStack <- matrix(c(
        "azul", 0,
        "amarelo", 0,
        "vermelho", 0,
        "verde", 0
      ), ncol = 2, byrow = TRUE)
    },

    verifyTopDiscartStack = function() {
      self$DiscartStackTop <- self$DiscartStack[nrow(self$DiscartStack),]
    }

  )
)
