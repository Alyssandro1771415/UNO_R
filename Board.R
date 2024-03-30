library(R6)

Board <- R6Class("Board",
  public = list(
    Winner = FALSE,
    DiscartStack = NULL,
    DiscartStackTop = NULL,


    initialize = function(initialcard = NA) {
      self$DiscartStack <- matrix(c(
        initialcard
      ), ncol = 2, byrow = TRUE)
    },

    verifyTopDiscartStack = function() {
      self$DiscartStackTop <- self$DiscartStack[nrow(self$DiscartStack),]
    }

  )
)
