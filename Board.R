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

    setTopDiscartStack = function(carta = NA){

      self$DiscartStackTop <- carta

    },

    addCardToDiscartStack = function(carta = NA){
      self$DiscartStack <- rbind(self$DiscartStack, carta)
      self$verifyTopDiscartStack()
    },

    verifyWin = function(quantCards){
      if (class(quantCards) == "NULL"){
        print("---!Vitŕoia!---")
        Sys.sleep(5)
        self$Winner <- TRUE
      }
      if (quantCards == 1) {
        print("UNO !!!")
        Sys.sleep(5)
        self$Winner <- FALSE
      }
      else {
        self$Winner <- FALSE
      }   
    }
  )
)
