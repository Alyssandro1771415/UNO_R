library(R6)

Cards <- R6Class("Cards",

    public = list(

        color = NULL,
        number = NULL,

        initialize = function(color = NA, number = NA) {
         self$color <- color
         self$number <- number
        }


    





    )
)