library(R6)

Board <- R6Class("Board",
  
  public = list(


    uno_deck_check <- function(deck) {
      # Verifica se o baralho é uma lista
      if (!is.list(deck)) {
        stop("O baralho deve ser uma lista.")
      }
      
      # Define as cores válidas
      acceptable_colors <- ("vermelho", "amarelo", "verde", "azul", "preto")
      
      # Verifica se cada carta no baralho tem um número e uma cor
      for (card in deck) {
        if (!is.numeric(card$number)) {
          stop("Cada carta deve ter um número.")
        }
        if (!is.character(card$color)) {
          stop("Cada carta deve ter uma cor.")
        }
        if (!card$color %in% acceptable_colors) {
          stop(paste("A cor ", card$color, " não é válida."))
        }
      }
      
      # Verifica se o baralho tem o número correto de cartas
      card_number_expected <- 108
      if (length(deck) != card_number_expected) {
        stop(paste("O baralho deve ter ", card_number_expected, " cartas."))
      }
      
      # Retorna TRUE se o baralho for válido
      return(TRUE)
    }
  )
)