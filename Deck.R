library(R6)
source("Cards.R")

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
    },

    generateDeck = function() {
     # Cores disponíveis
  cores <- c("azul", "vermelho", "verde", "amarelo")
  especiais <- c("+4", "mudaCor")

  # Números disponíveis
  numeros <- 1:9
  numeros_Especiais <- 10:11

  # Definindo a estrutura da matriz
  numero_linhas <- 4 + length(especiais) * length(numeros_Especiais)
  numero_colunas <- 2

  # Criando a matriz vazia
  matriz_cartas <- matrix(data = numeric(numero_linhas * numero_colunas), nrow = numero_linhas, ncol = numero_colunas)

  # Linha atual na matriz
  linha_atual <- 1

  # Loop para criar todas as cartas
  for (cor in cores) {
    for (num in numeros) {
      novo_card <- Cards$new(color = cor, number = num)
      matriz_cartas[linha_atual, 1] <- novo_card$color
      matriz_cartas[linha_atual, 2] <- novo_card$number
      linha_atual <- linha_atual + 1
    }
  }

  # Adicionando cartas especiais
  for (especial in especiais) {
    for (num in numeros_Especiais) {
      novo_card <- Cards$new(color = especial, number = num)
      matriz_cartas[linha_atual, 1] <- novo_card$color
      matriz_cartas[linha_atual, 2] <- novo_card$number
      linha_atual <- linha_atual + 1
    }
  }

  # Retornando a matriz de cartas
  return(matriz_cartas)
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
