library(R6)
source("Board.R")
source("Deck.R")
source("PlayerHand.R")

baralho <- Deck$new()
#baralho$generateDeck()
#baralho$shuffleDeck()
tabuleiro <- Board$new(baralho$setInitialCard())

pilhaDescarte <- matrix(c(

    "+2 Verde", 12,
    "+2 Vermelho", 12,
    "+2 Vermelho", 12,
    "+2 Amarelo", 12,
    "+2 Amarelo", 12,
    "+4", 13,
    "+4", 13

), ncol=2, byrow=TRUE)

baralho$cards <- baralho$cards[-1,]

print(baralho$cards) #Vazio

while(tabuleiro$Winner == FALSE){

    baralho$reStackDeck(pilhaDescarte)
    print(baralho$cards)
    print("-------------------==")
    print(pilhaDescarte)

    #print(tabuleiro$verifyTopDiscartStack())
    break

}
