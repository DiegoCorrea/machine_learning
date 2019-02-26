dados <- read.table("~/Downloads/jogar-tenis.csv", sep=";", header=T)

naive_bayes <- function(dados, teste, class){
  prob_class = table(class)/length(class)
  resp <- c(1,1)
  for(i in 1:length(prob_class)) {
    subset_dados = dados[which(class == names(prob_class)[i]),]
    for(j in 1:(ncol(teste)-1)){
      resp[i] = (length(which(subset_dados[,j] == teste[,j]))/prob_class[i])*resp[i]
    }
    resp[i] = resp[i]*prob_class[i]
  }
  resp/sum(resp)
}
class = dados[1:13,c(6)]
base = dados[1:13,-c(1)]
test = dados[c(14),-c(1,6)]
print(naive_bayes(dados = base, teste = test, class = class))

