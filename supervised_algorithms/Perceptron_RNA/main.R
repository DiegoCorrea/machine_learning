f.u <- function(train, weight, baias) {
  sum(train*weight)+baias
}

activation <- function(u, trashRoad) {
  out = 1
  if (u < trashRoad)
    out = -1
  out
}

weightActualization <- function(weight_vec, learn_value, value) {
  w + learn_value*(esperado - obtido)*value
}

baiasActualization <- function(){
  
}

RNA <- function(data, rot, t = 0, learn_value = 0.4, iter = 100) {
  w = rep(0,ncol(data))
  baias = 1
  for(i in 1:iter) {
    train = sample(1:nrow(data),1)
    u = f.u(data[train,], w, baias)
    act =  activation(u, t)
    if (act != rot[train]) {
      w = w + learn_value*(rot[train] - act)*data[train,]
      baias = baias + learn_value*(rot[train] - act)
    }
  }
  result = list()
  result$w = w
  result$baias = baias
  result
}

data = matrix(c(0,0,1,1,1,0), nrow=2, byrow=T)
rotulo = c(-1,1)
print(data)
print(RNA(data, rotulo))
