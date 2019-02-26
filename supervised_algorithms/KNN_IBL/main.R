library(datasets)
data(iris)

euclidianDistance <- function(point_1, point_2) {
  sqrt(sum((point_1-point_2)^2))
}

knn_classifier <- function(train, test, k=1, n_col_label = 5) {
  train_labels = train[,n_col_label]
  train = train[,-c(n_col_label)]
  test = test[,-c(n_col_label)]
  dist = c()
  for(i in 1:nrow(train)){
    dist = c(dist, euclidianDistance(train[i,], test))
  }
  tail(names(sort(table(train_labels[sort.list(dist)[1:k]]))), 1)
}

knn_regression <- function(train, test, k=1, n_col_label = 4) {
  train_labels = train[,n_col_label]
  train = train[,-c(n_col_label)]
  test = test[,-c(n_col_label)]
  dist = c()
  for(i in 1:nrow(train)){
    dist = c(dist, euclidianDistance(train[i,], test))
  }
  train_labels[sort.list(dist)[1:k]]
}

# ################################################################################### #
# ################################### start main #################################### #
# ################################################################################### #
print(knn_classifier(train = iris[1:149,], test = iris[150,], k = 3, n_col_label = 5))

