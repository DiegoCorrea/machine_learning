dados<-matrix(nrow=10, ncol=2)
dados[,1]<-c(2.5, 0.5, 2.2, 1.9, 3.1, 2.3, 2., 1.0, 1.5, 1.1)
dados[,2]<-c(2.4, 0.7, 2.9, 2.2, 3.0, 2.7, 1.6, 1.1, 1.6, 0.9)

dados[,1]<-dados[,1]-mean(dados[,1])
dados[,2]<-dados[,2]-mean(dados[,2])

covmat<-cov(dados)

autovec<-eigen(covmat)$vectors

comps<-t(t(autovec)%*%t(dados))
# Uso dos pacotes em R
data(iris)
str(iris)
head(iris)
plot(iris)
fit<-prcomp(iris[,-5],scale=TRUE, center=TRUE)
summary(fit)$importance[3,3]
fit$x[,1:2]

plot(fit,type="lines")
cbind(fit$x[,1:2], iris[,5])
biplot(fit)
