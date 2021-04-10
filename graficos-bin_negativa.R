rm(list=ls(all=TRUE))
dados<- read.table("clipboard",header=TRUE)
attach(dados)
######################################
### AJUSTE USANDO COMANDO GLM.NB() ###
######################################
require(MASS)
fit.model<- glm.nb(nass~.,data=dados)
summary(fit.model)
fit.model<- stepAIC(fit.model)
par(mfrow=c(2,2))
### PREPARANDO OS GRÁFICOS
X <- model.matrix(fit.model)
n <- nrow(X)
p <- ncol(X)
fi <- fit.model$theta
#w <- fi*fitted(fit.model)/(fi + fitted(fit.model))
w <- fit.model$weights
W <- diag(w)
H <- solve(t(X)%*%W%*%X)
H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
h <- diag(H)
ts <- resid(fit.model,type="pearson")/sqrt(1-h)
td <- resid(fit.model,type="deviance")/sqrt(1-h)
di <- (h/(1-h))*(ts^2)
corte.hii<- 2*p/n # corte para elementos da diagonal de
H
corte.cook<- qf(0.5,p,n-p) # corte para Distância de Cook
#############################
### ALAVACAGEM / LEVERAGE ###
#############################
plot(fitted(fit.model), h,xlab="Valor Ajustado", ylab="Medida
h",cex.lab=1.5,cex.axis=1.5,
     ylim=c(0,1),pch=20)
lines(c(0,max(fitted(fit.model))+1),c(corte.hii,corte.hii),col='red',
      lty=2)
#identify(fitted(fit.model), h, n=1)
#########################
### PONTOS INFLUENTES ###
#########################
plot(di,type="h",cex.lab=1.5,cex.axis=1.5,xlab="Observação",ylab="Dist
ância de Cook",ylim=c(0,max(max(di),corte.cook)))
lines(c(0,n+1),c(corte.cook,corte.cook),col='red',lty=2)
#identify(di, n=1)
############################
### PREDITOR LINEAR VS Z ### p/ verificar adequação da função de
ligação
############################
eta <- predict(fit.model)
z <- eta + resid(fit.model, type="pearson")/sqrt(w)
plot(predict(fit.model),z,xlab="Preditor
Linear",cex.lab=1.5,cex.axis=1.5, 
ylab="Variavel z", pch=20)
lines(smooth.spline(predict(fit.model), z, df=2))
################
### ENVELOPE ###
################
e <- matrix(0,n,1000)
#
for(i in 1:1000){
resp <- rnegbin(n, fitted(fit.model),fi)
fit <- glm.nb(resp ~ X)
w <- fit$weights
W <- diag(w)
H <- solve(t(X)%*%W%*%X)
H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
h <- diag(H)
e[,i] <- sort(resid(fit,type="deviance")/sqrt(1-h))}
#
e1 <- numeric(n)
e2 <- numeric(n)
#
for(i in 1:n){
 eo <- sort(e[i,])
e1[i] <- eo[25]
e2[i] <- eo[975]}
#
med <- apply(e,1,mean)
faixa <- range(td,e1,e2)
#
qqnorm(td,xlab="Percentil da N(0,1)",
ylab="Componente do Desvio", ylim=faixa, cex.lab=1.5,cex.axis=1.5,
pch=20, main="")
par(new=TRUE)
#
qqnorm(e1,axes=F,xlab="",ylab="",type="l",ylim=faixa,lty=1, main="")
par(new=TRUE)
qqnorm(e2,axes=F,xlab="",ylab="", type="l",ylim=faixa,lty=1, main="")
par(new=TRUE)
qqnorm(med,axes=F,xlab="", ylab="", type="l",ylim=faixa,lty=2,
main="")