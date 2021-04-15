rm(list = ls())
loadlibrary <- function(x){
  
  if (!require(x,character.only = TRUE)) {
    install.packages(x,dependencies = T)
    if(!require(x, character.only = TRUE)) 
      stop("Package not found")
  }
}

packages <- c(
  "tidyverse",
  "readxl",
  "janitor",
  "skimr",
  "lubridate",
  "summarytools",
  "magrittr", 
  "pipeR",
  "knitr",
  "viridis",
  "cowplot",
  "tidyr",
  "reshape2",
  "VIM",
  "mice",
  "VGAM",
  "nlme",
  "visreg",
  "lme4",
  "glmnet",
  "leaps",
  "glmmLasso",
  "glmmTMB",
  "mgcv",
  "writexl",
  "car"
)

lapply(packages, loadlibrary) # carrega pacotes

# carrega dados
data <- read_excel("dados.xlsx")


###################################
### AJUSTE USANDO COMANDO GLM() ###
###################################

attach(data)

cor(data[2:28],data[30])


modelo_completo<- glm(dengue~IntCdAtBca + CobCondSaud + 
                        CobAtencBsca + temp_p10 + temp + temp_p90 + precip +
                        umid_p10 + umid + umid_p90 + alt + ifdm_saude + 
                        ifdm_edu + ifdm_emprend + cobveg + expcosteira + ivc +
                        Pobr + ExpAnosEstud + urb + menor15 + maior65 + 
                        adultos + pop + area + dens,family=poisson,data=data)
summary(modelo_completo)

### modelo meramente formal, abaixo temos o que vamos seguir devido correlação


modelo_corr<- glm(dengue~CobCondSaud + CobAtencBsca + temp_p90 + precip +
                        umid + ifdm_saude + ifdm_emprend + cobveg + 
                        expcosteira + ivc + ExpAnosEstud + 
                        urb + maior65 + adultos + dens,
                        family=poisson,data=data)

modelo_corr_pop<- glm(dengue~CobCondSaud + CobAtencBsca + temp_p90 + precip +
                    umid + ifdm_saude + ifdm_emprend + cobveg + 
                    expcosteira + ivc + ExpAnosEstud + 
                    urb + maior65 + adultos + pop,
                  family=poisson,data=data)

### comparando os dois modelos acima ficamos com o com a variavel dens pelo 
### menor aIC e desvio residual e adicionamos o offset pop

modelo_corr_offset<- glm(dengue~CobCondSaud + CobAtencBsca + temp_p90 + 
                           precip + umid + ifdm_saude + ifdm_emprend + cobveg +
                           expcosteira + ivc + ExpAnosEstud + urb + maior65 + 
                           adultos + dens + offset(log(pop)),
                         family=poisson,data=data)

modelo_corr_offset_pop<- glm(dengue~CobCondSaud + CobAtencBsca + temp_p90 + 
                           precip + umid + ifdm_saude + ifdm_emprend + cobveg +
                           ivc + ExpAnosEstud + urb + maior65 + 
                           adultos + offset(log(pop)),
                         family=poisson,data=data)


summary(modelo_corr_offset)

modelo_offset<-glm(dengue~offset(log(pop)) + IntCdAtBca + CobCondSaud + 
         CobAtencBsca + temp_p10 + temp + temp_p90 + precip +
         umid_p10 + umid + umid_p90 + alt + ifdm_saude + 
         ifdm_edu + ifdm_emprend + cobveg + expcosteira + ivc +
         Pobr + ExpAnosEstud + urb + menor15 + maior65 + 
         adultos +  area + dens,family=poisson,data=data)


require(MASS)
modelo_selecionado<- stepAIC(modelo_completo)
modelo_selecionado_offset<- stepAIC(modelo_offset)
modelo_selecionado_corr<- stepAIC(modelo_corr)

modelo_selecionado_corr_offset<-stepAIC(modelo_corr_offset)### melhor modelo
summary(modelo_selecionado_corr_offset)
vif(modelo_selecionado_corr_offset)


fit.model<-modelo_selecionado_corr_offset
par(mfrow=c(2,2))
### PREPARANDO OS GRÁFICOS
X <- model.matrix(fit.model)
n <- nrow(X)
p <- ncol(X)
w <- fit.model$weights
W <- diag(w)
H <- solve(t(X)%*%W%*%X)
H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
h <- diag(H)
ts <- resid(fit.model,type="pearson")/sqrt(1-h)
td <- resid(fit.model,type="deviance")/sqrt(1-h)
di <- (h/(1-h))*(ts^2)
corte.hii<- 2*p/n # corte para elementos da diagonal de H
corte.cook<- qf(0.5,p,n-p) # corte para Distância de Cook
#############################
### ALAVACAGEM / LEVERAGE ###
#############################
plot(fitted(fit.model), 
     h, 
     xlab="Valor Ajustado", 
     ylab="Medida h", 
     cex.lab=1.5, 
     cex.axis=1.5, 
     ylim=c(0,1), 
     pch=20
     )

lines(c(0,max(fitted(fit.model))+1), 
      c(corte.hii,corte.hii), 
      col='red',
      lty=2
      )
# identify(fitted(fit.model), h, n=12)
#########################
### PONTOS INFLUENTES ###
#########################
plot(di, 
     type="h", 
     cex.lab=1.5, 
     cex.axis=1.5, 
     xlab="Observação", 
     ylab="Distância de Cook", 
     ylim=c(0,max(max(di),corte.cook))
     )

lines(c(0,n+1), 
      c(corte.cook,corte.cook), 
      col='red', 
      lty=2 
      )
# identify(di, n=6)
############################
### PREDITOR LINEAR VS Z ### p/ verificar adequação da função de ligação
############################
w <- fit.model$weights
eta <- predict(fit.model)
z <- eta + resid(fit.model, type="pearson")/sqrt(w)

plot(predict(fit.model), 
     z, 
     xlab="Preditor Linear", 
     cex.lab=1.5, 
     cex.axis=1.5, 
     ylab="Variavel z", 
     pch=20
     )

lines(smooth.spline(predict(fit.model), z, df=2))
################
### ENVELOPE ###
################
e <- matrix(0,n,1000)
#
for(i in 1:1000){
  nresp <- rpois(n, fitted(fit.model))
  fit <- glm(nresp ~ X, family=poisson)
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
qqnorm(td, 
       xlab="Percentil da N(0,1)",
       ylab="Componente do Desvio",
       ylim=faixa, 
       cex.lab=1.5, 
       cex.axis=1.5,
       pch=20,
       main=""
       )
par(new=TRUE)
#
qqnorm(e1,axes=F,xlab="",ylab="",type="l",ylim=faixa,lty=1, main="")
par(new=TRUE)
qqnorm(e2,axes=F,xlab="",ylab="", type="l",ylim=faixa,lty=1, main="")
par(new=TRUE)
qqnorm(med,axes=F,xlab="", ylab="", type="l",ylim=faixa,lty=2, main="")

### retirei observações 78, 74 e 19

modelo_corr_offset2<- glm(dengue~CobCondSaud + CobAtencBsca + temp_p90 + 
                           precip + umid + ifdm_saude + ifdm_emprend + cobveg +
                           expcosteira + ivc + ExpAnosEstud + urb + maior65 + 
                           adultos + dens + offset(log(pop)),
                         family=poisson,data=data2)

modelo_selecionado_corr_offset2<-stepAIC(modelo_corr_offset2)###
summary(modelo_selecionado_corr_offset2)
vif(modelo_selecionado_corr_offset2)


fit.model2<-modelo_selecionado_corr_offset2
par(mfrow=c(2,2))
### PREPARANDO OS GRÁFICOS
X <- model.matrix(fit.model2)
n <- nrow(X)
p <- ncol(X)
w <- fit.model2$weights
W <- diag(w)
H <- solve(t(X)%*%W%*%X)
H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
h <- diag(H)
ts <- resid(fit.model2,type="pearson")/sqrt(1-h)
td <- resid(fit.model2,type="deviance")/sqrt(1-h)
di <- (h/(1-h))*(ts^2)
corte.hii<- 2*p/n # corte para elementos da diagonal de H
corte.cook<- qf(0.5,p,n-p) # corte para Distância de Cook
#############################
### ALAVACAGEM / LEVERAGE ###
#############################
plot(fitted(fit.model2), 
     h, 
     xlab="Valor Ajustado", 
     ylab="Medida h", 
     cex.lab=1.5, 
     cex.axis=1.5, 
     ylim=c(0,1), 
     pch=20 
     )

lines(c(0,max(fitted(fit.model2))+1), 
      c(corte.hii,corte.hii), 
      col='red', 
      lty=2
      )
identify(fitted(fit.model2), h, n=12)
#########################
### PONTOS INFLUENTES ###
#########################
plot(di, 
     type="h", 
     cex.lab=1.5, 
     cex.axis=1.5, 
     xlab="Observação", 
     ylab="Distância de Cook", 
     ylim=c(0,max(max(di),corte.cook))
     )

lines(c(0,n+1),c(corte.cook,corte.cook),col='red',lty=2)
identify(di, n=3)
############################
### PREDITOR LINEAR VS Z ### p/ verificar adequação da função de ligação
############################
w <- fit.model2$weights
eta <- predict(fit.model2)
z <- eta + resid(fit.model2, type="pearson")/sqrt(w)

plot(predict(fit.model2), 
     z, 
     xlab="Preditor Linear", 
     cex.lab=1.5, 
     cex.axis=1.5, 
     ylab="Variavel z", 
     pch=20
     )

lines(smooth.spline(predict(fit.model2), z, df=2))
################
### ENVELOPE ###
################
e <- matrix(0,n,1000)
#
for(i in 1:1000){
  nresp <- rpois(n, fitted(fit.model2))
  fit <- glm(nresp ~ X, family=poisson)
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
qqnorm(td, 
       xlab="Percentil da N(0,1)",
       ylab="Componente do Desvio", 
       ylim=faixa,
       cex.lab=1.5, 
       cex.axis=1.5,
       pch=20, 
       main=""
       )
par(new=TRUE)
#
qqnorm(e1,axes=F,xlab="",ylab="",type="l",ylim=faixa,lty=1, main="")
par(new=TRUE)
qqnorm(e2,axes=F,xlab="",ylab="", type="l",ylim=faixa,lty=1, main="")
par(new=TRUE)
qqnorm(med,axes=F,xlab="", ylab="", type="l",ylim=faixa,lty=2, main="")

detach(data)