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
  "car",
  "hnp"
)

lapply(packages, loadlibrary) # carrega pacotes

dados <- read_excel("dados.xlsx")
### SELECIONANDO O ANO DE 2013 ####
dados_2013 <- subset(dados,ano == 2013)
attach(dados_2013)
###################################
### AJUSTE USANDO COMANDO GLM() ###
###################################

# require(MASS)
# fit.model<- glm.nb(dengue~.-ano-id-Municipio ,data=dados_2013)
# summary(fit.model)
# fit.model<- stepAIC(fit.model)
# par(mfrow=c(2,2))

###################################
### contruindo modelo com base  ###
### na descriçao dos dados      ###
### fornecidos pelo professor   ###

# require(MASS)
# fit.model_ad <- glm.nb(
#   dengue ~ offset(log(pop)) +
#     IntCdAtBca +
#     CobCondSaud +
#     CobAtencBsca +
#     temp +
#     log(precip) +
#     umid +
#     log(alt) +
#     ifdm_edu +
#     ifdm_emprend +
#     ifdm_saude +
#     cobveg +
#     expcosteira+
#     Pobr+
#     ExpAnosEstud+
#     urb+
#     adultos-
#     Municipio+
#     dens,
#   data = dados_2013)
# 
# summary(fit.model_ad)
# 
# fit.model_ad <- stepAIC(fit.model_ad)
# summary(fit.model_ad)
# #####Modelo com as var menor15 e maior65
# 
# require(MASS)
# fit.model_na <- glm.nb(
#   dengue ~ offset(log(pop)) +
#     IntCdAtBca +
#     CobCondSaud +
#     CobAtencBsca +
#     temp +
#     log(precip) +
#     umid +
#     log(alt) +
#     ifdm_edu +
#     ifdm_emprend +
#     ifdm_saude +
#     cobveg +
#     expcosteira+
#     Pobr+
#     ExpAnosEstud+
#     urb+
#     menor15+maior65-
#     Municipio+
#     dens,
#   data = dados_2013)
# 
# summary(fit.model_na)
# 
# fit.model_na <- stepAIC(fit.model_na)
# summary(fit.model_na)
# ###com base nos resultado o modelo com somente a variavel adultos foi melhor
# ###usando agora no lugar da variavel temp as variaveis temp_p10 e temp_p90
# 
# require(MASS)
# fit.model_px <- glm.nb(
#   dengue ~ offset(log(pop)) +
#     IntCdAtBca +
#     CobCondSaud +
#     CobAtencBsca +
#     temp_p10+
#     temp_p90+
#     log(precip) +
#     umid +
#     log(alt) +
#     ifdm_edu +
#     ifdm_emprend +
#     ifdm_saude +
#     cobveg +
#     expcosteira+
#     Pobr+
#     ExpAnosEstud+
#     urb+
#     adultos-
#     Municipio+
#     dens,
#   data = dados_2013)
# 
# summary(fit.model_px)
# 
# fit.model_px<- stepAIC(fit.model_px)
# summary(fit.model_px)
# ###com base nos resultado o modelo com somente a variavel temp foi melhor
# ##### usando agora as variaveis umid_p10 e umid_p90 no lugar da umid
# require(MASS)
# fit.model_ux <- glm.nb(
#   dengue ~ offset(log(pop)) +
#     IntCdAtBca +
#     CobCondSaud +
#     CobAtencBsca +
#     temp +
#     log(precip) +
#     umid_p10+
#     umid_p90+
#     log(alt) +
#     ifdm_edu +
#     ifdm_emprend +
#     ifdm_saude +
#     cobveg +
#     expcosteira+
#     Pobr+
#     ExpAnosEstud+
#     urb+
#     adultos-
#     Municipio+
#     dens,
#   data = dados_2013, 
#   control = glm.control(maxit = 50))
# 
# summary(fit.model_ux)
# 
# fit.model_ux<- stepAIC(fit.model_ux)
# summary(fit.model_ux)
# ###nesse caso,apareceu um erro
# ###Sendo assim, optamos por manter a variavel umid, temp e adultos no modelo
# ###que pode ser obtido abaixo
# require(MASS)
# fit.model_ad <- glm.nb(
#   dengue ~ offset(log(pop)) +
#     IntCdAtBca +
#     CobCondSaud +
#     CobAtencBsca +
#     temp +
#     log(precip) +
#     umid +
#     log(alt) +
#     ifdm_edu +
#     ifdm_emprend +
#     ifdm_saude +
#     cobveg +
#     expcosteira+
#     Pobr+
#     ExpAnosEstud+
#     urb+
#     adultos-
#     Municipio+
#     dens,
#   data = dados_2013, 
#   control = glm.control(maxit = 50))
# 
# summary(fit.model_ad)

require(MASS)
#Modelo mariana
fit.model_ad <- glm.nb(dengue~CobCondSaud +
                         CobAtencBsca +
                         temp_p90 + 
                         precip +
                         umid +
                         ifdm_saude +
                         ifdm_emprend +
                         cobveg +
                         expcosteira +
                         ivc + 
                         ExpAnosEstud +
                         urb + 
                         maior65 + 
                         adultos +
                         dens +
                         offset(log(pop)),
                       control = glm.control(maxit = 50), 
                       data = dados_2013
                       )

fit.model_ad <- stepAIC(fit.model_ad)
summary(fit.model_ad)

vif(fit.model_ad)

fit.model <- fit.model_ad

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
#identify(fitted(fit.model), h, n=1)
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
#identify(di, n=1)
############################
### PREDITOR LINEAR VS Z ### p/ verificar adequação da função de ligação
############################
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

# dev.new(width=6,height=3)
hnp(fit.model, 
    xlab = 'Percentil da N(0,1)',
    ylab = 'Resíduos', 
    main = 'Gráfico Normal de Probabilidades', 
    pch = 20, 
    cex.lab=1.5, 
    cex.axis=1.5
    )

# 
# e <- matrix(0,n,1000)
# #
# for(i in 1:1000){
# resp <- rnegbin(n, fitted(fit.model),fi)
# fit <- glm.nb(resp ~ X)
# w <- fit$weights
# W <- diag(w)
# H <- solve(t(X)%*%W%*%X)
# H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
# h <- diag(H)
# e[,i] <- sort(resid(fit,type="deviance")/sqrt(1-h))}
# #
# e1 <- numeric(n)
# e2 <- numeric(n)
# #
# for(i in 1:n){
#  eo <- sort(e[i,])
# e1[i] <- eo[25]
# e2[i] <- eo[975]}
# #
# med <- apply(e,1,mean)
# faixa <- range(td,e1,e2)
# #
# qqnorm(td, 
#        xlab="Percentil da N(0,1)", 
#        ylab="Componente do Desvio",
#        ylim=faixa,
#        cex.lab=1.5, 
#        cex.axis=1.5, 
#        pch=20,
#        main=""
#        )
# par(new=TRUE)
# #
# qqnorm(e1,axes=F,xlab="",ylab="",type="l",ylim=faixa,lty=1, main="")
# par(new=TRUE)
# qqnorm(e2,axes=F,xlab="",ylab="", type="l",ylim=faixa,lty=1, main="")
# par(new=TRUE)
# qqnorm(med,axes=F,xlab="", ylab="", type="l",ylim=faixa,lty=2, main="")

detach(dados_2013)


#####estudo dos pontos influentes nas linhas 42, 44, 74

attach(dados_2013)
###42=Laranja da terra
###44=Mantenopolis
###74=VIana
dados_2013<- dados_2013[-42,]
dados_2013<- dados_2013[-43,]
dados_2013<- dados_2013[-72,]
require(MASS)
#Modelo mariana

#com a remoçao o modelo acima obteve pontos (42, 44, 74 (dados_2013 original) 
#Residual deviance:  83.104  on 68  degrees of freedom
#AIC: 835.54

#sem a remocao o modelo acima obteve 
#Residual deviance:  87.661  on 71  degrees of freedom
#AIC: 895.02

#alguns pontos influentes aparecem apos a remoçao, no caso, o ponto 5
#removendo o mesmo obtemos o seguinte
dados_2013<- dados_2013[-5,]
#Residual deviance:  81.419  on 69  degrees of freedom
####ponto 22
dados_2013<- dados_2013[-22,]
#Residual deviance:  79.397  on 64  degrees of freedom
##AIC: 798.11

## sendo que o melhor ajuste foi até a remoçao do ponto 5
detach(dados_2013)

attach(dados_2013)
fit.model_ad <- glm.nb(dengue~CobCondSaud +
                         CobAtencBsca +
                         temp_p90 + 
                         precip +
                         umid +
                         ifdm_saude +
                         ifdm_emprend +
                         cobveg +
                         expcosteira +
                         ivc + 
                         ExpAnosEstud +
                         urb + 
                         maior65 + 
                         adultos +
                         dens +
                         offset(log(pop)),
                       control = glm.control(maxit = 50), 
                       data = dados_2013
)

fit.model_ad <- stepAIC(fit.model_ad)
summary(fit.model_ad)

fit.model <- fit.model_ad

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
#identify(fitted(fit.model), h, n=1)
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
identify(di, n=1)
############################
### PREDITOR LINEAR VS Z ### p/ verificar adequação da função de ligação
############################
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

# dev.new(width=6,height=3)
hnp(fit.model, 
    xlab = 'Percentil da N(0,1)',
    ylab = 'Resíduos', 
    main = 'Gráfico Normal de Probabilidades', 
    pch = 20, 
    cex.lab=1.5, 
    cex.axis=1.5
)

# 
# e <- matrix(0,n,1000)
# #
# for(i in 1:1000){
# resp <- rnegbin(n, fitted(fit.model),fi)
# fit <- glm.nb(resp ~ X)
# w <- fit$weights
# W <- diag(w)
# H <- solve(t(X)%*%W%*%X)
# H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
# h <- diag(H)
# e[,i] <- sort(resid(fit,type="deviance")/sqrt(1-h))}
# #
# e1 <- numeric(n)
# e2 <- numeric(n)
# #
# for(i in 1:n){
#  eo <- sort(e[i,])
# e1[i] <- eo[25]
# e2[i] <- eo[975]}
# #
# med <- apply(e,1,mean)
# faixa <- range(td,e1,e2)
# #
# qqnorm(td, 
#        xlab="Percentil da N(0,1)", 
#        ylab="Componente do Desvio",
#        ylim=faixa,
#        cex.lab=1.5, 
#        cex.axis=1.5, 
#        pch=20,
#        main=""
#        )
# par(new=TRUE)
# #
# qqnorm(e1,axes=F,xlab="",ylab="",type="l",ylim=faixa,lty=1, main="")
# par(new=TRUE)
# qqnorm(e2,axes=F,xlab="",ylab="", type="l",ylim=faixa,lty=1, main="")
# par(new=TRUE)
# qqnorm(med,axes=F,xlab="", ylab="", type="l",ylim=faixa,lty=2, main="")
detach(dados_2013)

##sem remoçao 
# Residual deviance:  87.661  on 71  degrees of freedom
# AIC: 895.02
#uma remocao
# Residual deviance:  86.277  on 70  degrees of freedom
# AIC: 878.78
#duas remocoes
# Residual deviance:  84.598  on 70  degrees of freedom
# AIC: 858.05
#tres remocoes
# Residual deviance:  83.104  on 68  degrees of freedom
# AIC: 835.54
#quatro remocoes
# Residual deviance:  81.419  on 69  degrees of freedom
# AIC: 809.81
# 5 remocoes
# Residual deviance:  79.397  on 64  degrees of freedom
# AIC: 798.11



#### testando o modelo da mariana com as variaveis ao quadrado

require(MASS)
#Modelo mariana
fit.model_ad <- glm.nb(dengue~CobCondSaud +
                         CobAtencBsca +
                         temp_p90 + 
                         precip +
                         umid +
                         ifdm_saude +
                         ifdm_emprend +
                         cobveg +
                         expcosteira +
                         ivc + 
                         ExpAnosEstud +
                         urb + 
                         maior65 + 
                         adultos +
                         dens +
                         offset(log(pop)),
                       control = glm.control(maxit = 50), 
                       data = dados_2013
)

fit.model_ad <- stepAIC(fit.model_ad)
summary(fit.model_ad)

vif(fit.model_ad)

fit.model <- fit.model_ad