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
dados <- read_excel("dados.xlsx")
dados_2013 <- subset(dados,ano == 2013)
dados_2013 <- dados_2013[,-c(26,28,29)]
#colunas 26,28 e 29 são referentes a "area","id" e "ano" reespectivamente.
View(dados_2013)

############################ Ajuste poisson  ###############################

attach(dados_2013)
fit <- glm(dengue~. -Municipio,family=poisson,data=dados_2013)
summary(fit)
# Desvio residual muito diferente dos graus de liberdade, indica
#possível superdispersão.

vif(fit)
# vif aponta que existe "aliased coefficients", o que indica que
#existe alguma covariável no modelo com multicolinearidade perfeita.

attributes(alias(fit)$Complete)$dimnames[[1]]
# Identificamos que a covariável adultos possui multicolinearidade
#perfeita, logo, iremos excluí-la do modelo.

fit <- glm(dengue~. -Municipio -adultos,family=poisson,data=dados_2013)
summary(fit)
vif(fit)
# As covariáveis de umidade e temperatura estão com 
#multicolinearidade muito alta, logo, vamos considerar os modelos
#separados. Além disso, note que existe muitas covariáveis com 
#vif > 10.

#### Modelo com as medidas médias de temperatura(temp) e umidade(umid) ####
fit1 <- glm(dengue~. -Municipio -adultos -temp_p10 -temp_p90 -umid_p10 -umid_p90,
            family=poisson,
            data=dados_2013
            )

vif(fit1)

#### Modelo com as medidas extremas(temp_p10,temp_p90 e umid_p10,umid_p90####
fit2 <- glm(dengue~. -Municipio -adultos -temp -umid, 
            family=poisson, 
            data=dados_2013 
            )
vif(fit2)

#O vif das medidas extremas continuam com valores muito grandes, 
#então vamos dar continuidade com o modelo 1 (fit1). 

#Ainda precisamos lidar com as covariáveis com vif > 10, caso contrário
#teremos problema de singularidade (matrizes não inversíveis por serem LD) 
#ao criar gráficos de diagnósticos. Iremos remover 1 por 1 da com maior 
#multicolinearidade para a menor até obter todas covariáveis restantes 
#com vif < 10.

fit1 <- glm(dengue~. -Municipio -adultos -temp_p10 -temp_p90 -umid_p10 -umid_p90 -menor15 -dens -pop -expcosteira -Pobr -ifdm_saude, 
            family=poisson, 
            data=dados_2013
            )
#As seguintes covariáveis foram retiradas do modelo na seguinte ordem:
#menor15 > dens > pop > expcosteira > Pobr > ifdm_saude
vif(fit1)

fit1<- stepAIC(fit1)
summary(fit1)
# Nenhuma das covariáveis consideradas foi removida pelo critério AIC. 

############################ Diagnóstico ################################
         ################### rodar daqui ######################
fit.model <- fit1  

dev.new(width=6,height=3)
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
corte.hii<- 2*p/n # corte para elementos da diagonal de
H
corte.cook<- qf(0.5,p,n-p) # corte para Distância de Cook
#############################
### ALAVACAGEM / LEVERAGE ###
#############################
plot(fitted(fit.model), 
     h, 
     xlab = "Valor Ajustado", 
     ylab = "Medida h", 
     cex.lab = 1.5, 
     cex.axis = 1.5, 
     ylim = c(0,1), 
     pch = 20
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

lines(c(0,n+1), 
      c(corte.cook,corte.cook), 
      col='red', 
      lty=2
      )

############################
### PREDITOR LINEAR VS Z ### 
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
qqnorm(e1, 
       axes=F, 
       xlab="", 
       ylab="", 
       type="l", 
       ylim=faixa, 
       lty=1,
       main=""
       )
par(new=TRUE)
qqnorm(e2, 
       axes=F, 
       xlab="", 
       ylab="", 
       type="l", 
       ylim=faixa, 
       lty=1, 
       main=""
       )
par(new=TRUE)
qqnorm(med, 
       axes=F, 
       xlab="", 
       ylab="",
       type="l", 
       ylim=faixa, 
       lty=2,
       main=""
       )

          ################### Até aqui ######################

# Podemos observar pelo gráfico de envelope que o modelo está terrivelmente
#ajustado, somado com a informação inicial que observamos do desvio residual
#extremamente distante dos graus de liberdade, tudo indica que a poisson não
#é uma boa candidata para o problema, logo, devemos testar ajustar um modelo
#com a distribuição binomial negativa.

detach(dados_2013)