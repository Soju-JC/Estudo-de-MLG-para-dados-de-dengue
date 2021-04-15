rm(list=ls(all=TRUE))
require('readxl')
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

require(MASS)
fit.model_ad <- glm.nb(
  dengue ~ offset(log(pop)) +
    IntCdAtBca +
    CobCondSaud +
    CobAtencBsca +
    temp +
    log(precip) +
    umid +
    log(alt) +
    ifdm_edu +
    ifdm_emprend +
    ifdm_saude +
    cobveg +
    expcosteira+
    Pobr+
    ExpAnosEstud+
    urb+
    adultos-
    Municipio+
    dens,
  data = dados_2013)
summary(fit.model_ad)
fit.model_ad<- stepAIC(fit.model_ad)
summary(fit.model_ad)
#####Modelo com as var menor15 e maior65

require(MASS)
fit.model_na <- glm.nb(
  dengue ~ offset(log(pop)) +
    IntCdAtBca +
    CobCondSaud +
    CobAtencBsca +
    temp +
    log(precip) +
    umid +
    log(alt) +
    ifdm_edu +
    ifdm_emprend +
    ifdm_saude +
    cobveg +
    expcosteira+
    Pobr+
    ExpAnosEstud+
    urb+
    menor15+maior65-
    Municipio+
    dens,
  data = dados_2013)
summary(fit.model_na)
fit.model_na<- stepAIC(fit.model_na)
summary(fit.model_na)
###com base nos resultado o modelo com somente a variavel adultos foi melhor
###usando agora no lugar da variavel temp as variaveis temp_p10 e temp_p90

require(MASS)
fit.model_px <- glm.nb(
  dengue ~ offset(log(pop)) +
    IntCdAtBca +
    CobCondSaud +
    CobAtencBsca +
    temp_p10+
    temp_p90+
    log(precip) +
    umid +
    log(alt) +
    ifdm_edu +
    ifdm_emprend +
    ifdm_saude +
    cobveg +
    expcosteira+
    Pobr+
    ExpAnosEstud+
    urb+
    adultos-
    Municipio+
    dens,
  data = dados_2013)
summary(fit.model_px)
fit.model_px<- stepAIC(fit.model_px)
summary(fit.model_px)
###com base nos resultado o modelo com somente a variavel temp foi melhor
##### usando agora as variaveis umid_p10 e umid_p90 no lugar da umid
require(MASS)
fit.model_ux <- glm.nb(
  dengue ~ offset(log(pop)) +
    IntCdAtBca +
    CobCondSaud +
    CobAtencBsca +
    temp +
    log(precip) +
    umid_p10+
    umid_p90+
    log(alt) +
    ifdm_edu +
    ifdm_emprend +
    ifdm_saude +
    cobveg +
    expcosteira+
    Pobr+
    ExpAnosEstud+
    urb+
    adultos-
    Municipio+
    dens,
  data = dados_2013)
summary(fit.model_ux)
fit.model_ux<- stepAIC(fit.model_ux)
summary(fit.model_ux)
###nesse caso, observando os resultados, é possivel verificar que 
###nao ha tanta diferença entre esse modelo acima, e o modelo
###com somente a variavel umid
###Sendo assim, optamos por manter a variavel umid, temp e adultos no modelo
###que pode ser obtido abaixo
require(MASS)
fit.model_ad <- glm.nb(
  dengue ~ offset(log(pop)) +
    IntCdAtBca +
    CobCondSaud +
    CobAtencBsca +
    temp +
    log(precip) +
    umid +
    log(alt) +
    ifdm_edu +
    ifdm_emprend +
    ifdm_saude +
    cobveg +
    expcosteira+
    Pobr+
    ExpAnosEstud+
    urb+
    adultos-
    Municipio+
    dens,
  data = dados_2013)
summary(fit.model_ad)
fit.model_ad<- stepAIC(fit.model_ad)
summary(fit.model_ad)
