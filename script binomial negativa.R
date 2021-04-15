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

