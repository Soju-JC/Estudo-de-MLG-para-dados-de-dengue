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
  "ggcorrplot"
)

lapply(packages, loadlibrary) # carrega pacotes

# carrega dados
dados <- read_excel("dados.xlsx")
dados_2013 <- subset(dados,ano == 2013)
dados_2013 <- dados_2013[,-c(28,29)]
#colunas 28 e 29 são referentes a "id" e "ano" reespectivamente.
#View(dados_2013)
attach(dados_2013)
############################ Descritiva ###################################

############ Notificações de dengue por municipio ############

dev.new(width=6,height=3)
ggplot(dados_2013,aes(x = Municipio, y = dengue)) + 
  geom_histogram(fill = rgb(0.3,0.7,0.5,0.9), stat= "identity") + 
  labs(x = "Municipio", y = "\nNotificações de dengue") + 
  coord_flip() + 
  scale_y_continuous(expand = c(0,.5),
                     limits = c(0,25000),
                     breaks = seq(0,25000,2500)) + 
  theme(axis.text.x  = element_text(angle=45, hjust = 1)) +
  scale_x_discrete(expand = c(0,.5)) 

################## Grafico de correlação ############3##3###

corr <- round(cor(dados_2013[,-1]), 1)

dev.new(width=6,height=3)
ggcorrplot(corr, hc.order = TRUE, type = "upper", outline.col = "white", lab = TRUE)

################ Boxplots dos índices sociais ################

#Município de vitória não foi plotado pois possui notificações de
#dengue muito extremas em comparação com os demais municípios. Além
#disso, Vitória está abaixo da média dos municípios quanto ao índice
#IntCdAtBca, embora isso não seja verdade nos índices CobCondSaud e
#CobAtencBsca.

summary(IntCdAtBca) #mean = 34
summary(CobCondSaud) #mean = 76
summary(CobAtencBsca) #median = 100
#criar tabela com descrição das estatísticas básicas das 3.

             ########## IntCdAtBca ##########

# Cria IntCdAtBca_indice
dados_2013$IntCdAtBca_indice <- 
  ifelse(IntCdAtBca < 34, 1, 0)

dados_2013$IntCdAtBca_indice <- 
  fct_recode(as.factor(dados_2013$IntCdAtBca_indice), 
             "sim" = "1",
             "não" = "0")

#Boxplot IntCdAtBca_indice e dengue
dev.new(width=6,height=3)
ggplot(dados_2013[-78,], 
       aes(x = IntCdAtBca_indice,
           y = dengue,
           group = IntCdAtBca_indice, 
           fill = IntCdAtBca_indice)
       ) + 
geom_boxplot(notch = FALSE) + 
scale_y_continuous(breaks = seq(0,20500,500)) + 
labs(y = "Notificações de dengue\n",
     x = "\nIntCdAtBca menor do que a média (< 34) ?") + 
guides(fill = FALSE) #+ 
#geom_dotplot(binaxis = 'y',
# stackdir = 'center',
# binwidth = 1,
# dotsize = 0.5
# ) 
             
             
             ########## CobCondSaud ##########

#Cria CobCondSaud_indice
dados_2013$CobCondSaud_indice <- 
  ifelse(CobCondSaud < 76, 1, 0)

dados_2013$CobCondSaud_indice <- 
  fct_recode(as.factor(dados_2013$CobCondSaud_indice), 
             "sim" = "1",
             "não" = "0")

#Boxplot CobCondSaud_indice e dengue
dev.new(width=6,height=3)
ggplot(dados_2013[-78,], 
       aes(x = CobCondSaud_indice,
           y = dengue,
           group = CobCondSaud_indice, 
           fill = CobCondSaud_indice)
) + 
  geom_boxplot(notch = FALSE) + 
  scale_y_continuous(breaks = seq(0,20500,500)) + 
  labs(y = "Notificações de dengue\n",
       x = "\nCobCondSaud menor do que a média (< 76) ?") + 
  guides(fill = FALSE) #+ 
#geom_dotplot(binaxis = 'y',
# stackdir = 'center',
# binwidth = 1,
# dotsize = 0.5
# ) 

             

            ########## CobAtencBsca ##########

#Cria CobAtencBsca_indice
dados_2013$CobAtencBsca_indice <- 
  ifelse(CobAtencBsca < 100, 1, 0)

dados_2013$CobAtencBsca_indice <- 
  fct_recode(as.factor(dados_2013$CobAtencBsca_indice), 
             "sim" = "1",
             "não" = "0")

#Boxplot CobAtencBsca_indice e dengue
dev.new(width=6,height=3)
ggplot(dados_2013[-78,], 
       aes(x = CobAtencBsca_indice,
           y = dengue,
           group = CobAtencBsca_indice, 
           fill = CobAtencBsca_indice)
) + 
  geom_boxplot(notch = FALSE) + 
  scale_y_continuous(breaks = seq(0,20500,500)) + 
  labs(y = "Notificações de dengue\n",
       x = "\nCobAtencBsca menor do que a mediana (< 100) ?") + 
  guides(fill = FALSE) #+ 
#geom_dotplot(binaxis = 'y',
# stackdir = 'center',
# binwidth = 1,
# dotsize = 0.5
# ) 

##############################################################

detach(dados_2013)