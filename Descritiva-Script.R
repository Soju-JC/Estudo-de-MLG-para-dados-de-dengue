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
############################ Descritiva ###################################

####### Notificações de dengue por municipio #######
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

dev.new(width=6,height=3)
plot(dados_2013$CobAtencBsca, dados_2013$dengue)