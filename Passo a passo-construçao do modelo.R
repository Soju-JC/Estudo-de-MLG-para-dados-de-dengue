Passos básicos para a construção de um modelo de Regressão:
  
1 - Definir o modelo (verificar a literatura da área dos dados)
2 - Considerar todas as covariáveis (sem termos ao quadrado ou interações 2 a 2);
3 - Verificar Multicolineaidade (caso necessário, remover ou combinar covariáveis);
4 - Seleção de covariáveis, agora considerando seus quadrados e/ou interações 2 a 2, quando adequado (Recomendo Backward-AIC, mas fica a seu critério);
5 - Transformar covariáveis não selecionadas e verificar se as mesmas passam a ser significativas após transformação;
6 - Verificar pontos Influentes/de Alavanca/Outliers (caso necessário, remover os pontos);
7 - Análise de Resíduos (Verificação dos Pressupostos)
7.1 - Normalidade
7.2 - Homoscedasticidade
7.3 - Erros não-correlacionados




Comandos referentes à cada passo:
  
  dados<- read.table("clipboard",header=TRUE)	# leitura dos dados

fit<- lm(y~...,data=dados)			# ajuste do modelo
summary(fit)


#####  ######################
# 3 #  # MULTICOLINEARIDADE #
#####  ######################

require(car)		# o comando vif() esta dentro deste pacote
vif(fit)		# verifique se há algum valor acima de 10


#####  ##########################################
# 6 #  # PONTOS INFLUENTES/DE ALAVANCA/OUTLIERS #
#####  ##########################################

n<- nrow(dados)
k<- length(fit$coef) 		# k=p+1 (número de coeficientes)

corte.hii<- 2*k/n		# corte para elementos da diagonal de H
corte.cook<- qf(0.5,k,n-k)	# corte para Distância de Cook
corte.pad<- 2			# corte para resíduos padronizados

rpd<- rstandard(fit)		# resíduos padronizados
rst<- rstudent(fit)		# resíduos estudentizados

par(mfrow=c(2,2))

plot(hatvalues(fit),type="h",cex.lab=1.5,cex.axis=1.5,xlab="Observação",ylab="Leverage",ylim=c(0,max(max(hatvalues(fit)),corte.hii)))
lines(c(0,n+1),c(corte.hii,corte.hii),col='red',lty=2)
plot(cooks.distance(fit),type="h",cex.lab=1.5,cex.axis=1.5,xlab="Observação",ylab="Distância de Cook",ylim=c(0,max(max(cooks.distance(fit)),corte.cook)))
lines(c(0,n+1),c(corte.cook,corte.cook),col='red',lty=2)
plot(rpd,pch=20,cex.lab=1.5,cex.axis=1.5,xlab="Observação",ylab="Resíduos Padronizados",ylim=c(min(-corte.pad,min(rpd)),max(corte.pad,max(rpd))))
lines(c(0,n+1),c(corte.pad,corte.pad),col='red',lty=2)
lines(c(0,n+1),c(-corte.pad,-corte.pad),col='red',lty=2)
boxplot(rpd,pch=20,cex.lab=1.5,cex.axis=1.5,xlab="Resíduos Padronizados")

order(hatvalues(fit),decreasing=TRUE)[1:10]		# Maiores Alavancagens
order(cooks.distance(fit),decreasing=TRUE)[1:10]	# Maiores Influências
order(abs(rpd),decreasing=TRUE)[1:10]			# Maiores Resíduos



#######  ###############
# 7.1 #  # NORMALIDADE #
#######  ###############

require(nortest)

t1 <- ks.test(rst,"pnorm")	# KS
t2 <- lillie.test(rst)		# Lilliefors
t3 <- cvm.test(rst)		# Cramer-von Mises
t4 <- shapiro.test(rst)		# Shapiro-Wilk
t5 <- sf.test(rst)		# Shapiro-Francia
t6 <- ad.test(rst)		# Anderson-Darling

# Tabela de resultados
testes <- c(t1$method, t2$method, t3$method, t4$method, t5$method,t6$method)
estt <- as.numeric(c(t1$statistic, t2$statistic, t3$statistic,
                     t4$statistic, t5$statistic, t6$statistic))
valorp <- c(t1$p.value, t2$p.value, t3$p.value, t4$p.value, t5$p.value,t6$p.value)
resultados <- cbind(estt, valorp)
rownames(resultados) <- testes
colnames(resultados) <- c("Estatística", "p")
print(resultados, digits = 4)



#######  ######################
# 7.2 #  # HOMOSCEDASTICIDADE #
#######  ######################

# VERIFICANDO SE HÁ HETEROSCEDASTICIDADE

require(lmtest)

bptest(fit)			# Breusch-Pagan Test



# CASO HAJA HETEROSCEDASTICIDADE DEVEMOS:
# (1) TRANSFORMAR OS DADOS PARA ELIMINA-LA; OU
# (2) INCORPORA-LA AO MODELO COM OS COMANDOS ABAIXO

require(gamlss)

fit.gamlss<- gamlss(y~..., sigma.formula = ~ ..., data=na.omit(dados))
summary(fit.gamlss)




#######  #############################
# 7.3 #  # ERROS NÃO-CORRELACIONADOS #
#######  #############################

# VERIFICANDO SE HÁ CORRELAÇÃO SERIAL

require(car)

durbinWatsonTest(fit)		# Durbin-Watson Test



# CASO HAJA AUTOCORRELAÇÃO DEVEMOS:
# INCORPORA-LA AO MODELO COM OS COMANDOS ABAIXO

require(nlme)

fit.gls <- gls(y~..., data=dados, correlation=corARMA(p=1)) # AR(1)

summary(fit.gls)