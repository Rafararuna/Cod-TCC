# Pacotes:

library(tidyverse)
library(xtable)
library(dplyr)
library(plyr)
library(corrplot)
library(corrgram)
library(ggplot2)
library(gridExtra)
library(readr)
library(readxl)
library(lawstat)
library(lmtest)
library(car)
library(EnvStats)
library(olsrr)
library(nortest)
library(PMCMRplus)
library(MASS)
library(leaps)


# Banco de dados:

banco_tcc <-  read_excel("C:/Users/Ermida/Downloads/TCC1 - RAFAEL ARARUNA/Relat�rio Parcial/base tcc1 - vvr - base limpa pelo python.xlsx")
banco_tcc <-  read_excel("C:/Users/jgararuna/Downloads/TCC/base tcc1 - vvr - base limpa pelo python.xlsx")


# Definindo vari�veis:

BAIRRO <- as.factor(banco_tcc$BAIRRO)
AREA <- banco_tcc$�REA
QUARTO <- banco_tcc$QUARTO
BANHEIRO <- banco_tcc$BANHEIRO
SUITE <- banco_tcc$SU�TE
VAGA <- banco_tcc$VAGA
VALOR <- banco_tcc$VALOR
VALOR_M2 <- banco_tcc$VALORM2

#Definindo banco treino e banco valida��o

set.seed(180026798) 
amostra <- sample(1:359, size = 200, replace = F)
base.treino <- banco_tcc[amostra,]
base.valida <- banco_tcc[-amostra,]

BAIRRO_t <- as.factor(base.treino$BAIRRO)
AREA_t <- base.treino$�REA
QUARTO_t <- base.treino$QUARTO
BANHEIRO_t <- base.treino$BANHEIRO
SUITE_t <- base.treino$SU�TE
VAGA_t <- base.treino$VAGA
VALOR_t <- base.treino$VALOR
VALORM2_t <- base.treino$VALORM2
n_t <- length(VALOR_t)

BAIRRO_v <- base.valida$BAIRRO
AREA_v <- base.valida$�REA
QUARTO_v <- base.valida$QUARTO
BANHEIRO_v <- base.valida$BANHEIRO
SUITE_v <- base.valida$SU�TE
VAGA_v <- base.valida$VAGA
VALOR_v <- base.valida$VALOR
VALORM2_v <- base.valida$VALORM2
n_v <- length(VALOR_v)



#########################################################



# Cruzamento das variaveis com VALOR (para verificar a necessidade de categoriza��o)


## AREA COM VALOR

cor(AREA_t,VALOR_t) # 0.95
plot(AREA_t,VALOR_t) # correla��o positiva forte


## QUARTO COM VALOR

cor(QUARTO_t,VALOR_t) # 0.79
plot(QUARTO_t,VALOR_t) # correla��o positiva forte

table(QUARTO_t) # temos poucos imoveis com 4 e 5 quartos, assim, vale a pena pensar em uma padroniza��o
                # talvez fazer duas categorias: <=2 / >2 

base.treino$QUARTO <- as.factor(base.treino$QUARTO)
tapply(base.treino$VALOR, base.treino$QUARTO, mean) # aqui tamb�m pode-se sugerir uma categoriza��o do tipo: <=2 / >2


## BANHEIRO COM VALOR

cor(as.numeric(BANHEIRO_t),VALOR_t) # 0.82
plot(BANHEIRO_t,VALOR_t) # correla��o positiva forte

table(BANHEIRO_t) # nota-se poucas observa��es do valor 4 pra cima
                  # assim, � v�lido repensar em uma categoriza��o do tipo: <=2 / >2

base.treino$BANHEIRO <- as.factor(base.treino$BANHEIRO)
tapply(base.treino$VALOR, base.treino$BANHEIRO, mean) # aqui talvez fazer tr�s categorias: 1e2/3e4/5a7
                                                      # ou fazer duas categorias: <=3 / >3

## SUITE COM VALOR

cor(SUITE_t,VALOR_t) # 0.55
plot(SUITE_t,VALOR_t) # correla��o positiva moderada

table(SUITE_t) # nota-se poucas observa��es com 2 ou mais suites
               # talvez seja valido categorizar da seguinte forma: <1 / >=1

base.treino$SU�TE <- as.factor(base.treino$SU�TE)
tapply(base.treino$VALOR, base.treino$SU�TE, mean) # pelos resultados, d� pra categorizar da seguinte forma: <=2 / >2 
                                                   # testar de forma n�o binaria e depois binaria

## VAGA COM VALOR

cor(VAGA_t,VALOR_t) # 0.72
plot(VAGA_t,VALOR_t) # correla��o positiva forte

table(VAGA_t) # nota-se poucas observa��es com 2 ou mais vagas
              # talvez seja valido categorizar da seguinte forma: <1 / >=1

base.treino$VAGA <- as.factor(base.treino$VAGA)
tapply(base.treino$VALOR, base.treino$VAGA, mean) # pelos resultados, d� pra categorizar da seguinte forma: <=1 / >1  
                                                  # testar de forma n�o bin�ria e depois bin�ria


#DIANTE DOS RESULTADOS, VIMOS QUE FAZ SENTIDO REALIZAR UMA CATEGORIZA��O NESSAS VARI�VEIS



##################################################################



# Banco categorizado:

banco_treino_cat <- base.treino
banco_treino_cat$QUARTO_cat <- ifelse(banco_treino_cat$QUARTO <= 2, "<=2 quartos", ">2 quartos")
banco_treino_cat$BANHEIRO_cat <- ifelse(banco_treino_cat$BANHEIRO <= 3, "<=3 banheiros", ">3 banheiros")
banco_treino_cat$SUITE_cat <- ifelse(banco_treino_cat$SU�TE <= 1, "<=1 su�te", ">1 su�te")
banco_treino_cat$VAGA_cat <- ifelse(banco_treino_cat$VAGA <= 1, "<=1 vaga", ">1 vaga")

banco_treino_cat$QUARTO_cat <- as.factor(banco_treino_cat$QUARTO_cat)
banco_treino_cat$BANHEIRO_cat <- as.factor(banco_treino_cat$BANHEIRO_cat)
banco_treino_cat$SUITE_cat <- as.factor(banco_treino_cat$SUITE_cat)
banco_treino_cat$VAGA_cat <- as.factor(banco_treino_cat$VAGA_cat)
banco_treino_cat$BAIRRO_cat <- as.factor(banco_treino_cat$BAIRRO)



#####################################################################



# 1� Categoriza��o: suite e vaga sem serem binarias

# Construindo o modelo completo categorizado e sem trasnforma��o na variavel y
# obs: com suite e vaga sem serem binarias

mod.treino <- lm(VALOR_t~�REA+QUARTO_cat+BANHEIRO_cat+SUITE_cat+VAGA_cat+BAIRRO_cat, data = banco_treino_cat)

a <- summary(mod.treino) # considerando um alpha de 5%, quarto, banheiro e bairro n�o apresentaram signific�ncia
xtable::xtable(a)

anova(mod.treino)


# �nalise de diagn�stico do mod.treino:

par(mfrow=c(1,2))

plot(mod.treino$fitted.values,mod.treino$residuals,pch=16, 
     xlab = "Valores ajustados",
     ylab = "Res�duos", col = 4)
abline(h=0,col=2)
abline(h=-4e+05,col=2)
abline(h=4e+05,col=2)
identify(mod.treino$fitted.values,mod.treino$residuals) # encontra-se 9 outliers, cujas observa��es s�o:  5   8  11  28  33  37  55 149 173

plot(mod.treino$residuals,pch=16, xlab = "", ylab = "Res�duos", col = 4)
abline(h=0,col=2)
abline(h=-4e+05,col=2)
abline(h=4e+05,col=2)
identify(mod.treino$residuals) # encontra-se 9 outliers, cujas observa��es s�o:  5   8  11  28  33  37  55 149 173


par(mfrow=c(2,4))


plot(banco_treino_cat$�REA,mod.treino$residuals,pch=16,xlab = "�rea", ylab = "Res�duos", col=4)
abline(h=0,col=2)
abline(h=-4e+05,col=2)
abline(h=4e+05,col=2)
identify(banco_treino_cat$�REA,mod.treino$residuals) # encontra-se 9 outliers, cujas observa��es s�o:  5   8  11  28  33  37  55 149 173

plot(banco_treino_cat$QUARTO_cat,mod.treino$residuals,pch=16,xlab = "Quarto", 
     ylab = "Res�duos", col=4)
abline(h=0,col=2)
abline(h=-4e+05,col=2)
abline(h=4e+05,col=2)
identify(banco_treino_cat$QUARTO_cat,mod.treino$residuals) # encontra-se 6 outliers, cujas observa��es s�o:  8  28  33  37  55 179

plot(banco_treino_cat$BANHEIRO_cat,mod.treino$residuals,pch=16,xlab = "Banheiro", 
     ylab = "Res�duos", col=4)
abline(h=0,col=2)
abline(h=-4e+05,col=2)
abline(h=4e+05,col=2)
identify(banco_treino_cat$BANHEIRO_cat,mod.treino$residuals) # encontra-se 7 outliers, cujas observa��es s�o:  5   8  28  33  37  55 149

plot(banco_treino_cat$SUITE_cat,mod.treino$residuals,pch=16,xlab = "Su�te",
     ylab = "Res�duos", col=4)
abline(h=0,col=2)
abline(h=-4e+05,col=2)
abline(h=4e+05,col=2)
identify(banco_treino_cat$SUITE_cat,mod.treino$residuals) # encontra-se 5 outliers, cujas observa��es s�o:  5   8  28  33 149

plot(banco_treino_cat$VAGA_cat,mod.treino$residuals,pch=16,xlab = "Vaga", 
     ylab = "Res�duos", col=4)
abline(h=0,col=2)
abline(h=-4e+05,col=2)
abline(h=4e+05,col=2)
identify(banco_treino_cat$VAGA_cat,mod.treino$residuals) # encontra-se 9 outliers, cujas observa��es s�o:  5   8  11  28  33  37  55 149 173


# Verificando normalidade:

par(mfrow=c(1,1))

stud_treino <- rstudent(mod.treino)

hist(stud_treino, xlab = "y", ylab = "Frequ�ncia", main = "", col = 4) # graficamente, aparente ser simetrico em torno do zero
                                                                       # porem, nota-se alguns valores distante, provavelmente s�o outliers, os quais podem prejudicar o pressuposto da normalidade

qqnorm(mod.treino$residuals,col=4,xlab = "Quantis Te�ricos",  ylab = "Quantis Amostrais", main = "")
qqline(mod.treino$residuals,col=2) # por esse gr�fico, � poss�vel notar que muitas observa��es (a maioria delas) est�o em cima da reta. 
                                   # Dessa forma, h� ind�cios de que os dados s�o normais. Por�m, tem-se pontos que estao fora da reta,
                                   # alguns deles muitos distantes, o que pode resultar em uma n�o normalidade

ols_plot_resid_qq(mod.treino)


shapiro.test(mod.treino$residuals) # Por meio desse teste, � poss�vel notar um p-valor menor que 5% (nivel de confian�a considerado), 
                                   # portanto h� evid�ncias suficientes para rejeitar h0, ouseja, os dados n�o possuem uma distribui��o
                                   # normal. Provavelmente por causa dos outliers que vimos nos graficos

# Verificando independencia dos erros:

dwtest(mod.treino) # Por meio desse teste, nota-se um p-valor maior que 5% (nivel de confian�a considerado), portanto n�o h� evid�ncias 
                   # suficientes para rejeitar h0, ou seja, os dados s�o independentes.

# Verificando homogeneidade da vari�ncia:

bptest(mod.treino) # Por meio desse teste, nota-se um p-valor menor que 5% (nivel de confian�a considerado), portanto h� evid�ncias 
                   # suficientes para rejeitar h0, ou seja, os dados s�o n�o possuem vari�ncia homog�nea.


# TALVEZ FAZER UM NOVO MODELO SEM OS OUTLIERS PRA VER SE TODOS OS PRESSUPOSTOS PASSAM!!



############################################################



# Verificando necessidade de transforma��o na vari�vel y:

boxcox(mod.treino,lambda = seq(-2,2,by=0.5), 
       ylab = "Log-Verossimilhan�a") #lambda =~ 0.5 



#############################################################



# Sele��o de vari�veis

# Para ver qual o n�mero melhor de vari�veis pra entrar no modelo:

k <- ols_step_all_possible(mod.treino)
plot(k) # analisando os gr�ficos das medidas que penalizam a quantidade de vari�veis (CP, AIC, BIC e o SBC), percebe-se que
        # as quantidade de vari�veis para entrar no modelos 2, 3, 4 e 5 ficaram com valores muito pr�ximos


# Ver quais os melhores modelos pra cada quantidade de vari�veis selecionada anteriormente:

base.treino_teste <- banco_treino_cat[,c(9,14,16,17,18,19,20)]

sele1 <- regsubsets(base.treino_teste$VALOR~., data=base.treino_teste, nbest = 10)
summary(sele1) 

cbind(summary(sele1)$which,summary(sele1)$rsq,summary(sele1)$adjr2, summary(sele1)$cp,summary(sele1)$bic)
# Nota-se que os melhores modelos, para cada quantidade de vari�veis selecionada anteriormente, s�o:
# > com 2 vari�veis: VALOR_t = INTERCEPTO + AREA + VAGA_cat
# > com 3 vari�veis: VALOR_t = INTERCEPTO + AREA + SUITE_cat + VAGA_cat 
# > com 4 vari�veis: VALOR_t = INTERCEPTO + AREA + QUARTO_cat + SUITE_cat + VAGA_cat 
# > com 5 vari�veis: VALOR_t = INTERCEPTO + AREA + QUARTO_cat + BANHEIRO_cat + SUITE_cat + VAGA_cat  

BAIRRO_cat <- base.treino_teste$BAIRRO_cat
AREA <- base.treino_teste$�REA
QUARTO_cat <- base.treino_teste$QUARTO_cat
BANHEIRO_cat <- base.treino_teste$BANHEIRO_cat
SUITE_cat <- base.treino_teste$SUITE_cat
VAGA_cat <- base.treino_teste$VAGA_cat
VALOR <- base.treino_teste$VALOR
n <- length(VALOR)


# An�lise de diagn�stico do modelo com 2 vari�veis

mod_2 <- lm(VALOR~AREA + VAGA_cat)
a <- summary(mod_2)
xtable::xtable(a)

# Res�duo exclu�do studentizado

stud_2 <- rstudent(mod_2)

par(mfrow=c(1,2))

plot(mod_2$fitted.values,stud_2,pch=16, xlab = "y ajustado",
     ylab = "Res�duo studentizado", col=4)
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)
identify(mod_2$fitted.values,stud_2) # nota-se a presen�a de 11 outliers, cujas observa��es s�o: 5   8  11  28  33  37  55 121 149 173 179

plot(stud_2,pch=16, col=4, ylab = "Res�duo studentizado", xlab = "")
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)
identify(stud_2) # nota-se a presen�a de 11 outliers, cujas observa��es s�o: 5   8  11  28  33  37  55 121 149 173 179

# Verificando normalidade:

par(mfrow=c(1,1))

hist(stud_2, xlab = "Res�duo studentizado", ylab = "Frequ�ncia", main = "", col = 4) # aparenta ser sim�trico em torno do zero, apresentando
                                                                                     # ind�cios de que os dados s�o normais. Por�m, nota-se alguns valores mais distantes, provavelmente outliers,
                                                                                     # os quais podem prejudicar o pressuposto da normalidade

qqnorm(stud_2,col=4,xlab = "Quantis Te�ricos", ylab = "Quantis Amostrais",
       main = "")
qqline(stud_2,col=2)

ols_plot_resid_qq(mod_2) # nota-se que a maioria dos pontos est�o sobre a reta, indicando que os dados, provavelmente, s�o normais.
                         # porem, h� pontos que est�o muito distantes da reta, podendo prejudicar a valida��o desse pressuposto

shapiro.test(mod_2$residuals) # nota-se um p-valor menor que o n�vel de significancia de 5%, portanto, h� evidencias suficientes para
                              # rejeitar a hipotese nula, ou seja, os dados n�o s�o normais

# Verificando independ�ncia dos erros:

dwtest(mod_2) # nota-se um p-valor maior que o n�vel de significancia de 5%, portanto, n�o h� evidencias suficientes para
              # rejeitar a hipotese nula, ou seja, os erros s�o independentes

# Verificando a homogeneidade da vari�ncia:

bptest(mod_2) # nota-se um p-valor menor que o n�vel de significancia de 5%, portanto, h� evidencias suficientes para
              # rejeitar a hipotese nula, ou seja, as vari�ncias n�o s�o iguais


# Multicolinearidade: (PERGUNTAR PRO PROFESSOR)

g <- (vi_2 <- vif(mod_2))
xtable::xtable(g)
mean(vi_2)

# Verificando observa��es influentes:

# DFBETAS
# O dfbeta sao pontos que estao tendo uma influencia acima 
# da desejada em rela��o a estima��o daquele parametro, daquele beta.

ols_plot_dfbetas(mod_2) # nota-se a presen�a de alguns valores influentes, alguns dele se repetem em todo grafico, outros em quase todos eles
                        # vale a pena pensar em tirar eles do conjunto de dados


# DFCOOK
# O dcook � uma medida mais geral, a gente v� a influencia no valor ajustado geral

plot(mod_2,which=4) 

ols_plot_cooksd_chart(mod_4) # nota-se tambem alguns valores influentes, alguns deles s�o os mesmo que apareceram no DFBTEAS



# Res�duo exclu�do - para calcular o PRESS: (PERGUTAR PRO PROFESSOR)

medinflu_2 <- influence.measures(mod_2)
indice <- c(1:n)

rexc_2 <- mod_2$residuals/(1-medinflu_2$infmat[,7])
plot(indice,rexc_2)

PRESS_2 <- sum((rexc_2)^2)

rstandard(mod_2,type="predictive")
sum((rstandard(mod_2,type="predictive"))^2) # isso � o erro de previs�o, o erro equivalente ao PRESS


# An�lise de diagn�stico do modelo com 3 vari�veis

mod_3 <- lm(VALOR~AREA + SUITE_cat + VAGA_cat)


# An�lise de diagn�stico do modelo com 4 vari�veis

mod_4 <- lm(VALOR~AREA + QUARTO_cat + SUITE_cat + VAGA_cat)

# An�lise de diagn�stico do modelo com 5 vari�veis

mod_5 <- lm(VALOR~AREA + QUARTO_cat + BANHEIRO_cat + SUITE_cat + VAGA_cat)
