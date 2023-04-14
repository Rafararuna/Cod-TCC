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

banco_tcc <- read_excel("C:/Users/aluno/Downloads/base tcc1 - vvr - base limpa pelo python.xlsx")
banco_tcc <-  read_excel("C:/Users/jgararuna/Downloads/TCC/base tcc1 - vvr - base limpa pelo python.xlsx")


# Definindo variáveis:

BAIRRO <- as.factor(banco_tcc$BAIRRO)
AREA <- banco_tcc$ÁREA
QUARTO <- banco_tcc$QUARTO
BANHEIRO <- banco_tcc$BANHEIRO
SUITE <- banco_tcc$SUÍTE
VAGA <- banco_tcc$VAGA
VALOR <- banco_tcc$VALOR
VALOR_M2 <- banco_tcc$VALORM2

#Definindo banco treino e banco validação

set.seed(180026798) 
amostra <- sample(1:359, size = 200, replace = F)
base.treino <- banco_tcc[amostra,]
base.valida <- banco_tcc[-amostra,]

BAIRRO_t <- as.factor(base.treino$BAIRRO)
AREA_t <- base.treino$ÁREA
QUARTO_t <- base.treino$QUARTO
BANHEIRO_t <- base.treino$BANHEIRO
SUITE_t <- base.treino$SUÍTE
VAGA_t <- base.treino$VAGA
VALOR_t <- base.treino$VALOR
VALORM2_t <- base.treino$VALORM2
n_t <- length(VALOR_t)

BAIRRO_v <- base.valida$BAIRRO
AREA_v <- base.valida$ÁREA
QUARTO_v <- base.valida$QUARTO
BANHEIRO_v <- base.valida$BANHEIRO
SUITE_v <- base.valida$SUÍTE
VAGA_v <- base.valida$VAGA
VALOR_v <- base.valida$VALOR
VALORM2_v <- base.valida$VALORM2
n_v <- length(VALOR_v)



#########################################################



# Cruzamento das variaveis com VALOR (para verificar a necessidade de categorização)


## AREA COM VALOR

cor(AREA_t,VALOR_t) # 0.95
plot(AREA_t,VALOR_t) # correlação positiva forte


## QUARTO COM VALOR

cor(QUARTO_t,VALOR_t) # 0.79
plot(QUARTO_t,VALOR_t) # correlação positiva forte

a <- table(QUARTO_t) # temos poucos imoveis com 4 e 5 quartos, assim, vale a pena pensar em uma padronização
                     # talvez fazer duas categorias: <=2 / >2
xtable::xtable(a)

base.treino$QUARTO <- as.factor(base.treino$QUARTO)
tapply(base.treino$VALOR, base.treino$QUARTO, mean) # aqui também pode-se sugerir uma categorização do tipo: <=2 / >2


## BANHEIRO COM VALOR

cor(as.numeric(BANHEIRO_t),VALOR_t) # 0.82
plot(BANHEIRO_t,VALOR_t) # correlação positiva forte

a <- table(BANHEIRO_t) # nota-se poucas observações do valor 4 pra cima
                       # assim, é válido repensar em uma categorização do tipo: <=2 / >2
xtable::xtable(a)

base.treino$BANHEIRO <- as.factor(base.treino$BANHEIRO)
tapply(base.treino$VALOR, base.treino$BANHEIRO, mean) # aqui talvez fazer três categorias: 1e2/3e4/5a7
                                                      # ou fazer duas categorias: <=3 / >3

## SUITE COM VALOR

cor(SUITE_t,VALOR_t) # 0.55
plot(SUITE_t,VALOR_t) # correlação positiva moderada

a <- table(SUITE_t) # nota-se poucas observações com 2 ou mais suites
                    # talvez seja valido categorizar da seguinte forma: <1 / >=1
xtable::xtable(a)

base.treino$SUÍTE <- as.factor(base.treino$SUÍTE)
tapply(base.treino$VALOR, base.treino$SUÍTE, mean) # pelos resultados, dê pra categorizar da seguinte forma: <=2 / >2 
                                                   # testar de forma não binaria e depois binaria

## VAGA COM VALOR

cor(VAGA_t,VALOR_t) # 0.72
plot(VAGA_t,VALOR_t) # correlação positiva forte

a <- table(VAGA_t) # nota-se poucas observações com 2 ou mais vagas
                   # talvez seja valido categorizar da seguinte forma: <1 / >=1
xtable::xtable(a)

base.treino$VAGA <- as.factor(base.treino$VAGA)
tapply(base.treino$VALOR, base.treino$VAGA, mean) # pelos resultados, dê pra categorizar da seguinte forma: <=1 / >1  
                                                  # testar de forma não binária e depois binária



#DIANTE DOS RESULTADOS, VIMOS QUE FAZ SENTIDO REALIZAR UMA CATEGORIZAÇÃO NESSAS VARIÁVEIS



##################################################################


# 1º Categorização: suite e vaga sem serem binarias

# Banco categorizado:

banco_treino_cat <- base.treino
banco_treino_cat$QUARTO_cat <- ifelse(banco_treino_cat$QUARTO <= 2, "<=2 quartos", ">2 quartos")
banco_treino_cat$BANHEIRO_cat <- ifelse(banco_treino_cat$BANHEIRO <= 3, "<=3 banheiros", ">3 banheiros")
banco_treino_cat$SUITE_cat <- ifelse(banco_treino_cat$SUÍTE <= 1, "<=1 suíte", ">1 suíte")
banco_treino_cat$VAGA_cat <- ifelse(banco_treino_cat$VAGA <= 1, "<=1 vaga", ">1 vaga")

banco_treino_cat$QUARTO_cat <- as.factor(banco_treino_cat$QUARTO_cat)
banco_treino_cat$BANHEIRO_cat <- as.factor(banco_treino_cat$BANHEIRO_cat)
banco_treino_cat$SUITE_cat <- as.factor(banco_treino_cat$SUITE_cat)
banco_treino_cat$VAGA_cat <- as.factor(banco_treino_cat$VAGA_cat)
banco_treino_cat$BAIRRO_cat <- as.factor(banco_treino_cat$BAIRRO)



#####################################################################



# Construindo o modelo completo categorizado e sem trasnformação na variavel y
# obs: com suite e vaga sem serem binarias

mod.treino <- lm(VALOR~ÁREA+QUARTO_cat+BANHEIRO_cat+SUITE_cat+VAGA_cat+BAIRRO_cat, data = banco_treino_cat)

a <- summary(mod.treino) # considerando um alpha de 5%, banheiro e bairro não apresentaram significância
xtable::xtable(a)

anova(mod.treino)


# Ánalise de diagnóstico do mod.treino:

par(mfrow=c(1,2))

plot(mod.treino$fitted.values,mod.treino$residuals,pch=16, 
     xlab = "Valores ajustados",
     ylab = "Resíduos", col = 4)
abline(h=0,col=2)
abline(h=-4e+05,col=2)
abline(h=4e+05,col=2)
identify(mod.treino$fitted.values,mod.treino$residuals) # encontra-se 9 outliers, cujas observaçõees são:  5   8  11  28  33  37  55 149 173

plot(mod.treino$residuals,pch=16, xlab = "", ylab = "Resíduos", col = 4)
abline(h=0,col=2)
abline(h=-4e+05,col=2)
abline(h=4e+05,col=2)
identify(mod.treino$residuals) # encontra-se 9 outliers, cujas observações são:  5   8  11  28  33  37  55 149 173


par(mfrow=c(2,3))


plot(banco_treino_cat$ÁREA,mod.treino$residuals,pch=16,xlab = "Área", ylab = "Resíduos", col=4)
abline(h=0,col=2)
abline(h=-4e+05,col=2)
abline(h=4e+05,col=2)
identify(banco_treino_cat$ÁREA,mod.treino$residuals) # encontra-se 9 outliers, cujas observações são:  5   8  11  28  33  37  55 149 173

plot(banco_treino_cat$QUARTO_cat,mod.treino$residuals,pch=16,xlab = "Quarto", 
     ylab = "Resíduos", col=4)
abline(h=0,col=2)
abline(h=-4e+05,col=2)
abline(h=4e+05,col=2)
identify(banco_treino_cat$QUARTO_cat,mod.treino$residuals) # encontra-se 6 outliers, cujas observações são:  8  28  33  37  55 179

plot(banco_treino_cat$BANHEIRO_cat,mod.treino$residuals,pch=16,xlab = "Banheiro", 
     ylab = "Resíduos", col=4)
abline(h=0,col=2)
abline(h=-4e+05,col=2)
abline(h=4e+05,col=2)
identify(banco_treino_cat$BANHEIRO_cat,mod.treino$residuals) # encontra-se 7 outliers, cujas observações são:  5   8  28  33  37  55 149

plot(banco_treino_cat$SUITE_cat,mod.treino$residuals,pch=16,xlab = "Suíte",
     ylab = "Resíduos", col=4)
abline(h=0,col=2)
abline(h=-4e+05,col=2)
abline(h=4e+05,col=2)
identify(banco_treino_cat$SUITE_cat,mod.treino$residuals) # encontra-se 7 outliers, cujas observações são:  5   8  28  33 37 55 149

plot(banco_treino_cat$VAGA_cat,mod.treino$residuals,pch=16,xlab = "Vaga", 
     ylab = "Resíduos", col=4)
abline(h=0,col=2)
abline(h=-4e+05,col=2)
abline(h=4e+05,col=2)
identify(banco_treino_cat$VAGA_cat,mod.treino$residuals) # encontra-se 9 outliers, cujas observações são:  5   8  11  28  33  37  55 149 173


# Verificando normalidade:

par(mfrow=c(1,1))

stud_treino <- rstudent(mod.treino)

hist(stud_treino, xlab = "y", ylab = "Frequência", main = "", col = 4) # graficamente, aparente ser simetrico em torno do zero
                                                                       # porem, nota-se alguns valores distante, provavelmente são outliers, os quais podem prejudicar o pressuposto da normalidade

qqnorm(mod.treino$residuals,col=4,xlab = "Quantis Teóricos",  ylab = "Quantis Amostrais", main = "")
qqline(mod.treino$residuals,col=2) # por esse gráfico, é possível notar que muitas observações (a maioria delas) estão em cima da reta. 
                                   # Dessa forma, há indícios de que os dados são normais. Porém, tem-se pontos que estao fora da reta,
                                   # alguns deles muitos distantes, o que pode resultar em uma não normalidade

ols_plot_resid_qq(mod.treino)


shapiro.test(mod.treino$residuals) # Por meio desse teste, é possível notar um p-valor menor que 5% (nivel de confiança considerado), 
                                   # portanto há evidências suficientes para rejeitar h0, ou seja, os dados não possuem uma distribuição
                                   # normal. Provavelmente por causa dos outliers que vimos nos graficos

# Verificando independencia dos erros:

dwtest(mod.treino) # Por meio desse teste, nota-se um p-valor maior que 5% (nivel de confiança considerado), portanto não há evidências 
                   # suficientes para rejeitar h0, ou seja, os dados são independentes.

# Verificando homogeneidade da variância:

bptest(mod.treino) # Por meio desse teste, nota-se um p-valor menor que 5% (nivel de confiança considerado), portanto há evidências 
                   # suficientes para rejeitar h0, ou seja, os dados são não possuem variância homogênea.


# TALVEZ FAZER UM NOVO MODELO SEM OS OUTLIERS PRA VER SE TODOS OS PRESSUPOSTOS PASSAM!!



############################################################



# Verificando necessidade de transformação na variável y:

boxcox(mod.treino,lambda = seq(-2,2,by=0.5), 
       ylab = "Log-Verossimilhan?a") #lambda =~ 0.5 



#############################################################



# Seleção de variáveis

# Para ver qual o número melhor de variáveis pra entrar no modelo:

k <- ols_step_all_possible(mod.treino)
plot(k) # analisando os gráficos das medidas que penalizam a quantidade de variáveis (CP, AIC, BIC e o SBC), percebe-se que
        # as quantidade de variáveis para entrar no modelos 2, 3, 4 e 5 ficaram com valores muito próximos


# Ver quais os melhores modelos pra cada quantidade de variáveis selecionada anteriormente:

base.treino_teste <- banco_treino_cat[,c(9,14,16,17,18,19,20)]

sele1 <- regsubsets(base.treino_teste$VALOR~., data=base.treino_teste, nbest = 10)
summary(sele1) 

cbind(summary(sele1)$which,summary(sele1)$rsq,summary(sele1)$adjr2, summary(sele1)$cp,summary(sele1)$bic)
# Nota-se que os melhores modelos, para cada quantidade de variáveis selecionada anteriormente, são:
# > com 2 variáveis: VALOR_t = INTERCEPTO + AREA + VAGA_cat
# > com 3 variáveis: VALOR_t = INTERCEPTO + AREA + SUITE_cat + VAGA_cat 
# > com 4 variáveis: VALOR_t = INTERCEPTO + AREA + QUARTO_cat + SUITE_cat + VAGA_cat 
# > com 5 variáveis: VALOR_t = INTERCEPTO + AREA + QUARTO_cat + BANHEIRO_cat + SUITE_cat + VAGA_cat  

BAIRRO_cat <- base.treino_teste$BAIRRO_cat
AREA <- base.treino_teste$ÁREA
QUARTO_cat <- base.treino_teste$QUARTO_cat
BANHEIRO_cat <- base.treino_teste$BANHEIRO_cat
SUITE_cat <- base.treino_teste$SUITE_cat
VAGA_cat <- base.treino_teste$VAGA_cat
VALOR <- base.treino_teste$VALOR
n <- length(VALOR)


# Análise de diagnóstico do modelo com 2 variáveis

mod_2 <- lm(VALOR~AREA + VAGA_cat)
a <- summary(mod_2)
xtable::xtable(a)

# Resíduo excluído studentizado

stud_2 <- rstudent(mod_2)

par(mfrow=c(1,2))

plot(mod_2$fitted.values,stud_2,pch=16, xlab = "y ajustado",
     ylab = "Resíduo studentizado", col=4)
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)
identify(mod_2$fitted.values,stud_2) # nota-se a presença de 11 outliers, cujas observações são: 5   8  11  28  33  37  55 121 149 173 179

plot(stud_2,pch=16, col=4, ylab = "Resíduo studentizado", xlab = "")
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)
identify(stud_2) # nota-se a presença de 11 outliers, cujas observações são: 5   8  11  28  33  37  55 121 149 173 179

# Verificando normalidade:

par(mfrow=c(1,1))

hist(stud_2, xlab = "Resíduo studentizado", ylab = "Frequência", main = "", col = 4) # aparenta ser simétrico em torno do zero, apresentando
                                                                                     # indícios de que os dados são normais. Porém, nota-se alguns valores mais distantes, provavelmente outliers,
                                                                                     # os quais podem prejudicar o pressuposto da normalidade

qqnorm(stud_2,col=4,xlab = "Quantis Teóricos", ylab = "Quantis Amostrais",
       main = "")
qqline(stud_2,col=2)

ols_plot_resid_qq(mod_2) # nota-se que a maioria dos pontos estão sobre a reta, indicando que os dados, provavelmente, são normais.
                         # porem, há pontos que estão muito distantes da reta, podendo prejudicar a validação desse pressuposto

shapiro.test(mod_2$residuals) # nota-se um p-valor menor que o nível de significancia de 5%, portanto, há evidencias suficientes para
                              # rejeitar a hipotese nula, ou seja, os dados não são normais

# Verificando independência dos erros:

dwtest(mod_2) # nota-se um p-valor maior que o nível de significancia de 5%, portanto, não há evidencias suficientes para
              # rejeitar a hipotese nula, ou seja, os erros são independentes

# Verificando a homogeneidade da variância:

bptest(mod_2) # nota-se um p-valor menor que o nível de significancia de 5%, portanto, há evidencias suficientes para
              # rejeitar a hipotese nula, ou seja, as variâncias não são iguais


# Multicolinearidade: (PERGUNTAR PRO PROFESSOR)

g <- (vi_2 <- vif(mod_2))
xtable::xtable(g)
mean(vi_2)


# Verificando observações influentes:

# DFBETAS
# O dfbeta sao pontos que estao tendo uma influencia acima 
# da desejada em relação a estimação daquele parametro, daquele beta.

ols_plot_dfbetas(mod_2) # nota-se a presença de alguns valores influentes, alguns dele se repetem em todo grafico, outros em quase todos eles
                        # vale a pena pensar em tirar eles do conjunto de dados


# DFCOOK
# O dcook é uma medida mais geral, a gente vê a influencia no valor ajustado geral

plot(mod_2,which=4) 

ols_plot_cooksd_chart(mod_4) # nota-se tambem alguns valores influentes, alguns deles são os mesmo que apareceram no DFBTEAS



# Resíduo excluído - para calcular o PRESS: (PERGUTAR PRO PROFESSOR)

medinflu_2 <- influence.measures(mod_2)
indice <- c(1:n)

rexc_2 <- mod_2$residuals/(1-medinflu_2$infmat[,7])
plot(indice,rexc_2)

PRESS_2 <- sum((rexc_2)^2)

rstandard(mod_2,type="predictive")
sum((rstandard(mod_2,type="predictive"))^2) # isso é o erro de previsão, o erro equivalente ao PRESS


# Análise de diagnóstico do modelo com 3 variáveis

mod_3 <- lm(VALOR~AREA + SUITE_cat + VAGA_cat)
a <- summary(mod_3)
xtable::xtable(a)

# Resíduo excluído studentizado

stud_3 <- rstudent(mod_3)

par(mfrow=c(1,2))

plot(mod_3$fitted.values,stud_3,pch=16, xlab = "y ajustado",
     ylab = "Resíduo studentizado", col=4)
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)
identify(mod_3$fitted.values,stud_3) # nota-se a presença de 10 outliers, cujas observações são: 5   8  11  28  33  37  55 149 173 179

plot(stud_3,pch=16, col=4, ylab = "Resíduo studentizado", xlab = "")
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)
identify(stud_3) # nota-se a presença de 10 outliers, cujas observações são: 5   8  11  28  33  37  55 149 173 179

# Verificando normalidade:

par(mfrow=c(1,1))

hist(stud_3, xlab = "Resíduo studentizado", ylab = "Frequência", main = "", col = 4) # aparenta ser simétrico em torno do zero, apresentando
                                                                                     # indícios de que os dados são normais. Porém, nota-se alguns valores mais distantes, provavelmente outliers,
                                                                                     # os quais podem prejudicar o pressuposto da normalidade

qqnorm(stud_3,col=4,xlab = "Quantis Teóricos", ylab = "Quantis Amostrais",
       main = "")
qqline(stud_3,col=2)

ols_plot_resid_qq(mod_3) # nota-se que a maioria dos pontos estão sobre a reta, indicando que os dados, provavelmente, são normais.
                         # porem, há pontos que estão muito distantes da reta, podendo prejudicar a validação desse pressuposto

shapiro.test(mod_3$residuals) # nota-se um p-valor menor que o nível de significancia de 5%, portanto, há evidencias suficientes para
                              # rejeitar a hipotese nula, ou seja, os dados não são normais

# Verificando independência dos erros:

dwtest(mod_3) # nota-se um p-valor maior que o nível de significancia de 5%, portanto, não há evidencias suficientes para
              # rejeitar a hipotese nula, ou seja, os erros são independentes

# Verificando a homogeneidade da variância:

bptest(mod_3) # nota-se um p-valor menor que o nível de significancia de 5%, portanto, há evidencias suficientes para
              # rejeitar a hipotese nula, ou seja, as variâncias não são iguais


# Multicolinearidade: (PERGUNTAR PRO PROFESSOR)

g <- (vi_3 <- vif(mod_3))
xtable::xtable(g)
mean(vi_3)

# Verificando observações influentes:

# DFBETAS
# O dfbeta sao pontos que estao tendo uma influencia acima 
# da desejada em relação a estimação daquele parametro, daquele beta.

ols_plot_dfbetas(mod_3) # nota-se a presença de alguns valores influentes, alguns dele se repetem em todo grafico, outros em quase todos eles
                        # vale a pena pensar em tirar eles do conjunto de dados


# DFCOOK
# O dcook é uma medida mais geral, a gente vê a influencia no valor ajustado geral

plot(mod_3,which=4) 

ols_plot_cooksd_chart(mod_3) # nota-se tambem alguns valores influentes, alguns deles são os mesmo que apareceram no DFBTEAS



# Resíduo excluído - para calcular o PRESS: (PERGUTAR PRO PROFESSOR)

medinflu_3 <- influence.measures(mod_3)
indice <- c(1:n)

rexc_3 <- mod_3$residuals/(1-medinflu_3$infmat[,8])
plot(indice,rexc_3)

PRESS_3 <- sum((rexc_3)^2)

rstandard(mod_3,type="predictive")
sum((rstandard(mod_3,type="predictive"))^2) # isso é o erro de previsão, o erro equivalente ao PRESS



# Análise de diagnóstico do modelo com 4 variáveis

mod_4 <- lm(VALOR~AREA + QUARTO_cat + SUITE_cat + VAGA_cat)
a <- summary(mod_4)
xtable::xtable(a)

# Resíduo excluído studentizado

stud_4 <- rstudent(mod_4)

par(mfrow=c(1,2))

plot(mod_4$fitted.values,stud_4,pch=16, xlab = "y ajustado",
     ylab = "Resíduo studentizado", col=4)
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)
identify(mod_4$fitted.values,stud_4) # nota-se a presença de 10 outliers, cujas observações são: 5   8  11  28  33  37  55 149 173 179

plot(stud_4,pch=16, col=4, ylab = "Resíduo studentizado", xlab = "")
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)
identify(stud_4) # nota-se a presença de 10 outliers, cujas observações são: 5   8  11  28  33  37  55 149 173 179

# Verificando normalidade:

par(mfrow=c(1,1))

hist(stud_4, xlab = "Resíduo studentizado", ylab = "Frequência", main = "", col = 4) # aparenta ser simétrico em torno do zero, apresentando
                                                                                     # indícios de que os dados são normais. Porém, nota-se alguns valores mais distantes, provavelmente outliers,
                                                                                     # os quais podem prejudicar o pressuposto da normalidade

qqnorm(stud_4,col=4,xlab = "Quantis Teóricos", ylab = "Quantis Amostrais",
       main = "")
qqline(stud_4,col=2)

ols_plot_resid_qq(mod_4) # nota-se que a maioria dos pontos estão sobre a reta, indicando que os dados, provavelmente, são normais.
                         # porem, há pontos que estão muito distantes da reta, podendo prejudicar a validação desse pressuposto

shapiro.test(mod_4$residuals) # nota-se um p-valor menor que o nível de significancia de 5%, portanto, há evidencias suficientes para
                              # rejeitar a hipotese nula, ou seja, os dados não são normais

# Verificando independência dos erros:

dwtest(mod_4) # nota-se um p-valor maior que o nível de significancia de 5%, portanto, não há evidencias suficientes para
              # rejeitar a hipotese nula, ou seja, os erros são independentes

# Verificando a homogeneidade da variância:

bptest(mod_4) # nota-se um p-valor menor que o nível de significancia de 5%, portanto, há evidencias suficientes para
              # rejeitar a hipotese nula, ou seja, as variâncias não são iguais


# Multicolinearidade: (PERGUNTAR PRO PROFESSOR)

g <- (vi_4 <- vif(mod_4))
xtable::xtable(g)
mean(vi_4)

# Verificando observações influentes:

# DFBETAS
# O dfbeta sao pontos que estao tendo uma influencia acima 
# da desejada em relação a estimação daquele parametro, daquele beta.

ols_plot_dfbetas(mod_4) # nota-se a presença de alguns valores influentes, alguns dele se repetem em todo grafico, outros em quase todos eles
                        # vale a pena pensar em tirar eles do conjunto de dados


# DFCOOK
# O dcook é uma medida mais geral, a gente vê a influencia no valor ajustado geral

plot(mod_4,which=4) 

ols_plot_cooksd_chart(mod_4) # nota-se tambem alguns valores influentes, alguns deles são os mesmo que apareceram no DFBTEAS



# Resíduo excluído - para calcular o PRESS: (PERGUTAR PRO PROFESSOR)

medinflu_4 <- influence.measures(mod_4)
indice <- c(1:n)

rexc_4 <- mod_4$residuals/(1-medinflu_4$infmat[,9])
plot(indice,rexc_4)

PRESS_4 <- sum((rexc_4)^2)

rstandard(mod_4,type="predictive")
sum((rstandard(mod_4,type="predictive"))^2) # isso é o erro de previsão, o erro equivalente ao PRESS


# Análise de diagnóstico do modelo com 5 variáveis

mod_5 <- lm(VALOR~AREA + QUARTO_cat + BANHEIRO_cat + SUITE_cat + VAGA_cat)
a <- summary(mod_5) # banheiro_cat nao passa
xtable::xtable(a)

# Resíduo excluído studentizado

stud_5 <- rstudent(mod_5)

par(mfrow=c(1,2))

plot(mod_5$fitted.values,stud_5,pch=16, xlab = "y ajustado",
     ylab = "Resíduo studentizado", col=4)
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)
identify(mod_5$fitted.values,stud_5) # nota-se a presença de 9 outliers, cujas observações são: 5   8  11  28  33  37  55 149 173

plot(stud_5,pch=16, col=4, ylab = "Resíduo studentizado", xlab = "")
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)
identify(stud_5) # nota-se a presença de 9 outliers, cujas observações são:  5   8  11  28  33  37  55 149 173

# Verificando normalidade:

par(mfrow=c(1,1))

hist(stud_5, xlab = "Resíduo studentizado", ylab = "Frequência", main = "", col = 4) # aparenta ser simétrico em torno do zero, apresentando
                                                                                     # indícios de que os dados são normais. Porém, nota-se alguns valores mais distantes, provavelmente outliers,
                                                                                     # os quais podem prejudicar o pressuposto da normalidade

qqnorm(stud_5,col=4,xlab = "Quantis Teóricos", ylab = "Quantis Amostrais",
       main = "")
qqline(stud_5,col=2)

ols_plot_resid_qq(mod_5) # nota-se que a maioria dos pontos estão sobre a reta, indicando que os dados, provavelmente, são normais.
                         # porem, há pontos que estão muito distantes da reta, podendo prejudicar a validação desse pressuposto

shapiro.test(mod_5$residuals) # nota-se um p-valor menor que o nível de significancia de 5%, portanto, há evidencias suficientes para
                              # rejeitar a hipotese nula, ou seja, os dados não são normais

# Verificando independência dos erros:

dwtest(mod_5) # nota-se um p-valor maior que o nível de significancia de 5%, portanto, não há evidencias suficientes para
              # rejeitar a hipotese nula, ou seja, os erros são independentes

# Verificando a homogeneidade da variância:

bptest(mod_5) # nota-se um p-valor menor que o nível de significancia de 5%, portanto, há evidencias suficientes para
              # rejeitar a hipotese nula, ou seja, as variâncias não são iguais


# Multicolinearidade: (PERGUNTAR PRO PROFESSOR)

g <- (vi_5 <- vif(mod_5))
xtable::xtable(g)
mean(vi_5)

# Verificando observações influentes:

# DFBETAS
# O dfbeta sao pontos que estao tendo uma influencia acima 
# da desejada em relação a estimação daquele parametro, daquele beta.

ols_plot_dfbetas(mod_5) # nota-se a presença de alguns valores influentes, alguns dele se repetem em todo grafico, outros em quase todos eles
                        # vale a pena pensar em tirar eles do conjunto de dados


# DFCOOK
# O dcook é uma medida mais geral, a gente vê a influencia no valor ajustado geral

plot(mod_5,which=4) 

ols_plot_cooksd_chart(mod_5) # nota-se tambem alguns valores influentes, alguns deles são os mesmo que apareceram no DFBTEAS



# Resíduo excluído - para calcular o PRESS: (PERGUTAR PRO PROFESSOR)

medinflu_5 <- influence.measures(mod_5)
indice <- c(1:n)

rexc_5 <- mod_5$residuals/(1-medinflu_5$infmat[,10])
plot(indice,rexc_5)

PRESS_5 <- sum((rexc_5)^2)

rstandard(mod_5,type="predictive")
sum((rstandard(mod_5,type="predictive"))^2) # isso é o erro de previsão, o erro equivalente ao PRESS



##########################################################################



# Realizando os Método Automáticos:

##Forward##

BAIRRO_cat <- base.treino_teste$BAIRRO_cat
AREA <- base.treino_teste$ÁREA
QUARTO_cat <- base.treino_teste$QUARTO_cat
BANHEIRO_cat <- base.treino_teste$BANHEIRO_cat
SUITE_cat <- base.treino_teste$SUITE_cat
VAGA_cat <- base.treino_teste$VAGA_cat
VALOR <- base.treino_teste$VALOR
n <- length(VALOR)

modmin <- lm(VALOR~1,data=base.treino_teste)
step(modmin, direction = 'forward',
     scope = ( ~ BAIRRO_cat + AREA + QUARTO_cat + BANHEIRO_cat + SUITE_cat + VAGA_cat))

#(Intercept)                  AREA       VAGA_cat>1 vaga     SUITE_cat>1 su?te  QUARTO_cat>2 quartos  
#-86612                 10700                295355                223132                 90705  

# Resultado do forward:
modfor <- lm(VALOR ~ AREA + VAGA_cat + SUITE_cat + QUARTO_cat)
summary(modfor) # mesma coisa do mod_4


##Backward##

mod. <- lm(VALOR~BAIRRO_cat + AREA + QUARTO_cat + BANHEIRO_cat + SUITE_cat + VAGA_cat)
step(mod.,direction = 'backward')

#(Intercept)                  AREA  QUARTO_cat>2 quartos     SUITE_cat>1 su?te       VAGA_cat>1 vaga  
#-86612                 10700                 90705                223132                295355 

# Resultado do backward:
modback <- lm(VALOR ~ AREA + QUARTO_cat + SUITE_cat + VAGA_cat)
summary(modback) # mesma coisa do FORWARD


##Stepwise##

modmin <- lm(VALOR~1,data=base.treino_teste)
step(modmin,scope = list(lower = modmin, upper = mod.),direction = 'both')

#(Intercept)                  AREA       VAGA_cat>1 vaga     SUITE_cat>1 su?te  QUARTO_cat>2 quartos  
#-86612                 10700                295355                223132                 90705  

#resultado do stepwise:
modstep <- lm(VALOR ~ AREA + VAGA_cat + SUITE_cat + QUARTO_cat)
summary(modstep) # mesma coisa do FORWARD e BACKWARD

# PORTANTO, PELOS METODOS AUTOMATICOS (TODOS CONVERGIRAM PARA O MESMO MODELO) O MELHOR MODELO É O COM 4 VARIAVEIS: 
# VALOR ~ AREA + VAGA_cat + SUITE_cat + QUARTO_cat


# Ánálise de diagnóstico do modelo selecionado pelos metodos automaticos: mesma analise feita no mod_4
# que rejeitou os pressupostos de normalidade e de homogeneidade da variância


################################################################################



# ANALISE DO MOD_4 (SELECIONADO PELOS METODOS AUTOMATICOS) SEM OS VALORES INFLUENTES/OUTLIERS

base.treino_teste <- banco_treino_cat[,c(9,14,16,17,18,19,20)]
base.treino_teste <- base.treino_teste[-c(5,11,25,28,37,55,67,81,98,121,132,149,173,179,196),]
# 5,11,25,28,37,55,67,81,98,121,132,149,173,179,196
# 11,37,55,81,98,121,132,173,179,196

BAIRRO_cat <- base.treino_teste$BAIRRO_cat
AREA <- base.treino_teste$ÁREA
QUARTO_cat <- base.treino_teste$QUARTO_cat
BANHEIRO_cat <- base.treino_teste$BANHEIRO_cat
SUITE_cat <- base.treino_teste$SUITE_cat
VAGA_cat <- base.treino_teste$VAGA_cat
VALOR <- base.treino_teste$VALOR
n <- length(VALOR)

mod_4 <- lm(VALOR ~ AREA + QUARTO_cat + SUITE_cat + VAGA_cat)
shapiro.test(mod_4$residuals)
dwtest(mod_4)
bptest(mod_4) 
# TODOS OS PRESSUPOSTOS PASSARAM!!!!!!!!!!!!


# Testes
mod_2 <- lm(VALOR ~ AREA + VAGA_cat)
shapiro.test(mod_2$residuals)
dwtest(mod_2)
bptest(mod_2) # todos passaram

mod_3 <- lm(VALOR ~ AREA + SUITE_cat + VAGA_cat)
shapiro.test(mod_3$residuals)
dwtest(mod_3)
bptest(mod_3) # todos passaram

mod_5 <- lm(VALOR ~ AREA + QUARTO_cat + BANHEIRO_cat + SUITE_cat + 
                    VAGA_cat)
shapiro.test(mod_5$residuals)
dwtest(mod_5)
bptest(mod_5) # todos passaram

mod_6 <- lm(VALOR ~ AREA + QUARTO_cat + BANHEIRO_cat + SUITE_cat + 
                    VAGA_cat + BAIRRO_cat)
shapiro.test(mod_6$residuals)
dwtest(mod_6)
bptest(mod_6) # todos passaram



##################################################################


# 2º Categorização: suite e vaga sendo binarias

# Banco categorizado:



banco_treino_cat <- base.treino
banco_treino_cat$QUARTO_cat <- ifelse(banco_treino_cat$QUARTO <= 2, "<=2 quartos", ">2 quartos")
banco_treino_cat$BANHEIRO_cat <- ifelse(banco_treino_cat$BANHEIRO <= 3, "<=3 banheiros", ">3 banheiros")
banco_treino_cat$SUITE_cat <- ifelse(banco_treino_cat$SUÍTE < 1, "Não tem suíte", "Tem suíte")
banco_treino_cat$VAGA_cat <- ifelse(banco_treino_cat$VAGA < 1, "Não tem vaga", "Tem vaga")

banco_treino_cat$QUARTO_cat <- as.factor(banco_treino_cat$QUARTO_cat)
banco_treino_cat$BANHEIRO_cat <- as.factor(banco_treino_cat$BANHEIRO_cat)
banco_treino_cat$SUITE_cat <- as.factor(banco_treino_cat$SUITE_cat)
banco_treino_cat$VAGA_cat <- as.factor(banco_treino_cat$VAGA_cat)
banco_treino_cat$BAIRRO_cat <- as.factor(banco_treino_cat$BAIRRO)



#####################################################################



# Construindo o modelo completo categorizado e sem trasnformação na variavel y
# obs: com suite e vaga sendo binarias

mod.treino <- lm(VALOR~ÁREA+QUARTO_cat+BANHEIRO_cat+SUITE_cat+VAGA_cat+BAIRRO_cat, data = banco_treino_cat)

a <- summary(mod.treino) # considerando um alpha de 5%, quarto, suite e bairro não apresentaram significância
xtable::xtable(a)        # sai a variavel Bairro


# Atualizando o modelo (sem a variavel Bairro)

mod.treino <- lm(VALOR~ÁREA+QUARTO_cat+BANHEIRO_cat+SUITE_cat+VAGA_cat, data = banco_treino_cat)

a <- summary(mod.treino) # considerando um alpha de 5%, quarto e suite não apresentaram significância
xtable::xtable(a)        # sai a variavel quarto

# Atualizando o modelo (sem as variaveis Bairro e Quarto)

mod.treino <- lm(VALOR~ÁREA+BANHEIRO_cat+SUITE_cat+VAGA_cat, data = banco_treino_cat)

a <- summary(mod.treino) # considerando um alpha de 5%, suite não apresentou significância
xtable::xtable(a)        # sai a variavel suite

# Atualizando o modelo (sem as variaveis Bairro, Quarto e Suíte)

mod.treino <- lm(VALOR~ÁREA+BANHEIRO_cat+VAGA_cat, data = banco_treino_cat)

a <- summary(mod.treino) # todas sao significantes
xtable::xtable(a)        


# Ánalise de diagnóstico do mod.treino:

par(mfrow=c(1,2))

plot(mod.treino$fitted.values,mod.treino$residuals,pch=16, 
     xlab = "Valores ajustados",
     ylab = "Resíduos", col = "#A11D21")
abline(h=0,col=1)
abline(h=-4e+05,col=1)
abline(h=4e+05,col=1)
identify(mod.treino$fitted.values,mod.treino$residuals) # encontra-se 9 outliers, cujas observaçõees são:  5  11  28  37  55 140 149 173 196

plot(mod.treino$residuals,pch=16, xlab = "",
     ylab = "Resíduos", col = "#A11D21")
abline(h=0,col=1)
abline(h=-4e+05,col=1)
abline(h=4e+05,col=1)
identify(mod.treino$residuals) # encontra-se 9 outliers, cujas observações são:  5  11  28  37  55 140 149 173 196


par(mfrow=c(2,3))


plot(banco_treino_cat$ÁREA,mod.treino$residuals,pch=16,xlab = "Área", ylab = "Resíduos", col=4)
abline(h=0,col=2)
abline(h=-4e+05,col=2)
abline(h=4e+05,col=2)
identify(banco_treino_cat$ÁREA,mod.treino$residuals) # encontra-se 8 outliers, cujas observações são:  5  11  28  37  55 140 173 196

plot(banco_treino_cat$QUARTO_cat,mod.treino$residuals,pch=16,xlab = "Quarto", 
     ylab = "Resíduos", col=4)
abline(h=0,col=2)
abline(h=-4e+05,col=2)
abline(h=4e+05,col=2)
identify(banco_treino_cat$QUARTO_cat,mod.treino$residuals) # encontra-se 9 outliers, cujas observações são:  5  28  37  55 132 140 173 179 196

plot(banco_treino_cat$BANHEIRO_cat,mod.treino$residuals,pch=16,xlab = "Banheiro", 
     ylab = "Resíduos", col=4)
abline(h=0,col=2)
abline(h=-4e+05,col=2)
abline(h=4e+05,col=2)
identify(banco_treino_cat$BANHEIRO_cat,mod.treino$residuals) # encontra-se 9 outliers, cujas observações são:  5  11  28  37  55  90 155 179 196

plot(banco_treino_cat$SUITE_cat,mod.treino$residuals,pch=16,xlab = "Suíte",
     ylab = "Resíduos", col=4)
abline(h=0,col=2)
abline(h=-4e+05,col=2)
abline(h=4e+05,col=2)
identify(banco_treino_cat$SUITE_cat,mod.treino$residuals) # encontra-se 12 outliers, cujas observações são:  5  11  28  37  55  90  96 140 149 155 179 196

plot(banco_treino_cat$VAGA_cat,mod.treino$residuals,pch=16,xlab = "Vaga", 
     ylab = "Resíduos", col=4)
abline(h=0,col=2)
abline(h=-4e+05,col=2)
abline(h=4e+05,col=2)
identify(banco_treino_cat$VAGA_cat,mod.treino$residuals) # encontra-se 10 outliers, cujas observações são:  5  11  28  37  55  90 140 149 179 196


# Verificando normalidade:

par(mfrow=c(1,2))

stud_treino <- rstudent(mod.treino)

hist(stud_treino, xlab = "y", ylab = "Frequência", main = "", col = "#A11D21") # graficamente, aparente ser simetrico em torno do zero
                                                                       # porem, nota-se alguns valores distante, provavelmente são outliers, os quais podem prejudicar o pressuposto da normalidade

qqnorm(mod.treino$residuals,col="#A11D21",xlab = "Quantis Teóricos",  ylab = "Quantis Amostrais", main = "")
qqline(mod.treino$residuals,col=1) # por esse gráfico, é possível notar que muitas observações (a maioria delas) estão em cima da reta. 
                                   # Dessa forma, há indícios de que os dados são normais. Porém, tem-se pontos que estao fora da reta,
                                   # alguns deles muitos distantes, o que pode resultar em uma não normalidade

ols_plot_resid_qq(mod.treino)


shapiro.test(mod.treino$residuals) # Por meio desse teste, é possível notar um p-valor menor que 5% (nivel de confiança considerado), 
                                   # portanto há evidências suficientes para rejeitar h0, ou seja, os dados não possuem uma distribuição
                                   # normal. Provavelmente por causa dos outliers que vimos nos graficos

# Verificando independencia dos erros:

dwtest(mod.treino) # Por meio desse teste, nota-se um p-valor maior que 5% (nivel de confiança considerado), portanto não há evidências 
                   # suficientes para rejeitar h0, ou seja, os dados são independentes.

# Verificando homogeneidade da variância:

bptest(mod.treino) # Por meio desse teste, nota-se um p-valor menor que 5% (nivel de confiança considerado), portanto há evidências 
                   # suficientes para rejeitar h0, ou seja, os dados são não possuem variância homogênea.


# TALVEZ FAZER UM NOVO MODELO SEM OS OUTLIERS PRA VER SE TODOS OS PRESSUPOSTOS PASSAM!!


# Multicolinearidade: (PERGUNTAR PRO PROFESSOR)

g <- (vi_treino <- vif(mod.treino))
xtable::xtable(g)
mean(vi_treino)

# Verificando observações influentes:

# DFBETAS
# O dfbeta sao pontos que estao tendo uma influencia acima 
# da desejada em relação a estimação daquele parametro, daquele beta.

ols_plot_dfbetas(mod.treino) # nota-se a presença de alguns valores influentes, alguns dele se repetem em todo grafico, outros em quase todos eles
                             # vale a pena pensar em tirar eles do conjunto de dados


# DFCOOK
# O dcook é uma medida mais geral, a gente vê a influencia no valor ajustado geral

par(mfrow=c(1,1))

plot(mod.treino,which=4, col = "#A11D21") 

ols_plot_cooksd_chart(mod.treino) # nota-se tambem alguns valores influentes, alguns deles são os mesmo que apareceram no DFBTEAS


############################################################



# Verificando necessidade de transformação na variável y:

boxcox(mod.treino,lambda = seq(-2,2,by=0.5), 
       ylab = "Log-Verossimilhan?a") #lambda =~ 0.5 



#############################################################



# Seleção de variáveis

# Para ver qual o número melhor de variáveis pra entrar no modelo:

mod.treino <- lm(VALOR~ÁREA+QUARTO_cat+BANHEIRO_cat+SUITE_cat+VAGA_cat+BAIRRO_cat, data = banco_treino_cat)

k <- ols_step_all_possible(mod.treino)
plot(k) # analisando os gráficos das medidas que penalizam a quantidade de variáveis (CP, AIC, BIC e o SBC), percebe-se que
        # as quantidade de variáveis para entrar no modelos 2, 3, 4 e 5 ficaram com valores muito próximos, sendo 3 e 4 os mais baixos


# Ver quais os melhores modelos pra cada quantidade de variáveis selecionada anteriormente:

base.treino_teste <- banco_treino_cat[,c(9,14,16,17,18,19,20)]

sele1 <- regsubsets(base.treino_teste$VALOR~., data=base.treino_teste, nbest = 3)
summary(sele1) 

b <- cbind(summary(sele1)$which,summary(sele1)$rsq,summary(sele1)$adjr2, summary(sele1)$cp,summary(sele1)$bic)
# Nota-se que os melhores modelos, para cada quantidade de variáveis selecionada anteriormente, são:
# > com 2 variáveis: VALOR_t = INTERCEPTO + AREA + VAGA_cat
# > com 3 variáveis: VALOR_t = INTERCEPTO + AREA + BANHEIRO_cat + VAGA_cat 
# > com 4 variáveis: VALOR_t = INTERCEPTO + AREA + BANHEIRO_cat + SUITE_cat + VAGA_cat 
# > com 5 variáveis: VALOR_t = INTERCEPTO + AREA + QUARTO_cat + BANHEIRO_cat + SUITE_cat + VAGA_cat 

xtable::xtable(b[c(7:12),])


BAIRRO_cat <- base.treino_teste$BAIRRO_cat
AREA <- base.treino_teste$ÁREA
QUARTO_cat <- base.treino_teste$QUARTO_cat
BANHEIRO_cat <- base.treino_teste$BANHEIRO_cat
SUITE_cat <- base.treino_teste$SUITE_cat
VAGA_cat <- base.treino_teste$VAGA_cat
VALOR <- base.treino_teste$VALOR
n <- length(VALOR)


# Análise de diagnóstico do modelo com 2 variáveis

mod_2 <- lm(VALOR~AREA + VAGA_cat)
a <- summary(mod_2)
xtable::xtable(a)

# Resíduo excluído studentizado

stud_2 <- rstudent(mod_2)

par(mfrow=c(1,2))

plot(mod_2$fitted.values,stud_2,pch=16, xlab = "y ajustado",
     ylab = "Resíduo studentizado", col=4)
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)
identify(mod_2$fitted.values,stud_2) # nota-se a presença de 9 outliers, cujas observações são: 5  11  28   37  55 149 173 179 196

plot(stud_2,pch=16, col=4, ylab = "Resíduo studentizado", xlab = "")
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)
identify(stud_2) # nota-se a presença de 9 outliers, cujas observações são: 5  11  28  37  55 149 173 179 196

# Verificando normalidade:

par(mfrow=c(1,1))

hist(stud_2, xlab = "Resíduo studentizado", ylab = "Frequência", main = "", col = 4) # aparenta ser simétrico em torno do zero, apresentando
                                                                                     # indícios de que os dados são normais. Porém, nota-se alguns valores mais distantes, provavelmente outliers,
                                                                                     # os quais podem prejudicar o pressuposto da normalidade

qqnorm(stud_2,col=4,xlab = "Quantis Teóricos", ylab = "Quantis Amostrais",
       main = "")
qqline(stud_2,col=2)

ols_plot_resid_qq(mod_2) # nota-se que a maioria dos pontos estão sobre a reta, indicando que os dados, provavelmente, são normais.
                         # porem, há pontos que estão muito distantes da reta, podendo prejudicar a validação desse pressuposto

shapiro.test(mod_2$residuals) # nota-se um p-valor menor que o nível de significancia de 5%, portanto, há evidencias suficientes para
                              # rejeitar a hipotese nula, ou seja, os dados não são normais

# Verificando independência dos erros:

dwtest(mod_2) # nota-se um p-valor maior que o nível de significancia de 5%, portanto, não há evidencias suficientes para
              # rejeitar a hipotese nula, ou seja, os erros são independentes

# Verificando a homogeneidade da variância:

bptest(mod_2) # nota-se um p-valor menor que o nível de significancia de 5%, portanto, há evidencias suficientes para
              # rejeitar a hipotese nula, ou seja, as variâncias não são iguais


# Multicolinearidade: (PERGUNTAR PRO PROFESSOR)

g <- (vi_2 <- vif(mod_2))
xtable::xtable(g)
mean(vi_2)


# Verificando observações influentes:

# DFBETAS
# O dfbeta sao pontos que estao tendo uma influencia acima 
# da desejada em relação a estimação daquele parametro, daquele beta.

ols_plot_dfbetas(mod_2) # nota-se a presença de alguns valores influentes, alguns dele se repetem em todo grafico, outros em quase todos eles
                        # vale a pena pensar em tirar eles do conjunto de dados


# DFCOOK
# O dcook é uma medida mais geral, a gente vê a influencia no valor ajustado geral

plot(mod_2,which=4) 

ols_plot_cooksd_chart(mod_4) # nota-se tambem alguns valores influentes, alguns deles são os mesmo que apareceram no DFBTEAS


# Resíduo excluído - para calcular o PRESS: (PERGUTAR PRO PROFESSOR)

medinflu_2 <- influence.measures(mod_2)
indice <- c(1:n)

rexc_2 <- mod_2$residuals/(1-medinflu_2$infmat[,7])
plot(indice,rexc_2)

PRESS_2 <- sum((rexc_2)^2)

rstandard(mod_2,type="predictive")
sum((rstandard(mod_2,type="predictive"))^2) # isso é o erro de previsão, o erro equivalente ao PRESS


# Análise de diagnóstico do modelo com 3 variáveis

mod_3 <- lm(VALOR~AREA + BANHEIRO_cat + VAGA_cat)
a <- summary(mod_3)
xtable::xtable(a)

# Resíduo excluído studentizado

stud_3 <- rstudent(mod_3)

par(mfrow=c(1,2))

plot(mod_3$fitted.values,stud_3,pch=16, xlab = "y ajustado",
     ylab = "Resíduo studentizado", col=4)
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)
identify(mod_3$fitted.values,stud_3) # nota-se a presença de 11 outliers, cujas observações são: 5  11  28  37  55 132 140 149 173 179 196

plot(stud_3,pch=16, col=4, ylab = "Resíduo studentizado", xlab = "")
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)
identify(stud_3) # nota-se a presença de 11 outliers, cujas observações são: 5  11  28  37  55 132 140 149 173 179 196

# Verificando normalidade:

par(mfrow=c(1,1))

hist(stud_3, xlab = "Resíduo studentizado", ylab = "Frequência", main = "", col = 4) # aparenta ser simétrico em torno do zero, apresentando
                                                                                     # indícios de que os dados são normais. Porém, nota-se alguns valores mais distantes, provavelmente outliers,
                                                                                     # os quais podem prejudicar o pressuposto da normalidade

qqnorm(stud_3,col=4,xlab = "Quantis Teóricos", ylab = "Quantis Amostrais",
       main = "")
qqline(stud_3,col=2)

ols_plot_resid_qq(mod_3) # nota-se que a maioria dos pontos estão sobre a reta, indicando que os dados, provavelmente, são normais.
                         # porem, há pontos que estão muito distantes da reta, podendo prejudicar a validação desse pressuposto

shapiro.test(mod_3$residuals) # nota-se um p-valor menor que o nível de significancia de 5%, portanto, há evidencias suficientes para
                              # rejeitar a hipotese nula, ou seja, os dados não são normais

# Verificando independência dos erros:

dwtest(mod_3) # nota-se um p-valor maior que o nível de significancia de 5%, portanto, não há evidencias suficientes para
              # rejeitar a hipotese nula, ou seja, os erros são independentes

# Verificando a homogeneidade da variância:

bptest(mod_3) # nota-se um p-valor menor que o nível de significancia de 5%, portanto, há evidencias suficientes para
              # rejeitar a hipotese nula, ou seja, as variâncias não são iguais


# Multicolinearidade: (PERGUNTAR PRO PROFESSOR)

g <- (vi_3 <- vif(mod_3))
xtable::xtable(g)
mean(vi_3)

# Verificando observações influentes:

# DFBETAS
# O dfbeta sao pontos que estao tendo uma influencia acima 
# da desejada em relação a estimação daquele parametro, daquele beta.

ols_plot_dfbetas(mod_3) # nota-se a presença de alguns valores influentes, alguns dele se repetem em todo grafico, outros em quase todos eles
                        # vale a pena pensar em tirar eles do conjunto de dados


# DFCOOK
# O dcook é uma medida mais geral, a gente vê a influencia no valor ajustado geral

plot(mod_3,which=4) 

ols_plot_cooksd_chart(mod_3) # nota-se tambem alguns valores influentes, alguns deles são os mesmo que apareceram no DFBTEAS



# Resíduo excluído - para calcular o PRESS: (PERGUTAR PRO PROFESSOR)

medinflu_3 <- influence.measures(mod_3)
indice <- c(1:n)

rexc_3 <- mod_3$residuals/(1-medinflu_3$infmat[,8])
plot(indice,rexc_3)

PRESS_3 <- sum((rexc_3)^2)

rstandard(mod_3,type="predictive")
sum((rstandard(mod_3,type="predictive"))^2) # isso é o erro de previsão, o erro equivalente ao PRESS


# Análise de diagnóstico do modelo com 4 variáveis

mod_4 <- lm(VALOR~AREA + BANHEIRO_cat + SUITE_cat + VAGA_cat)
a <- summary(mod_4) # suite não passa
xtable::xtable(a)

# Resíduo excluído studentizado

stud_4 <- rstudent(mod_4)

par(mfrow=c(1,2))

plot(mod_4$fitted.values,stud_4,pch=16, xlab = "y ajustado",
     ylab = "Resíduo studentizado", col=4)
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)
identify(mod_4$fitted.values,stud_4) # nota-se a presença de 10 outliers, cujas observações são: 5  11  28  37  55 132 140 149 173 196

plot(stud_4,pch=16, col=4, ylab = "Resíduo studentizado", xlab = "")
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)
identify(stud_4) # nota-se a presença de 10 outliers, cujas observações são: 5  11  28  37  55 132 140 149 173 196

# Verificando normalidade:

par(mfrow=c(1,1))

hist(stud_4, xlab = "Resíduo studentizado", ylab = "Frequência", main = "", col = 4) # aparenta ser simétrico em torno do zero, apresentando
                                                                                     # indícios de que os dados são normais. Porém, nota-se alguns valores mais distantes, provavelmente outliers,
                                                                                     # os quais podem prejudicar o pressuposto da normalidade

qqnorm(stud_4,col=4,xlab = "Quantis Teóricos", ylab = "Quantis Amostrais",
       main = "")
qqline(stud_4,col=2)

ols_plot_resid_qq(mod_4) # nota-se que a maioria dos pontos estão sobre a reta, indicando que os dados, provavelmente, são normais.
                         # porem, há pontos que estão muito distantes da reta, podendo prejudicar a validação desse pressuposto

shapiro.test(mod_4$residuals) # nota-se um p-valor menor que o nível de significancia de 5%, portanto, há evidencias suficientes para
                              # rejeitar a hipotese nula, ou seja, os dados não são normais

# Verificando independência dos erros:

dwtest(mod_4) # nota-se um p-valor maior que o nível de significancia de 5%, portanto, não há evidencias suficientes para
              # rejeitar a hipotese nula, ou seja, os erros são independentes

# Verificando a homogeneidade da variância:

bptest(mod_4) # nota-se um p-valor menor que o nível de significancia de 5%, portanto, há evidencias suficientes para
              # rejeitar a hipotese nula, ou seja, as variâncias não são iguais


# Multicolinearidade: (PERGUNTAR PRO PROFESSOR)

g <- (vi_4 <- vif(mod_4))
xtable::xtable(g)
mean(vi_4)

# Verificando observações influentes:

# DFBETAS
# O dfbeta sao pontos que estao tendo uma influencia acima 
# da desejada em relação a estimação daquele parametro, daquele beta.

ols_plot_dfbetas(mod_4) # nota-se a presença de alguns valores influentes, alguns dele se repetem em todo grafico, outros em quase todos eles
                        # vale a pena pensar em tirar eles do conjunto de dados


# DFCOOK
# O dcook é uma medida mais geral, a gente vê a influencia no valor ajustado geral

plot(mod_4,which=4) 

ols_plot_cooksd_chart(mod_4) # nota-se tambem alguns valores influentes, alguns deles são os mesmo que apareceram no DFBTEAS


# Resíduo excluído - para calcular o PRESS: (PERGUTAR PRO PROFESSOR)

medinflu_4 <- influence.measures(mod_4)
indice <- c(1:n)

rexc_4 <- mod_4$residuals/(1-medinflu_4$infmat[,9])
plot(indice,rexc_4)

PRESS_4 <- sum((rexc_4)^2)

rstandard(mod_4,type="predictive")
sum((rstandard(mod_4,type="predictive"))^2) # isso é o erro de previsão, o erro equivalente ao PRESS


# Análise de diagnóstico do modelo com 5 variáveis

mod_5 <- lm(VALOR~AREA + QUARTO_cat + BANHEIRO_cat + SUITE_cat + VAGA_cat)
a <- summary(mod_5) # quarto e suite nao passam
xtable::xtable(a)

# Resíduo excluído studentizado

stud_5 <- rstudent(mod_5)

par(mfrow=c(1,2))

plot(mod_5$fitted.values,stud_5,pch=16, xlab = "y ajustado",
     ylab = "Resíduo studentizado", col=4)
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)
identify(mod_5$fitted.values,stud_5) # nota-se a presença de 11 outliers, cujas observações são: 5  11  28  37  55 132 140 149 173 179 196

plot(stud_5,pch=16, col=4, ylab = "Resíduo studentizado", xlab = "")
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)
identify(stud_5) # nota-se a presença de 11 outliers, cujas observações são:  5  11  28  37  55 132 140 149 173 179 196

# Verificando normalidade:

par(mfrow=c(1,1))

hist(stud_5, xlab = "Resíduo studentizado", ylab = "Frequência", main = "", col = 4) # aparenta ser simétrico em torno do zero, apresentando
                                                                                     # indícios de que os dados são normais. Porém, nota-se alguns valores mais distantes, provavelmente outliers,
                                                                                     # os quais podem prejudicar o pressuposto da normalidade

qqnorm(stud_5,col=4,xlab = "Quantis Teóricos", ylab = "Quantis Amostrais",
       main = "")
qqline(stud_5,col=2)

ols_plot_resid_qq(mod_5) # nota-se que a maioria dos pontos estão sobre a reta, indicando que os dados, provavelmente, são normais.
                         # porem, há pontos que estão muito distantes da reta, podendo prejudicar a validação desse pressuposto

shapiro.test(mod_5$residuals) # nota-se um p-valor menor que o nível de significancia de 5%, portanto, há evidencias suficientes para
                              # rejeitar a hipotese nula, ou seja, os dados não são normais

# Verificando independência dos erros:

dwtest(mod_5) # nota-se um p-valor maior que o nível de significancia de 5%, portanto, não há evidencias suficientes para
              # rejeitar a hipotese nula, ou seja, os erros são independentes

# Verificando a homogeneidade da variância:

bptest(mod_5) # nota-se um p-valor menor que o nível de significancia de 5%, portanto, há evidencias suficientes para
              # rejeitar a hipotese nula, ou seja, as variâncias não são iguais


# Multicolinearidade: (PERGUNTAR PRO PROFESSOR)

g <- (vi_5 <- vif(mod_5))
xtable::xtable(g)
mean(vi_5)

# Verificando observações influentes:

# DFBETAS
# O dfbeta sao pontos que estao tendo uma influencia acima 
# da desejada em relação a estimação daquele parametro, daquele beta.

ols_plot_dfbetas(mod_5) # nota-se a presença de alguns valores influentes, alguns dele se repetem em todo grafico, outros em quase todos eles
                        # vale a pena pensar em tirar eles do conjunto de dados


# DFCOOK
# O dcook é uma medida mais geral, a gente vê a influencia no valor ajustado geral

plot(mod_5,which=4) 

ols_plot_cooksd_chart(mod_5) # nota-se tambem alguns valores influentes, alguns deles são os mesmo que apareceram no DFBTEAS



# Resíduo excluído - para calcular o PRESS: (PERGUTAR PRO PROFESSOR)

medinflu_5 <- influence.measures(mod_5)
indice <- c(1:n)

rexc_5 <- mod_5$residuals/(1-medinflu_5$infmat[,10])
plot(indice,rexc_5)

PRESS_5 <- sum((rexc_5)^2)

rstandard(mod_5,type="predictive")
sum((rstandard(mod_5,type="predictive"))^2) # isso é o erro de previsão, o erro equivalente ao PRESS



##########################################################################



# Realizando os Método Automáticos:

##Forward##

BAIRRO_cat <- base.treino_teste$BAIRRO_cat
AREA <- base.treino_teste$ÁREA
QUARTO_cat <- base.treino_teste$QUARTO_cat
BANHEIRO_cat <- base.treino_teste$BANHEIRO_cat
SUITE_cat <- base.treino_teste$SUITE_cat
VAGA_cat <- base.treino_teste$VAGA_cat
VALOR <- base.treino_teste$VALOR
n <- length(VALOR)

modmin <- lm(VALOR~1,data=base.treino_teste)
step(modmin, direction = 'forward',
     scope = ( ~ BAIRRO_cat + AREA + QUARTO_cat + BANHEIRO_cat + SUITE_cat + VAGA_cat))

#(Intercept)                      AREA          VAGA_cat>=1 vaga  BANHEIRO_cat>3 banheiros  
#-153258                     10995                    231849                    210665  

# Resultado do forward:
modfor <- lm(VALOR ~ AREA + VAGA_cat + BANHEIRO_cat)
summary(modfor) # mesma coisa do mod_3


##Backward##

mod. <- lm(VALOR~BAIRRO_cat + AREA + QUARTO_cat + BANHEIRO_cat + SUITE_cat + VAGA_cat)
step(mod.,direction = 'backward')

#(Intercept)                      AREA  BANHEIRO_cat>3 banheiros          VAGA_cat>=1 vaga  
#-153258                     10995                    210665                    231849  

# Resultado do backward:
modback <- lm(VALOR ~ AREA + BANHEIRO_cat + VAGA_cat)
summary(modback) # mesma coisa do FORWARD


##Stepwise##

modmin <- lm(VALOR~1,data=base.treino_teste)
step(modmin,scope = list(lower = modmin, upper = mod.),direction = 'both')

#(Intercept)                      AREA          VAGA_cat>=1 vaga  BANHEIRO_cat>3 banheiros  
#-153258                     10995                    231849                    210665   

#resultado do stepwise:
modstep <- lm(VALOR ~ AREA + VAGA_cat + BANHEIRO_cat)
summary(modstep) # mesma coisa do FORWARD e BACKWARD

# PORTANTO, PELOS METODOS AUTOMATICOS (TODOS CONVERGIRAM PARA O MESMO MODELO) O MELHOR MODELO É O COM 3 VARIAVEIS: 
# VALOR ~ AREA + VAGA_cat + BANHEIRO_cat


# Ánálise de diagnóstico do modelo selecionado pelos metodos automaticos: mesma analise feita no mod_3
# que rejeitou os pressupostos de normalidade e de homogeneidade da variância



################################################################################



# ANALISE DO MOD_3 (SELECIONADO PELOS METODOS AUTOMATICOS) SEM OS VALORES INFLUENTES/OUTLIERS

base.treino_teste <- banco_treino_cat[,c(9,14,16,17,18,19,20)]
base.treino_teste <- base.treino_teste[-c(5,11,28,37,55,132,140,149,164,173,179,193,196),]
#5,11,28,37,55,132,140,164,173,179,193
#149,196

BAIRRO_cat <- base.treino_teste$BAIRRO_cat
AREA <- base.treino_teste$ÁREA
QUARTO_cat <- base.treino_teste$QUARTO_cat
BANHEIRO_cat <- base.treino_teste$BANHEIRO_cat
SUITE_cat <- base.treino_teste$SUITE_cat
VAGA_cat <- base.treino_teste$VAGA_cat
VALOR <- base.treino_teste$VALOR
n <- length(VALOR)

mod_3 <- lm(VALOR ~ AREA + BANHEIRO_cat + VAGA_cat)
shapiro.test(mod_3$residuals)
dwtest(mod_3)
bptest(mod_3) 
# TODOS OS PRESSUPOSTOS PASSARAM!!!!!!!!!!!!


# Testes
mod_2 <- lm(VALOR ~ AREA + VAGA_cat)
shapiro.test(mod_2$residuals)
dwtest(mod_2)
bptest(mod_2) # todos passaram

mod_4 <- lm(VALOR ~ AREA + BANHEIRO_cat + SUITE_cat + VAGA_cat)
shapiro.test(mod_4$residuals)
dwtest(mod_4)
bptest(mod_4) # todos passaram

mod_5 <- lm(VALOR ~ AREA + QUARTO_cat + BANHEIRO_cat + SUITE_cat + 
                    VAGA_cat)
shapiro.test(mod_5$residuals)
dwtest(mod_5)
bptest(mod_5) # todos passaram

mod_6 <- lm(VALOR ~ AREA + QUARTO_cat + BANHEIRO_cat + SUITE_cat + 
                    VAGA_cat + BAIRRO_cat)
shapiro.test(mod_6$residuals)
dwtest(mod_6)
bptest(mod_6) # todos passaram