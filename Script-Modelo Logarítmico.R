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

# ANÁLISE DO MODELO SEM VALOR_M2 E COM LOG(VALOR)

# Banco de dados:

banco_tcc <-  read_excel("C:/Users/Ermida/Downloads/TCC1 - RAFAEL ARARUNA/Relatório Parcial/base tcc1 - vvr - base limpa pelo python.xlsx")
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



###############################################################################



# Construindo o modelo completo (modelo com todas as variáveis/sem seleção):

mod.treino <- lm(log(VALOR_t)~AREA_t+QUARTO_t+BANHEIRO_t+SUITE_t+VAGA_t+BAIRRO_t)

a <- summary(mod.treino) # considerando um alpha de 5%, banheiro e suite não são sognificantes
xtable::xtable(a)        # sai a variavel Banheiro

# Atualizando o modelo (sem a variavel Banheiro)

mod.treino <- lm(log(VALOR_t)~AREA_t+QUARTO_t+SUITE_t+VAGA_t+BAIRRO_t)

a <- summary(mod.treino) # considerando um alpha de 5%, suite não é sognificante
xtable::xtable(a)       # sai a variavel suite

# Atualizando o modelo (sem as variaveis Banheiro e Suite)

mod.treino <- lm(log(VALOR_t)~AREA_t+QUARTO_t+VAGA_t+BAIRRO_t)

a <- summary(mod.treino) # considerando um alpha de 5%, todas sao sognificante
xtable::xtable(a)      


# Ánalise de diagnóstico do mod.treino:

par(mfrow=c(1,2))

plot(mod.treino$fitted.values,mod.treino$residuals,pch=16, 
     xlab = "Valores ajustados",
     ylab = "Resíduos", col = "#A11D21")
abline(h=0,col=1)
abline(h=-0.5,col=1)
abline(h=0.5,col=1)
identify(mod.treino$fitted.values,mod.treino$residuals) # encontra-se 14 outliers, cujas observações são:  5   8  28  33  55  63  80  90 142 155 178 179 185 186

plot(mod.treino$residuals,pch=16, xlab = "", 
     ylab = "Resíduos", col = "#A11D21")
abline(h=0,col=1)
abline(h=-0.5,col=1)
abline(h=0.5,col=1)
identify(mod.treino$residuals) # encontra-se 14 outliers, cujas observações são:  5   8  28  33  55  63  80  90 142 155 178 179 185 186


par(mfrow=c(2,3))

plot(AREA_t,mod.treino$residuals,pch=16,xlab = "Área", ylab = "Resíduos", col=4)
abline(h=0,col=2)
abline(h=-0.5,col=2)
abline(h=0.5,col=2)
identify(AREA_t,mod.treino$residuals) # encontra-se 14 outliers, cujas observações são:  5   8  28  33  55  63  80  90 142 155 178 179 185 186

plot(QUARTO_t,mod.treino$residuals,pch=16,xlab = "Quarto", 
     ylab = "Resíduos", col=4)
abline(h=0,col=2)
abline(h=-0.5,col=2)
abline(h=0.5,col=2)
identify(QUARTO_t,mod.treino$residuals) # encontra-se 14 outliers, cujas observações são:  5   8  28  33  55  63  80  90 142 155 178 179 185 186

plot(BANHEIRO_t,mod.treino$residuals,pch=16,xlab = "Banheiro", 
     ylab = "Resíduos", col=4)
abline(h=0,col=2)
abline(h=-0.5,col=2)
abline(h=0.5,col=2)
identify(BANHEIRO_t,mod.treino$residuals) # encontra-se 14 outliers, cujas observações são:  5   8  28  33  55  63  80  90 142 155 178 179 185 186

plot(SUITE_t,mod.treino$residuals,pch=16,xlab = "Suíte",
     ylab = "Resíduos", col=4)
abline(h=0,col=2)
abline(h=-0.5,col=2)
abline(h=0.5,col=2)
identify(SUITE_t,mod.treino$residuals) # encontra-se 13 outliers, cujas observações são:  5   8  28  33  55  63  80  90 155 178 179 185 186

plot(VAGA_t,mod.treino$residuals,pch=16,xlab = "Vaga", 
     ylab = "Resíduos", col=4)
abline(h=0,col=2)
abline(h=-0.5,col=2)
abline(h=0.5,col=2)
identify(VAGA_t,mod.treino$residuals) # encontra-se 14 outliers, cujas observações são:  5   8  28  33  55  63  80  90 142 155 178 179 185 186


# Verificando normalidade:

par(mfrow=c(1,2))

stud_treino <- rstudent(mod.treino)

hist(stud_treino, xlab = "y", ylab = "Frequência", main = "", 
     col = "#A11D21") # por esse histograma, os dados não aparentam ter uma distribuição normal,
                                                                        # visto que não há uma nítida simetricidade dos dados

qqnorm(mod.treino$residuals,col="#A11D21",xlab = "Quantis Teóricos",  ylab = "Quantis Amostrais", main = "")
qqline(mod.treino$residuals,col=1) 

ols_plot_resid_qq(mod.treino) # por esse gráfico, é possível notar que a maioria das observações estão fora da reta. Dessa forma, há indícios
                              # de que os dados não são normais.


shapiro.test(mod.treino$residuals) # Por meio desse teste, é possível notar um p-valor menor que 5% (nivel de confiança considerado), 
                                   # portanto há evidências suficientes para rejeitar h0, ou seja, os dados não possuem uma distribuição
                                   # normal, como ja indicava os gráficos.

# Verificando independencia dos erros:

dwtest(mod.treino) # Por meio desse teste, nota-se um p-valor maior que 5% (nivel de confiança considerado), portanto não há evidências 
                   # suficientes para rejeitar h0, ou seja, os dados são independentes.

# Verificando homogeneidade da variância:

bptest(mod.treino) # Por meio desse teste, nota-se um p-valor maior que 5% (nivel de confiança considerado), portanto não há evidências 
                   # suficientes para rejeitar h0, ou seja, os dados possuem variância homogênea.


# TALVEZ CONSTRUIR UM NOVO MODELO SEM OS OUTLIERS E FAZER A MESMA ANALISE DE DISGNOTICO NA TENTATIVA DE OBTER NORMALIDADE


# Verificando observações influentes:

# DFBETAS
# O dfbeta sao pontos que estao tendo uma influencia acima 
# da desejada em relação a estimação daquele parametro, daquele beta.

ols_plot_dfbetas(mod.treino) # nota-se a presença de alguns valores influentes, alguns dele se repetem em todo grafico, outros em quase todos eles
                             # vale a pena pensar em tirar eles do conjunto de dados


# DFCOOK
# O dcook é uma medida mais geral, a gente vê a influencia no valor ajustado geral

par(mfrow = c(1,1))

plot(mod.treino,which=4, col = "#A11D21") 

ols_plot_cooksd_chart(mod.treino) # nota-se tambem alguns valores influentes, alguns deles são os mesmo que apareceram no DFBTEAS
                                  # 5,8,11,28,30,33,51,55,63,80,121,142,153,166,179

g <- (vi_treino <- vif(mod.treino))
xtable::xtable(g)
mean(vi_treino)



###############################################################################



# Seleção de variáveis

# Para ver qual o número melhor de variáveis pra entrar no modelo:

mod.treino <- lm(log(VALOR_t)~AREA_t+QUARTO_t+BANHEIRO_t+SUITE_t+VAGA_t+BAIRRO_t)

k <- ols_step_all_possible(mod.treino)
plot(k) # analisando os gráficos das medidas que penalizam a quantidade de variáveis (CP, AIC, BIC e o SBC), percebe-se que
        # as melhores quantidade de variáveis para entrar no modelo são 3, 4 e 5.


# Ver quais os melhores modelos pra cada quantidade de variáveis selecionada anteriormente:

base.treino_teste <- base.treino[,c(8:14)]

base.treino_teste$VALOR <- log(base.treino_teste$VALOR)

sele1 <- regsubsets(base.treino_teste$VALOR~., data=base.treino_teste, nbest = 3)
summary(sele1) 

cbind(summary(sele1)$which,summary(sele1)$rsq,summary(sele1)$adjr2, summary(sele1)$cp,summary(sele1)$bic)
# Nota-se que os melhores modelos, para cada quantidade de variáveis selecionada anteriormente, são:
# > com 3 variáveis: VALOR_t = INTERCEPTO + AREA_t + QUARTO_t + VAGA_t
# > com 4 variáveis: VALOR_t = INTERCEPTO + BAIRRO_t + AREA_t + QUARTO_t + VAGA_t
# > com 5 variáveis: VALOR_t = INTERCEPTO + BAIRRO_t + AREA_t + QUARTO_t + SUITE_t + VAGA_t

b <- cbind(summary(sele1)$which,summary(sele1)$rsq,summary(sele1)$adjr2, summary(sele1)$cp,summary(sele1)$bic)
xtable::xtable(b[7:15,])


# Análise de diagnóstico do modelo com 3 variáveis:

mod_3 <- lm(log(VALOR_t)~AREA_t + QUARTO_t + VAGA_t)
summary(mod_3)

# Resíduo excluído studentizado

stud_3 <- rstudent(mod_3)

par(mfrow=c(1,2))

plot(mod_3$fitted.values,stud_3,pch=16, xlab = "y ajustado",
     ylab = "Resíduo studentizado", col=4)
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)
identify(mod_3$fitted.values,stud_3) # nota-se a presença de 9 outliers, cujas observações são: 5   8  28  33  55  63  80 142 185

plot(stud_3,pch=16, col=4, ylab = "Resíduo studentizado", xlab = "")
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)
identify(stud_3) # nota-se a presença de 9 outliers, cujas observações são: 5   8  28  33  55  63  80 142 185

# Verificando normalidade:

par(mfrow=c(1,1))

hist(stud_3, xlab = "Resíduo studentizado", ylab = "Frequência", main = "", col = 4) # aparenta ser simétrico em torno do zero, apresentando
                                                                                     # indícios de que os dados são normais, apesar da cauda mais pesada pra esquerda

qqnorm(stud_3,col=4,xlab = "Quantis Teóricos", ylab = "Quantis Amostrais",
       main = "")
qqline(stud_3,col=2)

ols_plot_resid_qq(mod_3) # apesar da maioria dos pontos estarem proximos da reta (alguns ate estao sobre a reta), a maioria deles estão fora da reta, 
                         # alguns deles consideravelmente distantes, indicando que os dados, provavelmente, não são normais.

shapiro.test(mod_3$residuals) # nota-se um p-valor menor que o nível de significancia de 5%, portanto, há evidencias suficientes para
                              # rejeitar a hipotese nula, ou seja, os dados não são normais

# Verificando independência dos erros:

dwtest(mod_3) # nota-se um p-valor maior que o nível de significancia de 5%, portanto, não há evidencias suficientes para
              # rejeitar a hipotese nula, ou seja, os erros são independentes

# Verificando a homogeneidade da variância:

bptest(mod_3) # nota-se um p-valor maior que o nível de significancia de 5%, portanto, não há evidencias suficientes para
              # rejeitar a hipotese nula, ou seja, as variâncias são iguais


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
indice <- c(1:n_t)

rexc_3 <- mod_3$residuals/(1-medinflu_3$infmat[,8])
plot(indice,rexc_3)

PRESS_3 <- sum((rexc_3)^2)

rstandard(mod_3,type="predictive")
sum((rstandard(mod_3,type="predictive"))^2)


# Análise de diagnóstico do modelo com 4 variáveis:

mod_4 <- lm(log(VALOR_t)~BAIRRO_t + AREA_t + QUARTO_t + VAGA_t)
summary(mod_4) 

# Resíduo excluído studentizado

stud_4 <- rstudent(mod_4)

par(mfrow=c(1,2))

plot(mod_4$fitted.values,stud_4,pch=16, xlab = "y ajustado",
     ylab = "Resíduo studentizado", col=4)
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)
identify(mod_4$fitted.values,stud_4) # nota-se a presença de 9 outliers, cujas observações são: 5   8  28  33  55  63  80 142 185


plot(stud_4,pch=16, col=4, ylab = "Resíduo studentizado", xlab = "")
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)
identify(stud_4) # nota-se a presença de 9 outliers, cujas observações são: 5   8  28  33  55  63  80 142 185

# Verificando normalidade:

par(mfrow=c(1,1))

hist(stud_4, xlab = "Resíduo studentizado", ylab = "Frequência", main = "", col = 4) # apesar da cauda mais pesada pra esquerda, aparenta ser simétrico
                                                                                     # em torno do zero, apresentando indícios de que os dados são normais

qqnorm(stud_4,col=4,xlab = "Quantis Teóricos", ylab = "Quantis Amostrais",
       main = "")
qqline(stud_4,col=2)

ols_plot_resid_qq(mod_4) # apesar da maioria dos pontos estarem proximos da reta (alguns ate estao sobre a reta), a maioria deles estão fora da reta, 
                         # alguns deles consideravelmente distantes, indicando que os dados, provavelmente, não são normais.

shapiro.test(mod_4$residuals) # nota-se um p-valor menor que o nível de significancia de 5%, portanto, há evidencias suficientes para
                              # rejeitar a hipotese nula, ou seja, os dados não são normais

# Verificando independência dos erros:

dwtest(mod_4) # nota-se um p-valor maior que o nível de significancia de 5%, portanto, não há evidencias suficientes para
              # rejeitar a hipotese nula, ou seja, os erros são independentes

# Verificando a homogeneidade da variância:

bptest(mod_4) # nota-se um p-valor maior que o nível de significancia de 5%, portanto, não há evidencias suficientes para
              # rejeitar a hipotese nula, ou seja, as variâncias são iguais


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
indice <- c(1:n_t)

rexc_4 <- mod_4$residuals/(1-medinflu_4$infmat[,9])
plot(indice,rexc_4)

PRESS_4 <- sum((rexc_4)^2)

rstandard(mod_4,type="predictive")
sum((rstandard(mod_4,type="predictive"))^2) # isso é o erro de previsão, o erro equivalente ao PRESS


# Análise de diagnóstico do modelo com 5 variáveis:

mod_5 <- lm(log(VALOR_t)~BAIRRO_t + AREA_t + QUARTO_t + SUITE_t + VAGA_t)
summary(mod_5) # SUITE_t nao passa

# Resíduo excluído studentizado

stud_5 <- rstudent(mod_5)

par(mfrow=c(1,2))

plot(mod_5$fitted.values,stud_5,pch=16, xlab = "y ajustado",
     ylab = "Resíduo studentizado", col=4)
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)
identify(mod_5$fitted.values,stud_5) # nota-se a presença de 9 outliers, cujas observações são: 5   8  28  33  55  63  80 142 185

plot(stud_5,pch=16, col=4, ylab = "Resíduo studentizado", xlab = "")
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)
identify(stud_5) # nota-se a presença de 9 outliers, cujas observações são: 5   8  28  33  55  63  80 142 185

# Verificando normalidade:

par(mfrow=c(1,1))

hist(stud_5, xlab = "Resíduo studentizado", ylab = "Frequência", main = "", col = 4) # apesar da cauda mais pesada pra esquerda, aparenta ser simétrico
                                                                                     # em torno do zero, apresentando indícios de que os dados são normais
qqnorm(stud_5,col=4,xlab = "Quantis Teóricos", ylab = "Quantis Amostrais",
       main = "")
qqline(stud_5,col=2)

ols_plot_resid_qq(mod_5) # apesar da maioria dos pontos estarem proximos da reta (alguns ate estao sobre a reta), a maioria deles estão fora da reta, 
                         # alguns deles consideravelmente distantes, indicando que os dados, provavelmente, não são normais.

shapiro.test(mod_5$residuals) # nota-se um p-valor menor que o nível de significancia de 5%, portanto, há evidencias suficientes para
                              # rejeitar a hipotese nula, ou seja, os dados não são normais

# Verificando independência dos erros:

dwtest(mod_5) # nota-se um p-valor maior que o nível de significancia de 5%, portanto, não há evidencias suficientes para
              # rejeitar a hipotese nula, ou seja, os erros são independentes

# Verificando a homogeneidade da variância:

bptest(mod_5) # nota-se um p-valor maior que o nível de significancia de 5%, portanto, não há evidencias suficientes para
              # rejeitar a hipotese nula, ou seja, as variâncias são iguais


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
indice <- c(1:n_t)

rexc_5 <- mod_5$residuals/(1-medinflu_5$infmat[,10])
plot(indice,rexc_5)

PRESS_5 <- sum((rexc_5)^2)

rstandard(mod_5,type="predictive")
sum((rstandard(mod_5,type="predictive"))^2) # isso é o erro de previsão, o erro equivalente ao PRESS


# Análise de diagnóstico do modelo com 6 variáveis:

mod_6 <- lm(log(VALOR_t)~BAIRRO_t + AREA_t + QUARTO_t + BANHEIRO_t + SUITE_t + VAGA_t)
summary(mod_6) # IGUAL AO MOD.TREINO



################################################################################



# Realizando os Método Automáticos:

##Forward##

base.treino_teste <- base.treino[,c(8:14)]

base.treino_teste$VALOR <- log(base.treino_teste$VALOR)

BAIRRO_t <- as.factor(base.treino_teste$BAIRRO)
AREA_t <- base.treino_teste$ÁREA
QUARTO_t <- base.treino_teste$QUARTO
BANHEIRO_t <- base.treino_teste$BANHEIRO
SUITE_t <- base.treino_teste$SUÍTE
VAGA_t <- base.treino_teste$VAGA
VALOR_t_log <- base.treino_teste$VALOR
n_t <- length(VALOR_t)

modmin <- lm(VALOR_t_log~1,data=base.treino_teste)
step(modmin, direction = 'forward',
     scope = ( ~ BAIRRO_t + AREA_t + QUARTO_t + BANHEIRO_t + SUITE_t + VAGA_t))

#(Intercept)           AREA_t         QUARTO_t           VAGA_t  BAIRRO_tASA SUL  
#12.132171         0.006671         0.257754         0.192298         0.134079  

# Resultado do forward:
modfor <- lm(log(VALOR_t)~AREA_t + QUARTO_t + VAGA_t + BAIRRO_t)
summary(modfor) # MESMA COISA DO MOD_4


##Backward##

mod. <- lm(VALOR_t_log~BAIRRO_t + AREA_t  + VAGA_t +  SUITE_t + QUARTO_t + BANHEIRO_t)
step(mod.,direction = 'backward')

#(Intercept)  BAIRRO_tASA SUL           AREA_t           VAGA_t         QUARTO_t  
#12.132171         0.134079         0.006671         0.192298         0.257754  

# Resultado do backward:
modback <- lm(log(VALOR_t)~AREA_t + QUARTO_t + VAGA_t + BAIRRO_t)
summary(modback) # MESMA COISA DO FORWARD/MOD_4


##Stepwise##

modmin <- lm(VALOR_t_log~1,data=base.treino_teste)
step(modmin,scope = list(lower = modmin, upper = mod.),direction = 'both')

#(Intercept)           AREA_t         QUARTO_t           VAGA_t  BAIRRO_tASA SUL  
#12.132171         0.006671         0.257754         0.192298         0.134079   

#resultado do stepwise:
modstep <- lm(log(VALOR_t)~AREA_t + QUARTO_t + VAGA_t + BAIRRO_t)
summary(modstep)  # MESMA COISA DO FORWARD/BACKWARD/MOD_4

# PORTANTO, PELOS METODOS AUTOMATICOS (TODOS CONVERGIRAM PARA O MESMO MODELO) O MELHOR MODELO É O COM 4 VARIAVEIS: 
# log(VALOR_t)~AREA_t + QUARTO_t + VAGA_t + BAIRRO_t 



########################################################################



# ANALISE DO MOD_4 (SELECIONADO PELOS METODOS AUTOMATICOS) SEM OS VALORES INFLUENTES/OUTLIERS

base.treino_teste <- base.treino[-c(5,8,11,28,30,33,51,55,63,80,121,142,153,
                                    155,166,178,179,186),]
#5,8,11,28,30,33,51,55,63,80,121,142,153,166,179 - influentes
#90,155,178,185,186 - outliers q nao apereceram no grafico dfcook
# obs: ficou faltando colocar os outliers 90 e 185 ; se colocar eles, a normalidade continua passando, mas a homogeneidade nao

BAIRRO_t <- as.factor(base.treino_teste$BAIRRO)
AREA_t <- base.treino_teste$ÁREA
QUARTO_t <- base.treino_teste$QUARTO
BANHEIRO_t <- base.treino_teste$BANHEIRO
SUITE_t <- base.treino_teste$SUÍTE
VAGA_t <- base.treino_teste$VAGA
VALOR_t <- base.treino_teste$VALOR
n_t <- length(VALOR_t)

mod_4 <- lm(log(VALOR_t)~AREA_t + QUARTO_t + VAGA_t + BAIRRO_t)
shapiro.test(mod_4$residuals)
dwtest(mod_4)
bptest(mod_4) 
# TODOS PASSARAM!!!!!!!!!


# Testes

mod_3 <- lm(log(VALOR_t)~AREA_t + QUARTO_t + VAGA_t)
shapiro.test(mod_3$residuals)
dwtest(mod_3)
bptest(mod_3) # não passou

mod_5 <- lm(log(VALOR_t)~BAIRRO_t + AREA_t + QUARTO_t + SUITE_t + VAGA_t)
shapiro.test(mod_5$residuals)
dwtest(mod_5)
bptest(mod_5)
# TODOS PASSARAM

mod_6 <- lm(log(VALOR_t)~BAIRRO_t + AREA_t + QUARTO_t + BANHEIRO_t + SUITE_t + VAGA_t)
shapiro.test(mod_6$residuals)
dwtest(mod_6)
bptest(mod_6)
# TODOS PASSARAM