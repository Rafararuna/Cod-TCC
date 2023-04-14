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

BAIRRO_v <- as.factor(base.valida$BAIRRO)
AREA_v <- base.valida$ÁREA
QUARTO_v <- base.valida$QUARTO
BANHEIRO_v <- base.valida$BANHEIRO
SUITE_v <- base.valida$SUÍTE
VAGA_v <- base.valida$VAGA
VALOR_v <- base.valida$VALOR
VALORM2_v <- base.valida$VALORM2
n_v <- length(VALOR_v)



###############################################################################


# Modelo 1 Final:

mod_1_final <- lm(VALOR_t~AREA_t + SUITE_t + VAGA_t)
a <- summary(mod_1_final)

# Erro de previsão médio - PRESS - equivalente ao leave on out
medinflu_1 <- influence.measures(mod_1_final)
indice_1 <- c(1:n_t)
rexc_1 <- mod_1_final$residuals/(1-medinflu_1$infmat[,8])
PRESS_1 <- sum((rexc_1/a$sigma)^2) # 220.7721
plot(indice_1,rexc_1, col = "#A11D21")

rstandard(mod_1_final,type="predictive") 
sum((rstandard(mod_1_final,type="predictive")/a$sigma)^2) # 220.7721


## Montando o modelo 1 final com os dados base de validação:

mod1.valid <- lm(VALOR_v~AREA_v + SUITE_v + VAGA_v)
v <- summary(mod1.valid) # coeficientes de determinação
xtable::xtable(v)


# Erro quadrático médio de previsão

VALOR.pred1 = mod_1_final$coefficients[1] +
  mod_1_final$coefficients[2]*AREA_v +
  mod_1_final$coefficients[3]*SUITE_v +
  mod_1_final$coefficients[4]*VAGA_v

MSPR = sum(((VALOR_v-VALOR.pred1)/sd(VALOR_v))^2)/n_v # medida de erro preditivo 
                                                      # 0.2313711



################################################################################



# Modelo 2 Final:
mod_2_final <- lm(log(VALOR_t)~AREA_t + QUARTO_t + VAGA_t + BAIRRO_t)
a <- summary(mod_2_final)

# Erro de previsão medio - PRESS
medinflu_2 <- influence.measures(mod_2_final)
indice_2 <- c(1:n_t)
rexc_2 <- mod_2_final$residuals/(1-medinflu_2$infmat[,9]) 
PRESS_2 <- sum((rexc_2/a$sigma)^2) # 209.9784
plot(indice_2,rexc_2, col = "#A11D21")

rstandard(mod_2_final,type="predictive") 
sum((rstandard(mod_2_final,type="predictive")/a$sigma)^2) # 209.9784 


## Montando o modelo 2 final com os dados base de validação:

mod2.valid <- lm(log(VALOR_v)~AREA_v + QUARTO_v + VAGA_v + BAIRRO_v)
v <- summary(mod2.valid) # coeficientes de determinação ; bairro nao passou
xtable::xtable(v)


# Erro quadrático médio

BAIRRO_v <- factor(base.valida$BAIRRO)
BAIRRO_v2 <- rep(0,n_v)
BAIRRO_v2[base.valida$BAIRRO == "ASA SUL"] <- 1

VALOR.pred2 = mod_2_final$coefficients[1] +
  mod_2_final$coefficients[2]*AREA_v +
  mod_2_final$coefficients[3]*QUARTO_v +
  mod_2_final$coefficients[4]*VAGA_v +
  mod_2_final$coefficients[5]*BAIRRO_v2
  

MSPR = sum(((log(VALOR_v)-VALOR.pred2)/sd(log(VALOR_v)))^2)/n_v # medida de erro preditivo 
                                                                # 0.2270306



#####################################################################################



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

BAIRRO_cat <- banco_treino_cat$BAIRRO_cat
AREA <- banco_treino_cat$ÁREA
QUARTO_cat <- banco_treino_cat$QUARTO_cat
BANHEIRO_cat <- banco_treino_cat$BANHEIRO_cat
SUITE_cat <- banco_treino_cat$SUITE_cat
VAGA_cat <- banco_treino_cat$VAGA_cat
VALOR <- banco_treino_cat$VALOR
n <- length(VALOR)


# Modelo 3 Final:
mod_3_final <- lm(VALOR ~ AREA + BANHEIRO_cat + VAGA_cat)
a <- summary(mod_3_final)

# Erro de previsão médio - PRESS
medinflu_3 <- influence.measures(mod_3_final)
indice_3 <- c(1:n)
rexc_3 <- mod_3_final$residuals/(1-medinflu_3$infmat[,8])
PRESS_3 <- sum((rexc_3/a$sigma)^2) 
plot(indice_3,rexc_3, col = "#A11D21") # 217.4094

rstandard(mod_3_final,type="predictive") 
sum((rstandard(mod_3_final,type="predictive")/a$sigma)^2) # 217.4094


## Montando o modelo 3 final com os dados base de validação:

banco_valida_cat_v <- base.valida
banco_valida_cat_v$QUARTO_cat_v <- ifelse(banco_valida_cat_v$QUARTO <= 2, "<=2 quartos", ">2 quartos")
banco_valida_cat_v$BANHEIRO_cat_v <- ifelse(banco_valida_cat_v$BANHEIRO <= 3, "<=3 banheiros", ">3 banheiros")
banco_valida_cat_v$SUITE_cat_v <- ifelse(banco_valida_cat_v$SUÍTE < 1, "Não tem suíte", "Tem suíte")
banco_valida_cat_v$VAGA_cat_v <- ifelse(banco_valida_cat_v$VAGA < 1, "Não tem vaga", "Tem vaga")

banco_valida_cat_v$QUARTO_cat_v <- as.factor(banco_valida_cat_v$QUARTO_cat_v)
banco_valida_cat_v$BANHEIRO_cat_v <- as.factor(banco_valida_cat_v$BANHEIRO_cat_v)
banco_valida_cat_v$SUITE_cat_v <- as.factor(banco_valida_cat_v$SUITE_cat_v)
banco_valida_cat_v$VAGA_cat_v <- as.factor(banco_valida_cat_v$VAGA_cat_v)
banco_valida_cat_v$BAIRRO_cat_v <- as.factor(banco_valida_cat_v$BAIRRO)

BAIRRO_cat_v <- banco_valida_cat_v$BAIRRO_cat_v
AREA_v <- banco_valida_cat_v$ÁREA
QUARTO_cat_v <- banco_valida_cat_v$QUARTO_cat_v
BANHEIRO_cat_v <- banco_valida_cat_v$BANHEIRO_cat_v
SUITE_cat_v <- banco_valida_cat_v$SUITE_cat_v
VAGA_cat_v <- banco_valida_cat_v$VAGA_cat_v
VALOR_v <- banco_valida_cat_v$VALOR
n <- length(VALOR_v)

mod3.valid <- lm(VALOR_v ~ AREA_v + BANHEIRO_cat_v + VAGA_cat_v)
v <- summary(mod3.valid) # coeficientes de determinação ; banheiro não passou
xtable::xtable(v)


# Erro quadrático médio

BANHEIRO_cat_v <- factor(banco_valida_cat_v$BANHEIRO_cat_v)
BANHEIRO_cat_v2 <- rep(0,n_v)
BANHEIRO_cat_v2[banco_valida_cat_v$BANHEIRO_cat_v == ">3 banheiros"] <- 1

VAGA_cat_v <- factor(banco_valida_cat_v$VAGA_cat_v)
VAGA_cat_v2 <- rep(0,n_v)
VAGA_cat_v2[banco_valida_cat_v$VAGA_cat_v == "Tem vaga"] <- 1

VALOR.pred3 = mod_3_final$coefficients[1] +
  mod_3_final$coefficients[2]*AREA_v +
  mod_3_final$coefficients[3]*BANHEIRO_cat_v2 +
  mod_3_final$coefficients[4]*VAGA_cat_v2


MSPR = sum(((VALOR_v-VALOR.pred3)/sd(VALOR_v))^2)/n_v # medida de erro preditivo 
                                                      # 0.2772007



#########################################################################################



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

banco_treino_cat$VALOR <- log(banco_treino_cat$VALOR)

BAIRRO_cat <- banco_treino_cat$BAIRRO_cat
AREA <- banco_treino_cat$ÁREA
QUARTO_cat <- banco_treino_cat$QUARTO_cat
BANHEIRO_cat <- banco_treino_cat$BANHEIRO_cat
SUITE_cat <- banco_treino_cat$SUITE_cat
VAGA_cat <- banco_treino_cat$VAGA_cat
VALOR_log <- banco_treino_cat$VALOR
n_t <- length(VALOR_log)

# Modelo 4 Final:
mod_4_final <- lm(VALOR_log ~ AREA + QUARTO_cat + BANHEIRO_cat + VAGA_cat + BAIRRO_cat)
a <- summary(mod_4_final)

# Erro de previsão medio - PRESS
medinflu_4 <- influence.measures(mod_4_final)
indice_4 <- c(1:n_t)
rexc_4 <- mod_4_final$residuals/(1-medinflu_4$infmat[,10])
PRESS_4 <- sum((rexc_4/a$sigma)^2) # 208.9026
plot(indice_4,rexc_4, col = "#A11D21")

rstandard(mod_4_final,type="predictive") 
sum((rstandard(mod_4_final,type="predictive")/a$sigma)^2) # 208.9026


## Montando o modelo 4 final com os dados base de validação:

banco_valida_cat_v <- base.valida
banco_valida_cat_v$QUARTO_cat_v <- ifelse(banco_valida_cat_v$QUARTO <= 2, "<=2 quartos", ">2 quartos")
banco_valida_cat_v$BANHEIRO_cat_v <- ifelse(banco_valida_cat_v$BANHEIRO <= 3, "<=3 banheiros", ">3 banheiros")
banco_valida_cat_v$SUITE_cat_v <- ifelse(banco_valida_cat_v$SUÍTE < 1, "Não tem suíte", "Tem suíte")
banco_valida_cat_v$VAGA_cat_v <- ifelse(banco_valida_cat_v$VAGA < 1, "Não tem vaga", "Tem vaga")

banco_valida_cat_v$QUARTO_cat_v <- as.factor(banco_valida_cat_v$QUARTO_cat_v)
banco_valida_cat_v$BANHEIRO_cat_v <- as.factor(banco_valida_cat_v$BANHEIRO_cat_v)
banco_valida_cat_v$SUITE_cat_v <- as.factor(banco_valida_cat_v$SUITE_cat_v)
banco_valida_cat_v$VAGA_cat_v <- as.factor(banco_valida_cat_v$VAGA_cat_v)
banco_valida_cat_v$BAIRRO_cat_v <- as.factor(banco_valida_cat_v$BAIRRO)

banco_valida_cat_v$VALOR <- log(banco_valida_cat_v$VALOR)

BAIRRO_cat_v <- banco_valida_cat_v$BAIRRO_cat_v
AREA_v <- banco_valida_cat_v$ÁREA
QUARTO_cat_v <- banco_valida_cat_v$QUARTO_cat_v
BANHEIRO_cat_v <- banco_valida_cat_v$BANHEIRO_cat_v
SUITE_cat_v <- banco_valida_cat_v$SUITE_cat_v
VAGA_cat_v <- banco_valida_cat_v$VAGA_cat_v
VALOR_log_v <- banco_valida_cat_v$VALOR
n_v <- length(VALOR_log_v)

mod4.valid <- lm(VALOR_log_v ~ AREA_v + QUARTO_cat_v + BANHEIRO_cat_v + VAGA_cat_v + BAIRRO_cat_v)
v <- summary(mod4.valid) # coeficientes de determinação ; banheiro e bairro não passaram
xtable::xtable(v)


# Erro quadrático médio

QUARTO_cat_v <- factor(banco_valida_cat_v$QUARTO_cat_v)
QUARTO_cat_v2 <- rep(0,n_v)
QUARTO_cat_v2[banco_valida_cat_v$QUARTO_cat_v == ">2 quartos"] <- 1

BANHEIRO_cat_v <- factor(banco_valida_cat_v$BANHEIRO_cat_v)
BANHEIRO_cat_v2 <- rep(0,n_v)
BANHEIRO_cat_v2[banco_valida_cat_v$BANHEIRO_cat_v == ">3 banheiros"] <- 1

VAGA_cat_v <- factor(banco_valida_cat_v$VAGA_cat_v)
VAGA_cat_v2 <- rep(0,n_v)
VAGA_cat_v2[banco_valida_cat_v$VAGA_cat_v == "Tem vaga"] <- 1

BAIRRO_cat_v <- factor(banco_valida_cat_v$BAIRRO_cat_v)
BAIRRO_cat_v2 <- rep(0,n_v)
BAIRRO_cat_v2[banco_valida_cat_v$BAIRRO_cat_v == "ASA SUL"] <- 1

VALOR.pred4 = mod_4_final$coefficients[1] +
  mod_4_final$coefficients[2]*AREA_v +
  mod_4_final$coefficients[3]*QUARTO_cat_v2 +
  mod_4_final$coefficients[4]*BANHEIRO_cat_v2 +
  mod_4_final$coefficients[5]*VAGA_cat_v2 +
  mod_4_final$coefficients[6]*BAIRRO_cat_v2


MSPR = sum(((VALOR_log_v-VALOR.pred4)/sd(VALOR_log_v))^2)/n_v # medida de erro preditivo 
                                                              # 0.2394628



#########################################################################################



mod_4 <- lm(VALOR_log ~ AREA + QUARTO_cat + VAGA_cat + BAIRRO_cat)

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

banco_treino_cat$VALOR <- log(banco_treino_cat$VALOR)

BAIRRO_cat <- banco_treino_cat$BAIRRO_cat
AREA <- banco_treino_cat$ÁREA
QUARTO_cat <- banco_treino_cat$QUARTO_cat
BANHEIRO_cat <- banco_treino_cat$BANHEIRO_cat
SUITE_cat <- banco_treino_cat$SUITE_cat
VAGA_cat <- banco_treino_cat$VAGA_cat
VALOR_log <- banco_treino_cat$VALOR
n_t <- length(VALOR_log)

# Modelo 4 Final:
mod_4_final <- lm(VALOR_log ~ AREA + QUARTO_cat + VAGA_cat + BAIRRO_cat)
a <- summary(mod_4_final)

# Erro de previsão medio - PRESS
medinflu_4 <- influence.measures(mod_4_final)
indice_4 <- c(1:n_t)
rexc_4 <- mod_4_final$residuals/(1-medinflu_4$infmat[,9])
PRESS_4 <- sum((rexc_4/a$sigma)^2) # 207.3586
plot(indice_4,rexc_4, col = "#A11D21")

rstandard(mod_4_final,type="predictive") 
sum((rstandard(mod_4_final,type="predictive")/a$sigma)^2) # 208.9026


## Montando o modelo 4 final com os dados base de validação:

banco_valida_cat_v <- base.valida
banco_valida_cat_v$QUARTO_cat_v <- ifelse(banco_valida_cat_v$QUARTO <= 2, "<=2 quartos", ">2 quartos")
banco_valida_cat_v$BANHEIRO_cat_v <- ifelse(banco_valida_cat_v$BANHEIRO <= 3, "<=3 banheiros", ">3 banheiros")
banco_valida_cat_v$SUITE_cat_v <- ifelse(banco_valida_cat_v$SUÍTE < 1, "Não tem suíte", "Tem suíte")
banco_valida_cat_v$VAGA_cat_v <- ifelse(banco_valida_cat_v$VAGA < 1, "Não tem vaga", "Tem vaga")

banco_valida_cat_v$QUARTO_cat_v <- as.factor(banco_valida_cat_v$QUARTO_cat_v)
banco_valida_cat_v$BANHEIRO_cat_v <- as.factor(banco_valida_cat_v$BANHEIRO_cat_v)
banco_valida_cat_v$SUITE_cat_v <- as.factor(banco_valida_cat_v$SUITE_cat_v)
banco_valida_cat_v$VAGA_cat_v <- as.factor(banco_valida_cat_v$VAGA_cat_v)
banco_valida_cat_v$BAIRRO_cat_v <- as.factor(banco_valida_cat_v$BAIRRO)

banco_valida_cat_v$VALOR <- log(banco_valida_cat_v$VALOR)

BAIRRO_cat_v <- banco_valida_cat_v$BAIRRO_cat_v
AREA_v <- banco_valida_cat_v$ÁREA
QUARTO_cat_v <- banco_valida_cat_v$QUARTO_cat_v
BANHEIRO_cat_v <- banco_valida_cat_v$BANHEIRO_cat_v
SUITE_cat_v <- banco_valida_cat_v$SUITE_cat_v
VAGA_cat_v <- banco_valida_cat_v$VAGA_cat_v
VALOR_log_v <- banco_valida_cat_v$VALOR
n_v <- length(VALOR_log_v)

mod4.valid <- lm(VALOR_log_v ~ AREA_v + QUARTO_cat_v + VAGA_cat_v + BAIRRO_cat_v)
v <- summary(mod4.valid) # coeficientes de determinação ; bairro não passou
xtable::xtable(v)


# Erro quadrático médio

QUARTO_cat_v <- factor(banco_valida_cat_v$QUARTO_cat_v)
QUARTO_cat_v2 <- rep(0,n_v)
QUARTO_cat_v2[banco_valida_cat_v$QUARTO_cat_v == ">2 quartos"] <- 1

VAGA_cat_v <- factor(banco_valida_cat_v$VAGA_cat_v)
VAGA_cat_v2 <- rep(0,n_v)
VAGA_cat_v2[banco_valida_cat_v$VAGA_cat_v == "Tem vaga"] <- 1

BAIRRO_cat_v <- factor(banco_valida_cat_v$BAIRRO_cat_v)
BAIRRO_cat_v2 <- rep(0,n_v)
BAIRRO_cat_v2[banco_valida_cat_v$BAIRRO_cat_v == "ASA SUL"] <- 1

VALOR.pred4 = mod_4_final$coefficients[1] +
  mod_4_final$coefficients[2]*AREA_v +
  mod_4_final$coefficients[3]*QUARTO_cat_v2 +
  mod_4_final$coefficients[4]*VAGA_cat_v2 +
  mod_4_final$coefficients[5]*BAIRRO_cat_v2


MSPR = sum(((VALOR_log_v-VALOR.pred4)/sd(VALOR_log_v))^2)/n_v # medida de erro preditivo 
                                                              # 0.239152
