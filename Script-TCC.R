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

# ANÁLISES PARCIAIS + ANALISE DO MODELO DIRETO

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


# Análises descritivas (usando o banco todo):

## Bairro

table(BAIRRO)
xtable(table(BAIRRO))

## Área Geral

xtable(summary(AREA))
sd(AREA)

ggplot(banco_tcc, aes(x=factor(""), y=AREA)) +
  geom_boxplot(fill=c("#A11D21"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Área")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))

## Area por bairro

ggplot(banco_tcc, aes(x=BAIRRO, y=AREA)) +
  geom_boxplot(fill=c("#A11D21"), width = 0.5) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="Bairro", y="Área") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black")) 

datafalt <- data.frame(BAIRRO,AREA)
med_por_bairro <- ddply(datafalt,.(BAIRRO),summarise,media=mean(AREA),dp=sqrt(var(AREA)),vari=var(AREA),minimo=min(AREA),maximo=max(AREA),cv=100*((sqrt(var(AREA))/mean(AREA))),n=length(AREA))
xtable(med_por_bairro)


## Quarto Geral

summary(QUARTO)
sd(QUARTO)


ggplot(banco_tcc, aes(x=factor(""), y=QUARTO)) +
  geom_boxplot(fill=c("#A11D21"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Número de Quartos")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))

Fr<-table(banco_tcc$QUARTO)
#Determinando as porcentagens de cada classe
Pr<-as.data.frame(round(prop.table(Fr), digits=4)*100)
colnames(Pr)<-c("Var1", "Pr")
comp<-merge(Fr, Pr, by="Var1")
comp$Pr<-paste(gsub("\\.",",",comp$Pr), "%", sep= '')

ggplot(comp, aes(x=Var1, y=Freq, label=Pr)) +
  geom_bar(stat="identity", fill="#A11D21") +
  geom_text(vjust=-0.5, size=4)+
  labs(x="Número de quartos", y="Frequência") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) 

#grid.arrange(a,b , nrow=2)

xtable(table(QUARTO))

## Quarto por bairro

ggplot(banco_tcc, aes(x=BAIRRO, y=QUARTO)) +
  geom_boxplot(fill=c("#A11D21"), width = 0.5) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="Bairro", y="Número de Quartos") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black")) 

datafalt <- data.frame(BAIRRO,QUARTO)
med_por_bairro <- ddply(datafalt,.(BAIRRO),summarise,media=mean(QUARTO),dp=sqrt(var(QUARTO)),vari=var(QUARTO),minimo=min(QUARTO),maximo=max(QUARTO),cv=100*((sqrt(var(QUARTO))/mean(QUARTO))),n=length(QUARTO))
xtable(med_por_bairro)


## Banheiro geral

summary(BANHEIRO)
sd(BANHEIRO)

ggplot(banco_tcc, aes(x=factor(""), y=BANHEIRO)) +
  geom_boxplot(fill=c("#A11D21"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Número de Banheiros")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))

Fr<-table(banco_tcc$BANHEIRO)
#Determinando as porcentagens de cada classe
Pr<-as.data.frame(round(prop.table(Fr), digits=4)*100)
colnames(Pr)<-c("Var1", "Pr")
comp<-merge(Fr, Pr, by="Var1")
comp$Pr<-paste(gsub("\\.",",",comp$Pr), "%", sep= '')

ggplot(comp, aes(x=Var1, y=Freq, label=Pr)) +
  geom_bar(stat="identity", fill="#A11D21") +
  geom_text(vjust=-0.5, size=4)+
  labs(x="Número de banheiros", y="Frequência") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) 

#grid.arrange(a,b , nrow=2)

xtable(table(BANHEIRO))


## Banheiro por bairro

ggplot(banco_tcc, aes(x=BAIRRO, y=BANHEIRO)) +
  geom_boxplot(fill=c("#A11D21"), width = 0.5) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="Bairro", y="Número de Banheiros") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black")) 

datafalt <- data.frame(BAIRRO,BANHEIRO)
med_por_bairro <- ddply(datafalt,.(BAIRRO),summarise,media=mean(BANHEIRO),dp=sqrt(var(BANHEIRO)),vari=var(BANHEIRO),minimo=min(BANHEIRO),maximo=max(BANHEIRO),cv=100*((sqrt(var(BANHEIRO))/mean(BANHEIRO))),n=length(BANHEIRO))
xtable(med_por_bairro)


## Suite geral

summary(SUITE)
sd(SUITE)

ggplot(banco_tcc, aes(x=factor(""), y=SUITE)) +
  geom_boxplot(fill=c("#A11D21"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Número de Suítes")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))

Fr<-table(banco_tcc$SUÍTE)
#Determinando as porcentagens de cada classe
Pr<-as.data.frame(round(prop.table(Fr), digits=4)*100)
colnames(Pr)<-c("Var1", "Pr")
comp<-merge(Fr, Pr, by="Var1")
comp$Pr<-paste(gsub("\\.",",",comp$Pr), "%", sep= '')

ggplot(comp, aes(x=Var1, y=Freq, label=Pr)) +
  geom_bar(stat="identity", fill="#A11D21") +
  geom_text(vjust=-0.5, size=4)+
  labs(x="Número de suítes", y="Frequência") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) 

#grid.arrange(a,b , nrow=2)

xtable(table(SUITE))

## Suite por bairro

ggplot(banco_tcc, aes(x=BAIRRO, y=SUITE)) +
  geom_boxplot(fill=c("#A11D21"), width = 0.5) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="Bairro", y="Número de Suítes") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black")) 

datafalt <- data.frame(BAIRRO,SUITE)
med_por_bairro <- ddply(datafalt,.(BAIRRO),summarise,media=mean(SUITE),dp=sqrt(var(SUITE)),vari=var(SUITE),minimo=min(SUITE),maximo=max(SUITE),cv=100*((sqrt(var(SUITE))/mean(SUITE))),n=length(SUITE))
xtable(med_por_bairro)


## Vaga geral

summary(VAGA)
sd(VAGA)

ggplot(banco_tcc, aes(x=factor(""), y=VAGA)) +
  geom_boxplot(fill=c("#A11D21"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Número de Vagas")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))

Fr<-table(banco_tcc$VAGA)
#Determinando as porcentagens de cada classe
Pr<-as.data.frame(round(prop.table(Fr), digits=4)*100)
colnames(Pr)<-c("Var1", "Pr")
comp<-merge(Fr, Pr, by="Var1")
comp$Pr<-paste(gsub("\\.",",",comp$Pr), "%", sep= '')

ggplot(comp, aes(x=Var1, y=Freq, label=Pr)) +
  geom_bar(stat="identity", fill="#A11D21") +
  geom_text(vjust=-0.5, size=4)+
  labs(x="Número de vagas", y="Frequência") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) 

#grid.arrange(a,b , nrow=2)

xtable(table(VAGA))

## Vaga por bairro

ggplot(banco_tcc, aes(x=BAIRRO, y=VAGA)) +
  geom_boxplot(fill=c("#A11D21"), width = 0.5) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="Bairro", y="Número de Vagas") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black")) 

datafalt <- data.frame(BAIRRO,VAGA)
med_por_bairro <- ddply(datafalt,.(BAIRRO),summarise,media=mean(VAGA),dp=sqrt(var(VAGA)),vari=var(VAGA),minimo=min(VAGA),maximo=max(VAGA),cv=100*((sqrt(var(VAGA))/mean(VAGA))),n=length(VAGA))
xtable(med_por_bairro)


## Valor geral

summary(VALOR)
sd(VALOR)

ggplot(banco_tcc, aes(x=factor(""), y=VALOR)) +
  geom_boxplot(fill=c("#A11D21"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Valor dos imóveis")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))

## Valor por bairro

ggplot(banco_tcc, aes(x=BAIRRO, y=VALOR)) +
  geom_boxplot(fill=c("#A11D21"), width = 0.5) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="Bairro", y="Valor dos imóveis") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black")) 

datafalt <- data.frame(BAIRRO,VALOR)
med_por_bairro <- ddply(datafalt,.(BAIRRO),summarise,media=mean(VALOR),dp=sqrt(var(VALOR)),vari=var(VALOR),minimo=min(VALOR),maximo=max(VALOR),cv=100*((sqrt(var(VALOR))/mean(VALOR))),n=length(VALOR))
xtable(med_por_bairro)


## Valor do m² geral

summary(VALOR_M2)
sd(VALOR_M2)

ggplot(banco_tcc, aes(x=factor(""), y=VALOR_M2)) +
  geom_boxplot(fill=c("#A11D21"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Valor do m² dos imóveis")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))

## Valor do m² por bairro

ggplot(banco_tcc, aes(x=BAIRRO, y=VALOR_M2)) +
  geom_boxplot(fill=c("#A11D21"), width = 0.5) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="Bairro", y="Valor do m² dos imóveis") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black")) 

datafalt <- data.frame(BAIRRO,VALOR_M2)
med_por_bairro <- ddply(datafalt,.(BAIRRO),summarise,media=mean(VALOR_M2),dp=sqrt(var(VALOR_M2)),vari=var(VALOR_M2),minimo=min(VALOR_M2),maximo=max(VALOR_M2),cv=100*((sqrt(var(VALOR_M2))/mean(VALOR_M2))),n=length(VALOR_M2))
xtable(med_por_bairro)


## Analise Bidimensional

banco_tcc_quanti <- banco_tcc[,c(9:15)]

cor(banco_tcc_quanti)

plot(banco_tcc_quanti,col="#A11D21")

M <- cor(banco_tcc_quanti)
corrplot(M, method = "number")

corrgram(banco_tcc_quanti, lower.panel = panel.pts, 
         upper.panel= panel.conf, diag.panel = panel.density, col=4)



################################################################################


#Definindo banco treino e banco validação

set.seed(180026798) 
amostra <- sample(1:359, size = 200, replace = F)
base.treino <- banco_tcc[amostra,]
base.valida <- banco_tcc[-amostra,]
#write.csv2(base.treino, "C:/Users/jgararuna/Downloads/TCC/base.treino.csv")
#write.csv2(base.valida, "C:/Users/jgararuna/Downloads/TCC/base.valida.csv")

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

mod.treino <- lm(VALOR_t~AREA_t+QUARTO_t+BANHEIRO_t+SUITE_t+VAGA_t+BAIRRO_t)

a <- summary(mod.treino) # considerando um alpha de 5%, quarto, banheiro e bairro não apresentaram significância
xtable::xtable(a)        # sai a variavel Quarto


# Atualizando modelo (agora sem a variavel quarto)

mod.treino <- lm(VALOR_t~AREA_t+BANHEIRO_t+SUITE_t+VAGA_t+BAIRRO_t)

a <- summary(mod.treino) # considerando um alpha de 5%, banheiro e bairro não apresentaram significância
xtable::xtable(a)        # sai a variável banheiro

# Atualizando modelo (agora sem a variavel quarto e banheiro)

mod.treino <- lm(VALOR_t~AREA_t+SUITE_t+VAGA_t+BAIRRO_t)

a <- summary(mod.treino) # considerando um alpha de 5%, bairro não apresentarou significância
xtable::xtable(a)        # sai a variavel bairro

# Atualizando modelo (agora sem a variavel quarto, banheiro e bairro)

mod.treino <- lm(VALOR_t~AREA_t+SUITE_t+VAGA_t)

a <- summary(mod.treino) # todas apresentaram significancia
xtable::xtable(a)


# Ánalise de diagnóstico do mod.treino final:

par(mfrow=c(1,2))

plot(mod.treino$fitted.values,mod.treino$residuals,pch=16, 
     xlab = "Valores ajustados",
     ylab = "Resíduos", col = "#A11D21")
abline(h=0,col=1)
abline(h=-4e+05,col=1)
abline(h=4e+05,col=1)
identify(mod.treino$fitted.values,mod.treino$residuals) # encontra-se 7 outliers, cujas observações são:  5 11 28 37 55 149 173

plot(mod.treino$residuals,pch=16, xlab = "", ylab = "Resíduos",
     col = "#A11D21")
abline(h=0,col=1)
abline(h=-4e+05,col=1)
abline(h=4e+05,col=1)
identify(mod.treino$residuals) # encontra-se 7 outliers, cujas observações são:  5 11 28 37 55 149 173


par(mfrow=c(2,4))

plot(AREA_t,mod.treino$residuals,pch=16,xlab = "Área", ylab = "Resíduos", col=4)
abline(h=0,col=2)
abline(h=-4e+05,col=2)
abline(h=4e+05,col=2)
identify(AREA_t,mod.treino$residuals) # encontra-se 7 outliers, cujas observações são:  5 11 28 37 55 149 173

plot(QUARTO_t,mod.treino$residuals,pch=16,xlab = "Quarto", 
     ylab = "Resíduos", col=4)
abline(h=0,col=2)
abline(h=-4e+05,col=2)
abline(h=4e+05,col=2)
identify(QUARTO_t,mod.treino$residuals) # encontra-se 6 outliers, cujas observações são:  5 11 28 37 55 173

plot(BANHEIRO_t,mod.treino$residuals,pch=16,xlab = "Banheiro", 
     ylab = "Resíduos", col=4)
abline(h=0,col=2)
abline(h=-4e+05,col=2)
abline(h=4e+05,col=2)
identify(BANHEIRO_t,mod.treino$residuals) # encontra-se 7 outliers, cujas observações são:  5 11 28 37 55 149 173

plot(SUITE_t,mod.treino$residuals,pch=16,xlab = "Suíte",
     ylab = "Resíduos", col=4)
abline(h=0,col=2)
abline(h=-4e+05,col=2)
abline(h=4e+05,col=2)
identify(SUITE_t,mod.treino$residuals) # encontra-se 7 outliers, cujas observações são:  5 11 28 37 55 149 173

plot(VAGA_t,mod.treino$residuals,pch=16,xlab = "Vaga", 
     ylab = "Resíduos", col=4)
abline(h=0,col=2)
abline(h=-4e+05,col=2)
abline(h=4e+05,col=2)
identify(VAGA_t,mod.treino$residuals) # encontra-se 7 outliers, cujas observações são:  5 11 28 37 55 149 173


# Verificando normalidade:

par(mfrow=c(1,2))

stud_treino <- rstudent(mod.treino)

hist(stud_treino, xlab = "y", ylab = "Frequência", main = "", col = "#A11D21") # por esse histograma, o modelo não parece ter uma distribuição normal,
                                                                   # visto que os dados não são simétricos em torno do zero.

qqnorm(mod.treino$residuals,col="#A11D21",xlab = "Quantis Teóricos",  ylab = "Quantis Amostrais", main = "")
qqline(mod.treino$residuals,col=1) # por esse gráfico, é possível notar que muitas observações (a maioria delas) estão em cima da reta. 
                                   # Dessa forma, há indícios de que os dados são normais. Porém, tem-se pontos aue estao fora da reta,
                                   # alguns deles muitos distantes, o que pode resultar em uma não normalidade

ols_plot_resid_qq(mod.treino)


shapiro.test(mod.treino$residuals) # Por meio desse teste, é possível notar um p-valor menor que 5% (nivel de confiança considerado), 
                                   # portanto há evidências suficientes para rejeitar h0, ouseja, os dados não possuem uma distribuição
                                   # normal.


# Verificando independencia dos erros:

dwtest(mod.treino) # Por meio desse teste, nota-se um p-valor maior que 5% (nivel de confiança considerado), portanto não há evidências 
                   # suficientes para rejeitar h0, ou seja, os dados são independentes.

# Verificando homogeneidade da variância:

bptest(mod.treino) # Por meio desse teste, nota-se um p-valor menor que 5% (nivel de confiança considerado), portanto há evidências 
                   # suficientes para rejeitar h0, ou seja, os dados são não possuem variância homogênea.

# TALVEZ CONSTRUIR UM NOVO MODELO SEM OS OUTLIERS E FAZER A MESMA ANALISE DE DISGNOTICO!


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


g <- (vi_treino <- vif(mod.treino))
xtable::xtable(g)
mean(vi_treino)



###############################################################################



# Verificando necessidade de transformação na variável y:

boxcox(mod.treino,lambda = seq(-2,2,by=0.5), 
       ylab = "Log-Verossimilhança") #lambda =~ 0.5 

lambda <- 0.5
VALOR_t_tranformado <- sqrt(VALOR_t)

# Análise bidimensional:

banco_treino_transformado <- data.frame(cbind(VALOR_t_tranformado, AREA_t,QUARTO_t,BANHEIRO_t,SUITE_t,VAGA_t,BAIRRO_t))

banco_treino_transformado$VALOR_t_tranformado <- as.numeric(banco_treino_transformado$VALOR_t_tranformado)
banco_treino_transformado$AREA_t <- as.numeric(banco_treino_transformado$AREA_t)
banco_treino_transformado$QUARTO_t <- as.numeric(banco_treino_transformado$QUARTO_t)
banco_treino_transformado$BANHEIRO_t <- as.numeric(banco_treino_transformado$BANHEIRO_t)
banco_treino_transformado$SUITE_t <- as.numeric(banco_treino_transformado$SUITE_t)
banco_treino_transformado$VAGA_t <- as.numeric(banco_treino_transformado$VAGA_t)

c <- cor(banco_treino_transformado[,-c(7)])
xtable::xtable(c)
plot(banco_treino_transformado[,-c(7)],col=4)

M <- cor(banco_treino_transformado[,-c(7)])
corrplot(M, method = "number") # nota-se algumas variaveis com correlações bastante altas, como:
                               # valor_t_transformado e area_t / valor_t_transformado e quarto_t / valor_t_transformado e banheiro_t
                               # area_t e quarto_t / area_t e banheiro_t
                               # quarto_t e banheiro_t
                               # os valores dessas correlações ficaram maiores que as do conjunto de dados sem a transformação na variavel y

corrgram(banco_treino_transformado[,-c(8)], lower.panel = panel.pts,
         upper.panel= panel.conf, diag.panel = panel.density)


# Criando o modelo completo transformado:

mod.treino_transformado <- lm(VALOR_t_tranformado~AREA_t+QUARTO_t+BANHEIRO_t+SUITE_t+VAGA_t+BAIRRO_t)
b <- summary(mod.treino_transformado) # quarto_t e bairro_t passaram a ser significantes
                                      # suite_t perdeu significancia
xtable::xtable(b)


# Fazendo a análise de diagnóstico do modelo completo transformado:

par(mfrow=c(1,2))

plot(mod.treino_transformado$fitted.values,mod.treino_transformado$residuals,pch=16, col = 4, 
     xlab = "y tranformado ajustado (y.)", ylab = "Resíduos")
abline(h=0,col=2)
abline(h=-200,col=2)
abline(h=200,col=2)
identify(mod.treino_transformado$fitted.values,mod.treino_transformado$residuals) # há a presença de 5 outliers:  5  8 28 33 55

plot(mod.treino_transformado$residuals,pch=16,col=4,ylab = "Resíduos", xlab = "")
abline(h=0,col=2)
abline(h=-200,col=2)
abline(h=200,col=2)
identify(mod.treino_transformado$residuals) # há a presença de 5 outliers:  5  8 28 33 55


# Verificando a normalidade:

par(mfrow=c(1,1))

hist(VALOR_t_tranformado, xlab = "y transformado", ylab = "Frequência", main = "", col = 4) # continuam não sendo simetrico, indicando, assim, uma nao normalidade dos dados

qqnorm(mod.treino_transformado$residuals,col=4,xlab = "Quantis Teóricos", ylab = "Quantis Amostrais",
       main = "")
qqline(mod.treino_transformado$residuals,col=2)

ols_plot_resid_qq(mod.treino_transformado) # nota-se muitos pontos sobre a reta, mas também ha muito pontos fora 
                                           # da reta, alguns destes estão bastante distantes até, indicando uma possível nao normalidade dos dados

shapiro.test(mod.treino_transformado$residuals) # nota-se um p-valor menor que o nivel de significancia de 5%, portanto, há evidencias
                                                # suficientes para rejeitar h0, logo, os dados não seguem um distribuição normal

# Verificando independencia:

dwtest(mod.treino_transformado) # nota-se um p-valor maior que o nivel de significancia de 5%, portanto, não há evidencias
                                # suficientes para rejeitar h0, logo, os dados são independentes

# Verificando a homogeneidade da variância:

bptest(mod.treino_transformado) # nota-se um p-valor maior (praticamente igual) que o nivel de significancia de 5%, portanto, não há evidencias
                                # suficientes para rejeitar h0, logo, as variâncias são iguais


# Vantagens: 
# > duas variáveis passaram a ser significantes
# > o pressuposto da homogeneidade das variancias passou
# > numero de outliers diminuiu

# VER COM O PROFESSOR!



################################################################################



# Seleção de variáveis

# Para ver qual o número melhor de variáveis pra entrar no modelo:

mod.treino <- lm(VALOR_t~AREA_t+QUARTO_t+BANHEIRO_t+SUITE_t+VAGA_t+BAIRRO_t)

k <- ols_step_all_possible(mod.treino)
plot(k) # analisando os gráficos das medidas que penalizam a quantidade de variáveis (CP, AIC, BIC e o SBC), percebe-se que
        # as quantidade de variáveis para entrar no modelo 2, 3, 4 e 5 ficaram com valores muito próximos


# Ver quais os melhores modelos pra cada quantidade de variáveis selecionada anteriormente:

base.treino_teste <- base.treino[,c(8:14)]

sele1 <- regsubsets(base.treino_teste$VALOR~., data=base.treino_teste, nbest = 3)
summary(sele1) 

b <- cbind(summary(sele1)$which,summary(sele1)$rsq,summary(sele1)$adjr2, summary(sele1)$cp,summary(sele1)$bic)
# Nota-se que os melhores modelos, para cada quantidade de variáveis selecionada anteriormente, são:
# > com 2 variáveis: VALOR_t = INTERCEPTO + AREA_t + VAGA_t
# > com 3 variáveis: VALOR_t = INTERCEPTO + AREA_t + SUITE_t + VAGA_t 
# > com 4 variáveis: VALOR_t = INTERCEPTO + BAIRRO_t + AREA_t + SUITE_t + VAGA_t 
# > com 5 variáveis: VALOR_t = INTERCEPTO + BAIRRO_t + AREA_t + BANHEIRO_t + SUITE_t + VAGA_t
xtable::xtable(b[7:12,])


# Análise de diagnóstico do modelo com 2 variáveis:

mod_2 <- lm(VALOR_t~AREA_t + VAGA_t)
summary(mod_2)

# Resíduo excluído studentizado

stud_2 <- rstudent(mod_2)

par(mfrow=c(1,2))

plot(mod_2$fitted.values,stud_2,pch=16, xlab = "y ajustado",
     ylab = "Resíduo studentizado", col=4)
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)
identify(mod_2$fitted.values,stud_2) # nota-se a presença de 9 outliers, cujas observações são: 5  11  28  37  55 121 149 173 179

plot(stud_2,pch=16, col=4, ylab = "Resíduo studentizado", xlab = "")
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)
identify(stud_2) # nota-se a presença de 9 outliers, cujas observações são: 5  11  28  37  55 121 149 173 179

# Verificando normalidade:

par(mfrow=c(1,1))

hist(stud_2, xlab = "Resíduo studentizado", ylab = "Frequência", main = "", col = 4) # aparenta ser simétrico em torno do zero, apresentando
                                                                                     # indícios de que os dados são normais

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
indice <- c(1:n_t)

rexc_2 <- mod_2$residuals/(1-medinflu_2$infmat[,7]) # ?
plot(indice,rexc_2)

PRESS_2 <- sum((rexc_2)^2)

rstandard(mod_2,type="predictive")
sum((rstandard(mod_2,type="predictive"))^2) # isso é o erro de previsão, o erro equivalente ao PRESS


# Análise de diagnóstico do modelo com 3 variáveis:

mod_3 <- lm(VALOR_t~AREA_t + SUITE_t + VAGA_t)
summary(mod_3)

# Resíduo excluído studentizado

stud_3 <- rstudent(mod_3)

par(mfrow=c(1,2))

plot(mod_3$fitted.values,stud_3,pch=16, xlab = "y ajustado",
     ylab = "Resíduo studentizado", col=4)
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)
identify(mod_3$fitted.values,stud_3) # nota-se a presença de 7 outliers, cujas observações são: 5 11 28 37 55 149 173

plot(stud_3,pch=16, col=4, ylab = "Resíduo studentizado", xlab = "")
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)
identify(stud_3) # nota-se a presença de 7 outliers, cujas observações são: 5 11 28 37 55 149 173

# Verificando normalidade:

par(mfrow=c(1,1))

hist(stud_3, xlab = "Resíduo studentizado", ylab = "Frequência", main = "", col = 4) # aparenta ser simétrico em torno do zero, apresentando
                                                                                     # indícios de que os dados são normais

qqnorm(stud_3,col=4,xlab = "Quantis Teóricos", ylab = "Quantis Amostrais",
       main = "")
qqline(stud_3,col=2)

ols_plot_resid_qq(mod_3) # nota-se que a maioria dos pontos estão sobre a reta, indicando que os dados, provavelmente, são normais.
                         # Porém, também há uma quantidade consideravel de pontos fora da reta, alguns deles muito distantes até, o que pode prejudicar a validação desse pressuposto

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
indice <- c(1:n_t)

rexc_3 <- mod_3$residuals/(1-medinflu_3$infmat[,8])
plot(indice,rexc_3)

PRESS_3 <- sum((rexc_3)^2)

rstandard(mod_3,type="predictive")
sum((rstandard(mod_3,type="predictive"))^2)


# Análise de diagnóstico do modelo com 4 variáveis:

mod_4 <- lm(VALOR_t~AREA_t + BAIRRO_t + SUITE_t + VAGA_t)
summary(mod_4) # BAIRO_t não passou

# Resíduo excluído studentizado

stud_4 <- rstudent(mod_4)

par(mfrow=c(1,2))

plot(mod_4$fitted.values,stud_4,pch=16, xlab = "y ajustado",
     ylab = "Resíduo studentizado", col=4)
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)
identify(mod_4$fitted.values,stud_4) # nota-se a presença de 7 outliers, cujas observações são: 5  11  28  37  55 149 173

plot(stud_4,pch=16, col=4, ylab = "Resíduo studentizado", xlab = "")
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)
identify(stud_4) # nota-se a presença de 7 outliers, cujas observações são: 5  11  28  37  55 149 173

# Verificando normalidade:

par(mfrow=c(1,1))

hist(stud_4, xlab = "Resíduo studentizado", ylab = "Frequência", main = "", col = 4) # aparenta ser simétrico em torno do zero, apresentando
                                                                                     # indícios de que os dados são normais

qqnorm(stud_4,col=4,xlab = "Quantis Teóricos", ylab = "Quantis Amostrais",
       main = "")
qqline(stud_4,col=2)

ols_plot_resid_qq(mod_4) # nota-se que a muitos pontos estão sobre a reta, indicando que os dados, provavelmente, são normais.
                         # porém, há muitos pontos fora da reta tbm, e alguns deles muito distantes, podendo assim prejudicar a validação desse pressuposto

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
indice <- c(1:n_t)

rexc_4 <- mod_4$residuals/(1-medinflu_4$infmat[,8])
plot(indice,rexc_4)

PRESS_4 <- sum((rexc_4)^2)

rstandard(mod_4,type="predictive")
sum((rstandard(mod_4,type="predictive"))^2)


# Análise de diagnóstico do modelo com 5 variáveis:

mod_5 <- lm(VALOR_t~AREA_t + BAIRRO_t + BANHEIRO_t + SUITE_t + VAGA_t)
summary(mod_5)

# Resíduo excluído studentizado

stud_5 <- rstudent(mod_5)

par(mfrow=c(1,2))

plot(mod_5$fitted.values,stud_5,pch=16, xlab = "y ajustado",
     ylab = "Resíduo studentizado", col=4)
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)
identify(mod_5$fitted.values,stud_5) # nota-se a presença de 7 outliers, cujas observações são: 5  11  28  37  55 149 173

plot(stud_5,pch=16, col=4, ylab = "Resíduo studentizado", xlab = "")
abline(h=0,col=2)
abline(h=-2,col=2)
abline(h=2,col=2)
identify(stud_5) # nota-se a presença de 7 outliers, cujas observações são: 5  11  28  37  55 149 173

# Verificando normalidade:

par(mfrow=c(1,1))

hist(stud_5, xlab = "Resíduo studentizado", ylab = "Frequência", main = "", col = 4) # aparenta ser simétrico em torno do zero, apresentando
                                                                                     # indícios de que os dados são normais

qqnorm(stud_5,col=4,xlab = "Quantis Teóricos", ylab = "Quantis Amostrais",
       main = "")
qqline(stud_5,col=2)

ols_plot_resid_qq(mod_5) # nota-se que a maioria dos pontos estão sobre a reta, indicando que os dados, provavelmente, são normais.
                         # porém, há muitos pontos fora da reta tbm, e alguns deles muito distantes, podendo assim prejudicar a validação desse pressuposto

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
indice <- c(1:n_t)

rexc_5 <- mod_5$residuals/(1-medinflu_5$infmat[,10])
plot(indice,rexc_5)

PRESS_5 <- sum((rexc_5)^2)

rstandard(mod_5,type="predictive")
sum((rstandard(mod_5,type="predictive"))^2)


################################################################################



# Realizando os Método Automáticos:

##Forward##

BAIRRO_t <- as.factor(base.treino_teste$BAIRRO)
AREA_t <- base.treino_teste$ÁREA
QUARTO_t <- base.treino_teste$QUARTO
BANHEIRO_t <- base.treino_teste$BANHEIRO
SUITE_t <- base.treino_teste$SUÍTE
VAGA_t <- base.treino_teste$VAGA
VALOR_t <- base.treino_teste$VALOR
VALORM2_t <- base.treino_teste$VALORM2
n_t <- length(VALOR_t)

modmin <- lm(VALOR_t~1,data=base.treino_teste)
step(modmin, direction = 'forward',
     scope = ( ~ BAIRRO_t + AREA_t + QUARTO_t + BANHEIRO_t + SUITE_t + VAGA_t))

#(Intercept)       AREA_t       VAGA_t      SUITE_t  
#-141932        10504       208569        49058  

# Resultado do forward:
modfor <- lm(VALOR_t~AREA_t + VAGA_t +  SUITE_t)
summary(modfor) 


##Backward##

mod. <- lm(VALOR_t~BAIRRO_t + AREA_t + VAGA_t +  SUITE_t + QUARTO_t + BANHEIRO_t)
step(mod.,direction = 'backward')

#(Intercept)       AREA_t       VAGA_t      SUITE_t  
#-141932        10504       208569        49058 

# Resultado do backward:
modback <- lm(VALOR_t~AREA_t + VAGA_t +  SUITE_t)
summary(modback) # mesma coisa do FORWARD


##Stepwise##

modmin <- lm(VALOR_t~1,data=base.treino_teste)
step(modmin,scope = list(lower = modmin, upper = mod.),direction = 'both')

#(Intercept)       AREA_t       VAGA_t      SUITE_t  
#-141932        10504       208569        49058  

#resultado do stepwise:
modstep <- lm(VALOR_t~AREA_t + VAGA_t +  SUITE_t)
summary(modstep) # mesma coisa do FORWARD e BACKWARD

# PORTANTO, PELOS METODOS AUTOMATICOS (TODOS CONVERGIRAM PARA O MESMO MODELO) O MELHOR MODELO É O COM 3 VARIAVEIS: 
# VALOR_t~AREA_t + VAGA_t +  SUITE_t 

# Ánálise de diagnóstico do modelo selecionado pelos metodos automaticos: mesma analise feita no mod_3


################################################################################

# ANALISE DO MOD_3 (SELECIONADO PELOS METODOS AUTOMATICOS) SEM OS VALORES INFLUENTES/OUTLIERS

base.treino_teste <- base.treino[-c(5,11,28,31,37,55,63,149,167,173,174),]

BAIRRO_t <- as.factor(base.treino_teste$BAIRRO)
AREA_t <- base.treino_teste$ÁREA
QUARTO_t <- base.treino_teste$QUARTO
BANHEIRO_t <- base.treino_teste$BANHEIRO
SUITE_t <- base.treino_teste$SUÍTE
VAGA_t <- base.treino_teste$VAGA
VALOR_t <- base.treino_teste$VALOR
n_t <- length(VALOR_t)

mod_3 <- lm(VALOR_t~AREA_t + SUITE_t + VAGA_t)
shapiro.test(mod_3$residuals)
dwtest(mod_3)
bptest(mod_3) 
# TODOS OS PRESSUPOSTOS PASSARAM!!!!!!!!!!!!


# Testes
mod_2 <- lm(VALOR_t~AREA_t + VAGA_t)
shapiro.test(mod_2$residuals)
dwtest(mod_2)
bptest(mod_2) # não passou

mod_4 <- lm(VALOR_t~AREA_t + BAIRRO_t + SUITE_t + VAGA_t)
shapiro.test(mod_4$residuals)
dwtest(mod_4)
bptest(mod_4) 
#todos passaram

mod_5 <- lm(VALOR_t~AREA_t + BAIRRO_t + BANHEIRO_t + SUITE_t + VAGA_t)
shapiro.test(mod_5$residuals)
dwtest(mod_5)
bptest(mod_5) 
#todos passaram