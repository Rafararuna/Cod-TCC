# Definindo o banco de dados
banco_tcc <-  read_excel("C:/Users/jgararuna/Downloads/TCC/base tcc1 - vvr - base limpa pelo python.xlsx")

BAIRRO <- as.factor(banco_tcc$BAIRRO)
AREA <- banco_tcc$ÁREA
QUARTO <- banco_tcc$QUARTO
BANHEIRO <- banco_tcc$BANHEIRO
SUITE <- banco_tcc$SUÍTE
VAGA <- banco_tcc$VAGA
VALOR <- banco_tcc$VALOR
VALOR_M2 <- banco_tcc$VALORM2

# Definindo banco treino e banco validação

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



#####################################################################



# Árovres de Regressão

## Instalando pacotes
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

## Definindo data frame
base.teste <- base.treino[,8:14]
base.teste$BAIRRO <- as.factor(base.teste$BAIRRO)
base.teste <- as.data.frame(base.teste)

## Ajuste da árvore
arvore <- rpart(VALOR~., data = base.teste)
rpart.plot(arvore, type = 2)

## Podando a árvore
melhorCp <- arvore$cptable[which.min(arvore$cptable[,"xerror"]),
                           "CP"]
poda <- prune(arvore, cp = melhorCp)

rpart.plot(poda, type = 2) # não mudou nada

# Plotando o nivel de importancia:
x_dt <- data.frame(arvore$variable.importance)

y_dt <- data.frame("Variável" = c("ÁREA", "BANHEIRO", "QUARTO", "VAGA", 
                                  "SUÍTE", "BAIRRO"), 
                   "Nível de importância" = x_dt[,1])

importances <- tibble(variable = y_dt$Variável,
                      importance = y_dt$Nível.de.importância) %>% 
  arrange(desc(importance))

ggplot(importances %>% 
         top_n(n=20),
       aes(x = reorder(variable, importance),
           y = importance)) +
  geom_bar(stat = "identity", position = "dodge", fill = "#A11D21") +
  coord_flip() +
  ylab("Nível de pureza") +
  xlab("") +
  ggtitle("") +
  guides(fill = "none") +
  scale_fill_gradient(low = "red", high = "blue") + 
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))


## Predição da árvore
base.teste_valida <- base.valida[,8:14]
base.teste_valida$BAIRRO <- as.factor(base.teste_valida$BAIRRO)
base.teste_valida <- as.data.frame(base.teste_valida)

pred_arvore1 <- predict(arvore, base.teste_valida)
pred_arvore2 <- predict(poda, base.teste_valida)

## Cálculo do risco
risco_arvore <- ((base.teste_valida$VALOR-pred_arvore1)/sd(base.teste_valida$VALOR))^2 %>% 
  mean()
risco_arvore # 0.3269247

risco_arvore <- ((base.teste_valida$VALOR-pred_arvore2)/sd(base.teste_valida$VALOR))^2 %>% 
  mean()
risco_arvore # 0.3269247

## OBS: ARVORES SÃO SIMPLES DMS PARA TER UM BOM PODER PREDITIVO


##############################################################################



# Florestas Aleatórias/Random Forest

## Instalando pacotes
install.packages("ranger")
library(ranger)

## Definindo datas frame
base.teste <- base.treino[,8:14]
base.teste$BAIRRO <- as.factor(base.teste$BAIRRO)
base.teste <- as.data.frame(base.teste)
names(base.teste) <- make.names(names(base.teste))

base.teste_valida <- base.valida[,8:14]
base.teste_valida$BAIRRO <- as.factor(base.teste_valida$BAIRRO)
base.teste_valida <- as.data.frame(base.teste_valida)
names(base.teste_valida) <- make.names(names(base.teste_valida))

## Floresta
floresta <- ranger(VALOR~.,
                   data = base.teste,
                   importance = "impurity")

## Predição da floresta
pred_floresta <- predict(floresta,
                         base.teste_valida)

## Plotando as importances
importances <- tibble(variable = names(importance(floresta)),
                      importance = importance(floresta)) %>% 
  arrange(desc(importance))

ggplot(importances %>% 
       top_n(n=20),
     aes(x = reorder(variable, importance),
         y = importance)) +
  geom_bar(stat = "identity", position = "dodge", fill = "#A11D21") +
  coord_flip() +
  ylab("Nível de pureza") +
  xlab("") +
  ggtitle("") +
  guides(fill = "none") +
  scale_fill_gradient(low = "red", high = "blue") + 
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))
## OBS: é válido dizer que a propria medida de importancia apresenta problemas ;
##      se vc tiver duas variaveis que tem uma correlação muito alta, o que acontece é que metade das vezes vai ser usada uma
##      e metade das vezes vai ser usada outra, dessa forma, a importancia dessa variavel cai pela metade basicamente ;
##      entao, resumindo, vc ter variaveis muito correlacionadas e que sao importantes, vai aparecer que elas tem pouca importancia ;
##      entao tem q tomar cuidado com essa medida de importancia
  
## Risco da floresta
risco_floresta <- ((base.teste_valida$VALOR-pred_floresta$predictions)/sd(base.teste_valida$VALOR))^2 %>% 
  mean()
risco_floresta # 0.2153593



#############################################################################################
 


# Redes Neurais

## Instalando pacotes:
install.packages("keras")
library(keras)  
#install_keras()


## Definindo datas frame
base.teste <- base.treino[,8:14]
base.teste$BAIRRO <- as.factor(base.teste$BAIRRO)
base.teste$BAIRRO <- as.numeric(base.teste$BAIRRO)
base.teste <- as.matrix(base.teste)

base.teste_valida <- base.valida[,8:14]
base.teste_valida$BAIRRO <- as.factor(base.teste_valida$BAIRRO)
base.teste_valida$BAIRRO <- as.numeric(base.teste_valida$BAIRRO)
base.teste_valida <- as.matrix(base.teste_valida)

banco_tcc_teste <- banco_tcc[,8:14]
banco_tcc_teste$BAIRRO <- as.factor(banco_tcc_teste$BAIRRO)
banco_tcc_teste$BAIRRO <- as.numeric(banco_tcc_teste$BAIRRO)
banco_tcc_teste<- as.matrix(banco_tcc_teste)


## Rede
modelo_rn <- keras_model_sequential() %>% 
  layer_dense(units = 6, activation = "relu",
              input_shape = 
                dim(base.teste)[2]) %>% 
  layer_dropout(0.2) %>% 
  layer_dense(units = 5, activation = "relu") %>% 
  layer_dropout(0.2) %>% 
  layer_dense(units = 4, activation = "relu") %>% 
  layer_dropout(0.2) %>% 
  layer_dense(units = 3, activation = "relu") %>% 
  layer_dropout(0.2) %>% 
  layer_dense(units = 2, activation = "relu") %>% 
  layer_dropout(0.2) %>% 
  layer_dense(units = 1)

modelo_rn <- keras_model_sequential() %>% 
  layer_dense(units = 6, activation = "relu",
              input_shape = 
                dim(banco_tcc_teste)[2]) %>% 
  layer_dropout(0.2) %>%
  layer_dense(units = 4, activation = "relu") %>%
  layer_dropout(0.2) %>%
  layer_dense(units = 3, activation = "linear") %>%
  layer_dropout(0.2) %>%
  layer_dense(units = 1)

# layer_dense: estamos conectando todo mundo q esta entrando nessa camada com todo mundo
#              que vai entrar na proxima
# layer_dropout: tecnica de regularização ; ele vai eliminar 20% dos neuronios cada vez 
#                q for calculado o gradiente descendente estocástico
# layer_dense: ou seja, os 6 neuronios da primeira camda estara ligada aos 5 neuronios dessa
#              segunda camada, e assim por diante.

# OBS > Na camada de saída, a função de ativação linear pode ser utilizada em problemas de regressão, 
#      já que produz resultados em todo o domínio dos números reais.
#     > 


## Definindo a função objetivo e como ela será minimizada:
## > mostrar como vamos compilar essa rede, como vamos estimar os betas dela
## > primeiro vamos definir a função objetivo, q nesse caso é o "mse"
## > depois definimos o otimizador, ou seja, qual variação vai ser usada de gradiente descendente estocastico
## > a métrica é como eu vou avaliar o desempenho dessa rede, q nesse caso foi pelo erro absoluto;
##   isso é so para analisar o desempenho, nao significa q vamos treinar ela com esse criterio

modelo_rn %>% compile(
  loss = 'mse',
  optimizer = optimizer_rmsprop(),
  metrics = list('mean_absolute_error'))


## Aqui vamos treinar ela, usando o fit
## > validation_split = 0.2: internamente ele vai usar o conjunto de validação para plotar as duas curvas, de treinamente e validação, 
##                           e ver como elas variam ; entao ele vai usar 80% pra treinar e 20% pra avaliar o desempenho

base.teste <- base.treino[,8:14]
base.teste$BAIRRO <- as.factor(base.teste$BAIRRO)
base.teste$BAIRRO <- as.numeric(base.teste$BAIRRO)
base.teste <- as.matrix(base.teste)

base.teste_valida <- base.valida[,8:14]
base.teste_valida$BAIRRO <- as.factor(base.teste_valida$BAIRRO)
base.teste_valida$BAIRRO <- as.numeric(base.teste_valida$BAIRRO)
base.teste_valida <- as.matrix(base.teste_valida)

banco_tcc_teste <- banco_tcc[,8:14]
banco_tcc_teste$BAIRRO <- as.factor(banco_tcc_teste$BAIRRO)
banco_tcc_teste$BAIRRO <- as.numeric(banco_tcc_teste$BAIRRO)
banco_tcc_teste <- as.matrix(banco_tcc_teste)


historico <- modelo_rn %>% fit(
  banco_tcc_teste,
  banco_tcc_teste[,7],
  epochs = 150,
  batchsize = 200,
  validation_split = 0.2, 
  verbose = TRUE,
  callbacks = list(
    callback_early_stopping(monitor = "val_mean_absolute_error", min_delta = 0.01,
                            patience = 25,
                            verbose = 0,
                            mode = "auto",
                            restore_best_weights = TRUE)))

plot(historico)
# o gráfico de cima mostra como a função objetivo esta variando no treinamento e na validação
# o gráfico de baixo mostra como o erro absoluto esta variando no treinamento e na validação


## Calculando a predição
pred_redeneural <- modelo_rn %>% predict(base.teste_valida)

## Calculando o risco
risco_redeneural <- ((base.teste_valida[,7]-pred_redeneural)/sd(base.teste_valida[,7]))^2 %>% 
  mean()
risco_redeneural # 0.04147756 ; 0.1823756 (4,3) ; 0.1052764 (5,4) ; 0.2493833 (5,3)

#######################################################################



# Validação cruzada

## Pacotes
install.packages("caret")
library(caret)

## Banco total
banco_tcc <-  read_excel("C:/Users/jgararuna/Downloads/TCC/base tcc1 - vvr - base limpa pelo python.xlsx")

base.teste <- base.treino[,8:14]
base.teste$BAIRRO <- as.factor(base.teste$BAIRRO)
base.teste <- as.data.frame(base.teste)

set.seed(17032000)

mycontrol <- trainControl(method = "cv", number = 10)


## modelo de gressao
mod_test <- train(VALOR~ÁREA + QUARTO + BANHEIRO + SUÍTE + VAGA + BAIRRO, 
                  data = base.teste,
                  method = "lm",
                  trControl = mycontrol)


## Modelo - Árvore de Regressão
arvore <- rpart(VALOR~., data = base.teste)
rpart.plot(arvore, type = 2)

## Podando a árvore
melhorCp <- arvore$cptable[which.min(arvore$cptable[,"xerror"]),
                           "CP"]
poda <- prune(arvore, cp = melhorCp)

rpart.plot(poda, type = 2) # não mudou nada

mod_arvore <- train(VALOR~., 
                    data = base.teste,
                    method = "rpart",
                    trControl = mycontrol)


## Modelo - Floresta Aleatória

floresta <- ranger(VALOR~.,
                   data = base.teste,
                   importance = "impurity")

mod_floresta <- train(VALOR~., data = base.teste,
                    method = "rf",
                    ntree = 500,
                    tuneGrid = data.frame(mtry = 2),
                    trControl = mycontrol)


## Modelo - Rede Neural
base.teste <- base.treino[,8:14]
base.teste$BAIRRO <- as.factor(base.teste$BAIRRO)
base.teste$BAIRRO <- as.numeric(base.teste$BAIRRO)
base.teste <- as.matrix(base.teste)

mod_rn <- train(VALOR~., data = base.teste,
                      method = "neuralnet",
                      trControl = mycontrol)