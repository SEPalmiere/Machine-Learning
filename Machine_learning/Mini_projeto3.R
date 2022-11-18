# Definindo a pasta de trabalho
setwd("C:/Users/Palmiere/Documents/ciencia_dados/Machine_learning")
getwd


# Instalando pacotes necessarios
install.packages("Amelia")
install.packages("caret")
install.packages("dplyr")
install.packages("reshape")
install.packages("randomForest")
install.packages("e1071")
install.packages("ggplot2")

# Carregando os pacotes
library(Amelia)
library(caret)
library(ggplot2)
library(dplyr)
library(reshape)
library(randomForest)
library(e1071)

# Carregando o dataset
dados_clientes <- read.csv("dados/dataset.csv")


# Visualizando os dados e a estrutura do dataset
View(dados_clientes)
str(dados_clientes)
summary(dados_clientes)
dim(dados_clientes)


# Limpeza e Transformação dos Dados

# 1- Removendo a coluna de IDs

dados_clientes$ID <- NULL
dim(dados_clientes)
View(dados_clientes)

# 2- Renomeando a coluna de classe
colnames(dados_clientes)
colnames(dados_clientes)[24] <- "inadimplente"
colnames(dados_clientes)
View(dados_clientes)

# 3- Verificando valores ausentes e removendo do dataset
sapply(dados_clientes, function(x) sum(is.na(x)))
?missmap
missmap(dados_clientes, main = "Valores faltantes observados")
dados_clientes <- na.omit(dados_clientes)

# 4- Convertendo os atributos genero, escolaridade, estado civil e idade para categoricos

colnames(dados_clientes)
colnames(dados_clientes)[2] <- "Genero"
colnames(dados_clientes)[3] <- "Escolaridade"
colnames(dados_clientes)[4] <- "Estado_Civil"
colnames(dados_clientes)[5] <- "Idade"
View(dados_clientes)

# Genero

View(dados_clientes$Genero)
str(dados_clientes$Genero)
summary(dados_clientes$Genero)

?cut
dados_clientes$Genero <- cut(dados_clientes$Genero, 
                             c(0,1,2),
                             labels = c("Masculino", "Feminino"))

View(dados_clientes$Genero)
str(dados_clientes$Genero)
summary(dados_clientes$Genero)

# Escolaridade

View(dados_clientes$Escolaridade)
str(dados_clientes$Escolaridade)
summary(dados_clientes$Escolaridade)

dados_clientes$Escolaridade <- cut(dados_clientes$Escolaridade,
                                   c(0,1,2,3,4),
                                   labels = c("Pos Graduado",
                                              "Graduado",
                                              "Ensino Medio",
                                              "Outros"))
View(dados_clientes$Escolaridade)
str(dados_clientes$Escolaridade)
summary(dados_clientes$Escolaridade)

# Estado Civil
View(dados_clientes$Estado_Civil)
str(dados_clientes$Estado_Civil)
summary(dados_clientes$Estado_Civil)

dados_clientes$Estado_Civil <- cut(dados_clientes$Estado_Civil,
                                   c(-1,0,1,2,3),
                                   labels = c("Desconhecido",
                                              "Casado",
                                              "Solteiro",
                                              "Outro") )

View(dados_clientes$Estado_Civil)
str(dados_clientes$Estado_Civil)
summary(dados_clientes$Estado_Civil)

# IdadeConvertendo a variavel para o tipo fator com faixa etaria

View(dados_clientes$Idade)
str(dados_clientes$Idade)
summary(dados_clientes$Idade)
hist(dados_clientes$Idade)

dados_clientes$Idade <- cut(dados_clientes$Idade,
                            c(0,30,50,100),
                            labels = c("Jovem",
                                       "Adulto",
                                       "Idoso"))


View(dados_clientes$Idade)
str(dados_clientes$Idade)
summary(dados_clientes$Idade)     


View(dados_clientes)

# Convertendo a variavel que indica pagamentos para o tipo fator
dados_clientes$PAY_0 <- as.factor((dados_clientes$PAY_0))
dados_clientes$PAY_2 <- as.factor((dados_clientes$PAY_2))
dados_clientes$PAY_3 <- as.factor((dados_clientes$PAY_3))
dados_clientes$PAY_4 <- as.factor((dados_clientes$PAY_4))
dados_clientes$PAY_5 <- as.factor((dados_clientes$PAY_5))
dados_clientes$PAY_6 <- as.factor((dados_clientes$PAY_6))

# Dataset apos as conversoes

str(dados_clientes)
sapply(dados_clientes, function(x)sum(is.na(x)))
missmap(dados_clientes, main = "Valores Faltantes Observados")
dados_clientes <-na.omit(dados_clientes)
missmap(dados_clientes, main = "Valores Faltantes Observados")
dim(dados_clientes)

View(dados_clientes)


# Alterando a variavel dependente para o tipo fator

str(dados_clientes$inadimplente)
colnames(dados_clientes)
dados_clientes$inadimplente <- as.factor(dados_clientes$inadimplente)
str(dados_clientes$inadimplente)
View(dados_clientes$inadimplente)

# Total de inadimplentes e de não-inadimplentes
table(dados_clientes$inadimplente)


# Verificação da porcentagem entre classes
prop.table(table(dados_clientes$inadimplente))

# Grafico da distribuição
qplot(inadimplente, data = dados_clientes, geom = 'bar') +
        theme(axis.text.x = element_text(angle = 90, hjust= 1))

# Set seed
set.seed(12345)

# Amostragem estratificada
# Seleciona as linhas de acordo com a variavel inadimplente como strata
?createDataPartition
indice <- createDataPartition(dados_clientes$inadimplente, p = 0.75, list = FALSE)
dim(indice)

# Definimos os dados de treinamento como subconjunto do conjunto de dados original
# com numeros de indice e de linha

dados_treino <- dados_clientes[indice,]
table(dados_treino$inadimplente)

# Verificando a porcentagem entre classes
prop.table(table(dados_treino$inadimplente))

# Numero do registros no dataset de treinamento
dim(dados_treino)    

# Comparamos as porcentagens entre as classes de treinamento e dados originais
compara_dados <- cbind(prop.table(table(dados_clientes$inadimplente)),
                       prop.table(table(dados_clientes$inadimplente)))
colnames(compara_dados) <- c("Treinamento", "Original")
compara_dados

# Covertendo colunas em linhas (Melt Data)
?reshape2::melt
melt_compara_dados <- melt(compara_dados)
melt_compara_dados

# Visualizando a distribuição do treinamento vs original
ggplot(melt_compara_dados, aes(x = X1, y = value)) +
  geom_bar(aes(fill = X2), stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust =1))

# Tudo que não esta no dataset de treinamento esta no dataset de teste

dados_teste <- dados_clientes[-indice,]
dim(dados_teste)
dim(dados_treino)

# ###Construindo a primeira versao do Modelo de Machine Learning###
# ____________________________________________________________ #

# Modelo Random Forest#
? randomForest

modelo_v1 <- randomForest(inadimplente ~ ., data = dados_treino)
modelo_v1

# Avaliando o modelo
plot(modelo_v1)

# Previsões com dados de teste

previsoes_v1 <- predict(modelo_v1, dados_teste)

# Confusion Matrix
?caret::confusionMatrix
cm_v1 <- caret::confusionMatrix(previsoes_v1, dados_teste$inadimplente, positive = "1")
cm_v1

# Calculando Precision, Recall e F1-Score (metricas de avaliação do modelo preditivo)
y <- dados_teste$i
y_pred_v1 <- previsoes_v1

precision <- posPredValue(y_pred_v1, y)
precision


recall <- sensitivity(y_pred_v1, y)
recall


F1 <- (2 * precision * recall) / (precision + recall)
F1

# --- SMOTE não funciona nesta versao do R ---#
# Balanceamento de classe
install.packages("DMvR")
library(DMvR)
?SMOTE

# Aplicando o SMOTE (Synthetic Minority Over-sampling Technique)
# https://arxiv.org/pdf/1106.1813.pdf

table(dados_treino$inadimplente)
set.seed(9560)
dados_treino_bal <- SMOTE(inadimplente ~., data = dados_treino)
table(dados_treino$inadimplente)
prop.table(table(dados_treino$inadimplente))

# Importancia das variaveis preditoras para as previsões
View(dados_treino)
varImpPlot(modelo_v1)

# Salvando o modelo em disco
saveRDS(modelo_v1, file = "modelo/modelo_v1.rds")


# Carregando o modelo
modelo_final <- readRDS("modelo/modelo_v3.rds")
