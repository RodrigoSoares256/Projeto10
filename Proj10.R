setwd("C:/FCD/BusinessAnalytics/Cap14")
getwd()

library(tidyr)
library(dplyr)
library(data.table)
library(rpart)
library(caret)
library(randomForest)


dfAisles <- read.csv("data/aisles.csv")
str(dfAisles)
View(dfAisles)

dfDepartments <- read.csv("data/departments.csv")
str(dfDepartments)
#View(dfDepartments)


dfProdutos <- read.csv("data/products.csv")
str(dfProdutos)
#View(dfProdutos)
length(unique(dfProdutos$product_name))

dfOrders <- read.csv("data/orders.csv", stringsAsFactors = TRUE)
str(dfOrders)
#View(dfOrders)

dfOrder_prod_prior <- fread("data/order_products__prior.csv")
str(dfOrder_prod_prior)
#View(dfOrder_prod_prior)

dfOrder_prod_train <- fread("data/order_products__train.csv")
str(dfOrder_prod_train)
#View(dfOrder_prod_train)

##############################################################################################################################################
#Irei iniciar realizando um pivot nos dados para colocar de maneira a facilitar saber quais itens foram
#comprados juntos

order_prod_prior_pivot <- dfOrder_prod_prior%>%
  select(order_id, product_id, add_to_cart_order)%>%
  pivot_wider(names_from = add_to_cart_order, values_from = product_id, values_fill = 0)

View(order_prod_prior_pivot)


order_train_pivot <- dfOrder_prod_train%>%
  select(order_id,product_id,add_to_cart_order)%>%
  pivot_wider(names_from =add_to_cart_order, values_from = product_id, values_fill = 0 )



View(order_train_pivot)

length(unique(order_train_pivot$order_id))

####################################################Análise exploratória###########################

#Iniciando através de uma análise simples podemos começar tentando analisar quais dos produtos são os mais
#consumidos 

######################################################################################
#Isso pode ser feito através de uma simples contagem de frequência

prodMaisConsumidos <-as.data.frame(table(dfOrder_prod_prior$product_id))

maisConsumidosOrdenados <- prodMaisConsumidos%>%
  arrange(desc(Freq))

str(maisConsumidosOrdenados)
dfProdutos$product_id <- as.factor(dfProdutos$product_id)

colnames(maisConsumidosOrdenados) = c("product_id","Freq")

View(maisConsumidosOrdenados)

dfMaisConsumidosCNomes <- inner_join(x = maisConsumidosOrdenados, y = dfProdutos, by ='product_id')

#Aqui temos os produtos mais consumidos
View(dfMaisConsumidosCNomes)

#####################################################################################
#Outro insight que podemos tentar obter aqui são os produto que tem maior recorrência na compra, ou seja
#Quais produtos que os clientes compram recorrentemente mais vezes?

dfMaisRecorrentes <-as.data.frame(table(dfOrder_prod_prior%>%filter(reordered == 1)%>%select(product_id)))
str(dfMaisRecorrentes)
colnames(dfMaisRecorrentes) = c("product_id","Freq")
maisRecorrentesOrd <- dfMaisRecorrentes%>%arrange(desc(Freq))

#Aqui obtemos os itens que são mais recorrentes ordenados
dfMaisRecOrdCNomes <- inner_join(x = maisRecorrentesOrd, y = dfProdutos, by = "product_id")
View(dfMaisRecOrdCNomes)


#####################################################################################
#Também é possível expandir o raciocínio para abarcar quais itens são os mais pedidos
#porém que foram pedidos pela primeira vez

dfPedPrimVez <-as.data.frame(table(dfOrder_prod_prior%>%filter(reordered == 0)%>%select(product_id)))

colnames(dfPedPrimVez) = c("product_id","Freq")
primVezOrdenados <- dfPedPrimVez%>%arrange(desc(Freq))

dfPrimVezOrdCNomes <- inner_join(x = primVezOrdenados, y = dfProdutos, by = "product_id")
View(dfPrimVezOrdCNomes)

######################################################################################
#Continuando a nossa análise exploratória quero entender quais horários do dia possuem maior quantidade
#de pedidos

dfHorariosMaisFrequentes <- as.data.frame(table(dfOrders$order_hour_of_day))%>%arrange(desc(Freq))
View(dfHorariosMaisFrequentes)
dfHorariosMaisFrequentes$Var1 <- as.integer(dfHorariosMaisFrequentes$Var1)
View(dfHorariosMaisFrequentes)
str(dfHorariosMaisFrequentes)
hist(dfOrders$order_hour_of_day, breaks = 24)

#O resultado não é necessariamente uma supresa visto que temos a maior parte do pessoal comprando próximo do
#Horário comercial, a partir da 16 horas as compras começam a diminuir gradativamente até ficarem muito próximas
# de zero durante a madrugada, sem surpresas

########################################################################################
#Continuando a nossa análise temos quais clientes compram mais

View(dfOrder_prod_prior)
View(dfOrders)
colnames(dfOrders)

dfOrderWithCustomer <- inner_join(x = dfOrder_prod_prior, y = dfOrders, by = "order_id")%>%select(order_id, user_id,days_since_prior_order)
View(dfOrderWithCustomer)
dfOrderWithCustomer$order_id <- as.factor(dfOrderWithCustomer$order_id)
dfOrderWithCustomer$user_id <- as.factor(dfOrderWithCustomer$user_id)
View(dfOrderWithCustomer)
str(dfOrderWithCustomer)

#Através disso podemos através do mesmo método verificar quais são os clientes campeões
#Assim podemos calcular os clientes que mais compraram dado o volume de pedidos
View(as.data.frame(table(dfOrderWithCustomer$user_id))%>%arrange(desc(Freq)))

##########################################################################################
#A análise dos clientes que mais compraram baseado no volume de pedidos é interessante porém clientes mais antigos
#podem levar vantagem nesse tipo de análise, para compensar isso farei mais um indicador utilizando a coluna
# que mostra a quantidade de dias desde a última compra

dfUserBuyfaster = dfOrderWithCustomer%>%
  filter(days_since_prior_order > 0)%>%
  group_by(user_id)%>%
  summarise(mean = mean(days_since_prior_order))%>%
  arrange(mean)

View(dfUserBuyfaster)
#Essa análise traz os usuários que compram mais rápido, ou seja, os usuários com a menor 
#média de dias desde a última compra

###########################################################################################
#Há algum dia da semana com mais pedidos

unique(dfOrders$order_dow)

dfOrderWithWeekday <- inner_join(x = dfOrder_prod_prior, y = dfOrders, by = "order_id")%>%select(order_dow)

dfOrderCountWeekDay <- as.data.frame(table(dfOrderWithWeekday))
dfOrderCountWeekDay <- table(dfOrderWithWeekday)

View(dfOrderCountWeekDay)

barplot(dfOrderCountWeekDay, horiz = TRUE, ylab = "Dia", xlab = "Compras")
#Sim, há um padrão nas compras

#############################################################################################
#A partir daqui irei focar na resolução do problema proposto pela competição, basicamente estão
#pedindo para prever quais produtos serão pedidos novamente para isso irei começar dividindo os dados em treino e teste
#A competição menciona um dataset de teste, entretanto este não foi encontrado para download no kaggle
# Assim sendo dividirei o dataset fornecido em treino e teste, de maneira a poder obter as métricas necessárias
# Segundo os organizadores da competição a métrica utilizada é o F1-Score

#Devido à quantidade de arquivos fornecidos e as possibilidades de junção das informações acredito que featura engineering será
#Essencial para obter uma boa performance

#Iniciarei criando um modelo base no qual irei me basear afim de construir os modelos futuros

dfOrder_prod_train$reordered <- as.factor(dfOrder_prod_train$reordered)

index <- createDataPartition(dfOrder_prod_train$reordered, p = 0.70, list = FALSE)

train <- dfOrder_prod_train[index,-1]
test <- dfOrder_prod_train[-index,-1]


modeloRpart <- train(reordered~.,data = train, method = "rpart")

predRPart <- predict(modeloRpart, test[,-3])

confusionMatrix(predRPart,test$reordered)

F_meas(predRPart,test$reordered)

#modeloRF <- train(reordered~., data = train, method = "rf")
#
#predRF <- predict(modeloRF, test[,-3])
#confusionMatrix(predRF, test$reordered)
#F_means(predRF, test$reordered)

#############################################################################

modeloRF <- randomForest(reordered~., data = train, ntree = 50)
predRF <- predict(modeloRF, test[,-3])
confusionMatrix(predRF, test$reordered)

precision(predRF, test$reordered)
recall(predRF, test$reordered)
F_meas(predRF, test$reordered)

modeloXGB <- train(reordered~.,data = train, method = "xgbTree")

####################################################################################
#Engenharia de atributos

dfComplete <- rbind(dfOrder_prod_prior,dfOrder_prod_train)

colnames(dfComplete)

dfAvgReorder <- dfComplete%>%
  group_by(product_id)%>%
  summarise(meanReorder = mean(reordered))

#View(dfAvgReorder)
colnames(dfAvgReorder)

dfCombined <- inner_join(dfComplete,dfAvgReorder,by = 'product_id' )

dfCombined <- inner_join(dfCombined,dfOrders, by = 'order_id')

colnames(dfCombined)

dfAvgReorderUse <- dfCombined%>%
  group_by(user_id)%>%
  summarise(meanReorderUser = mean(reordered))

#View(dfAvgReorderUse)

dfCombined <- inner_join(dfCombined, dfAvgReorderUse, by = 'user_id')


colnames(dfCombined)

dfFeatureEngineer <- dfCombined%>%
  filter(eval_set == "train")%>%
  select(product_id, add_to_cart_order, meanReorder, user_id, order_dow,order_hour_of_day,days_since_prior_order,meanReorderUser,reordered)


dfFeatureEngineer$reordered <- as.factor(dfFeatureEngineer$reordered)

index <- createDataPartition(dfFeatureEngineer$reordered, p = 0.70, list = FALSE)

train <- dfFeatureEngineer[index,]
test <-dfFeatureEngineer[-index,]


modeloGLM2 <- train(reordered~., data = train, method = "glm")

predGLM2 <- predict(modeloGLM2, test[,-9])


confusionMatrix(predGLM2,test$reordered)

F_meas(predGLM2,test$reordered)
#Até o momento me parece que a engenharia de atributos está surtindo bons resultados
# a medida de F1 Score obtida através do pacote caret mostrou um valor bem superior


modeloRF2 <- randomForest(reordered~., train, ntree = 50)

predRF2 <- predict(modeloRF2,test[,-9])

confusionMatrix(predRF2,test$reordered)

F_meas(predRF2,test$reordered)

# O modelo com o random Forest também apresentou uma performance ainda superior

#Irei tentar utilizar o Random Forest para verificar a importância das variáveis

rfImportance <- randomForest(reordered~., train, ntree = 50, importance = TRUE)

varImpPlot(rfImportance)
#AS variáveis = dia da semana, hora do dia parecem ser pouco significativas para o modelo, irei portanto criar um modelo sem elas de maneira a verificar
#Se consigo um resultado satisfatório

colnames(train)

modeloRF3 <- randomForest(reordered ~meanReorder+meanReorderUser+product_id+add_to_cart_order+user_id+days_since_prior_order, data = train, ntree = 50)

predRF3 <- predict(modeloRF3,test[,-9])

confusionMatrix(predRF3,test$reordered)

F_meas(predRF3,test$reordered)
# O resultado, como esperado foi praticamente o mesmo que o resultado obtido antes, o que evidencia mais uma vez que as variáveis retiradas realmente não faziam diferença
#Tentarei novamente a engenharia de atributos e colocarei mais atributos relativos aos produtos, como é o caso da categoria à que pertencem os produtos e verificar se 
# isso melhora a performance dos modelos

colnames(dfFeatureEng2)

dfFeatureEng2 <- inner_join(dfFeatureEngineer, dfProdutos, by = "product_id")

dfFeatureEng2 <- dfFeatureEng2%>%
  select(product_id, add_to_cart_order,meanReorder, user_id, days_since_prior_order, meanReorderUser, department_id, aisle_id, reordered )

#Criando novamente o modelo random Forest

modeloRF4 <- randomForest(reordered ~., data = train, ntree = 50)

predRF4 <- predict(modeloRF4,test[,-9])

confusionMatrix(predRF4,test$reordered)

F_meas(predRF4,test$reordered)
# a performance foi melhorada um pouco















