setwd("C:/FCD/BusinessAnalytics/Cap14")
getwd()

library(tidyr)
library(dplyr)
library(data.table)
library(arules)

#Neste segundo script busco mudar um pouco o enfoque
#Invés de tentar prever os itens que serão comprados novamente busco encontrar regras
# de associacação entre diversos pedidos, são regras do tipo:
#Quem compra o produto X também pode se interessar pelo produto Y

dfProdutos <- read.csv("data/products.csv", stringsAsFactors = FALSE)

str(dfProdutos)

dfOrder_prod_train <- fread("data/order_products__train.csv", stringsAsFactors = FALSE)

#O algoritmo que usarei é muito intenso computacionalmente, dessa forma preciso buscar uma forma
# de reduzir a quantidade de dados afim de viabilizar o procedimento dada a limitação de memória em
# meu computador
#Nesse projeto o critério que escolhi foi que todas as transações com mais de 10 itens não seriam
#utilizadas para o algoritmo selecionado (nesse caso o Apriori)
#Também seria possível tentar utilizar algum tipo de Cluster e tentar fazê-lo em todo o conjunto 
# de dados
dfOrder_filter <- dfOrder_prod_train%>%
  filter(add_to_cart_order<10)


dfOrder_filter_names <- inner_join(dfProdutos, dfOrder_filter, by = "product_id")

order_train_pivot <- dfOrder_filter_names%>%
  select(order_id, product_id,add_to_cart_order)


#Dado o formato das informações contidas no dataset original baixado do Kaggle se fez necessário realizar um pivot
#Na tabela de maneira a deixá-la no formato que é esperado pelo algoritmo
dfOrdersPivoted<- pivot_wider(data = order_train_pivot, id_cols = order_id, names_from = add_to_cart_order, values_from = product_id, values_fill = 0, names_sort = TRUE)[,-1]

#Posteriormente convertemos o dataset para o formato de transações que é esperado pelo pacote arules utilizado
transactionsPivFilter <- transactions(dfOrdersPivoted)

#Aqui rodamos o algoritmo apriori, este irá procurar associações entre os pedidos realizados, ou seja, procurar
#Itens que frequentemente são comprados juntos
rules <- apriori(transactionsPivFilter, parameter = list(support = 0.01, confidence = 0.5, maxlen = 4, minlen = 2))

#Após isso iremos buscar as regras ordenadas pelo métrica de support
rules_sort = sort(rules, by = "support", decreasing = TRUE)

#E visualizar as regras
inspect(head(rules_sort,5))


