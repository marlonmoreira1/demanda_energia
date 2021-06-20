install.packages('knitr')
install.packages('rmarkdown')
tinytex::install_tinytex()
#carregando os pacotes necessarios
library(data.table)
library(C50)
library(caret)
library(lubridate)
library(caTools)
library(rpart)
library(e1071)
library(ggplot2)
library(tidyverse)
library(ggthemes)
library(gridExtra)
library(ModelMetrics)
library(gbm)
library(forecast)
library(knitr)
library(rmarkdown)


#carregando os dados
treino = fread('projeto8-training.csv',stringsAsFactors = F, sep = ',',header = T)

#olhando os dados
View(treino)
str(treino)
summary(treino)

#olhando se há registros faltantes
sum(is.na(treino))

#função para transformar variaveis em fator
to.fator = function(df,colunas) {
  for (coluna in colunas) {
    df[[coluna]] = as.factor(paste(df[[coluna]]))
 }
  return(df) 
}


#função para transformar variaveis em numerica
to.num = function(df,colunas) {
  for (coluna in colunas) {
    df[[coluna]] = as.numeric(paste(df[[coluna]]))
  }
  return(df) 
}

#lista das variaveis categoricas
vars_categorical = c('WeekStatus','Day_of_week')

#lista das variaveis numericas
vars_numerical = c('Appliances','lights','T6','RH_6','T_out','RH_out','Windspeed','Visibility','Tdewpoint','rv1','rv2','NSM')

#aplicandos as funções
treino = to.fator(df = treino, colunas = vars_categorical)
treino = to.num(df = treino, colunas = vars_numerical)

#transformando a variavel date em formato de data,
#pois depois precisarei da hora e do mes
treino$date = ymd_hms(treino$date)

class(treino$date)

#eu vi anteriormente que as variaveis rv1 e rv2 são muito parecidas
alvo = treino[treino$rv1 == treino$rv2,c(28,29)]
#e de fato contém a mesma informação
View(alvo)


#criando novas variaveis de hora e mes
treino$hours = format(treino$date, '%H')
treino$hours = as.numeric(treino$hours)
View(treino$hours)
class(treino$hours)
treino$mes = month(treino$date)
class(treino$mes)

#grafico do gasto de energia por dia
ggplot(treino, aes(x = Day_of_week, 
                    y = rv1)) +
  geom_col(aes(color = Day_of_week,
               fill = Day_of_week)) +
   labs(title = "Gasto de energia por dia",
       x = "Dias da semana",
       y = "Gasto de energia")


#comparação de gasto de energia por dia de semana X fim de semana
ggplot(treino, aes(x = WeekStatus, 
                   y = rv1)) +
  geom_col(aes(color = WeekStatus,
               fill = WeekStatus)) +
  labs(title = "Gasto de energia por dia",
       x = "Dias da semana e fins de semana",
       y = "Gasto de energia")

#gasto de energia por mes
ggplot(treino, aes(x = mes, 
                   y = rv1)) +
  geom_col(aes()) +
  labs(title = "Gasto de energia por mes",
       x = "Meses",
       y = "Gasto de energia")


#gasto de energia por temperatura
#t 20 e 25 h 35 e 40
t1 = treino %>%
  group_by(T1) %>% 
  summarise(gasto_total = sum(rv1)) %>% 
  ggplot(aes(x = T1, y = gasto_total)) +
  geom_line() + 
  ggtitle('Gasto de Energia por temperatura') +
  theme(text = element_text(size = 10))

h1 = treino %>%
  group_by(RH_1) %>% 
  summarise(gasto_total = sum(rv1)) %>% 
  ggplot(aes(x = RH_1, y = gasto_total)) +
  geom_line() + 
  ggtitle('Gasto de Energia por humidade') +
  theme(text = element_text(size = 10))

grid.arrange(t1,h1)



t2 = treino %>%
  group_by(T2) %>% 
  summarise(gasto_total = sum(rv1)) %>% 
  ggplot(aes(x = T2, y = gasto_total)) +
  geom_line() + 
  ggtitle('Gasto de Energia por temperatura') +
  theme(text = element_text(size = 10))

h2 = treino %>%
  group_by(RH_2) %>% 
  summarise(gasto_total = sum(rv1)) %>% 
  ggplot(aes(x = RH_2, y = gasto_total)) +
  geom_line() + 
  ggtitle('Gasto de Energia por humidade') +
  theme(text = element_text(size = 10))

grid.arrange(t2,h2)



t3 = treino %>%
  group_by(T3) %>% 
  summarise(gasto_total = sum(rv1)) %>% 
  ggplot(aes(x = T3, y = gasto_total)) +
  geom_line() + 
  ggtitle('Gasto de Energia por temperatura') +
  theme(text = element_text(size = 10))

h3 = treino %>%
  group_by(RH_3) %>% 
  summarise(gasto_total = sum(rv1)) %>% 
  ggplot(aes(x = RH_3, y = gasto_total)) +
  geom_line() + 
  ggtitle('Gasto de Energia por humidade') +
  theme(text = element_text(size = 10))

grid.arrange(t3,h3)



t4 = treino %>%
  group_by(T4) %>% 
  summarise(gasto_total = sum(rv1)) %>% 
  ggplot(aes(x = T4, y = gasto_total)) +
  geom_line() + 
  ggtitle('Gasto de Energia por temperatura') +
  theme(text = element_text(size = 10))

h4 = treino %>%
  group_by(RH_4) %>% 
  summarise(gasto_total = sum(rv1)) %>% 
  ggplot(aes(x = RH_4, y = gasto_total)) +
  geom_line() + 
  ggtitle('Gasto de Energia por humidade') +
  theme(text = element_text(size = 10))

grid.arrange(t4,h4)


t5 = treino %>%
  group_by(T5) %>% 
  summarise(gasto_total = sum(rv1)) %>% 
  ggplot(aes(x = T5, y = gasto_total)) +
  geom_line() + 
  ggtitle('Gasto de Energia por temperatura') +
  theme(text = element_text(size = 10))

h5 = treino %>%
  group_by(RH_5) %>% 
  summarise(gasto_total = sum(rv1)) %>% 
  ggplot(aes(x = RH_5, y = gasto_total)) +
  geom_line() + 
  ggtitle('Gasto de Energia por humidade') +
  theme(text = element_text(size = 10))

grid.arrange(t5,h5)


t6 = treino %>%
  group_by(T6) %>% 
  summarise(gasto_total = sum(rv1)) %>% 
  ggplot(aes(x = T6, y = gasto_total)) +
  geom_line() + 
  ggtitle('Gasto de Energia por temperatura') +
  theme(text = element_text(size = 10))

h6 = treino %>%
  group_by(RH_6) %>% 
  summarise(gasto_total = sum(rv1)) %>% 
  ggplot(aes(x = RH_6, y = gasto_total)) +
  geom_line() + 
  ggtitle('Gasto de Energia por humidade') +
  theme(text = element_text(size = 10))

grid.arrange(t6,h6)



t7 = treino %>%
  group_by(T7) %>% 
  summarise(gasto_total = sum(rv1)) %>% 
  ggplot(aes(x = T7, y = gasto_total)) +
  geom_line() + 
  ggtitle('Gasto de Energia por temperatura') +
  theme(text = element_text(size = 10))

h7 = treino %>%
  group_by(RH_7) %>% 
  summarise(gasto_total = sum(rv1)) %>% 
  ggplot(aes(x = RH_7, y = gasto_total)) +
  geom_line() + 
  ggtitle('Gasto de Energia por humidade') +
  theme(text = element_text(size = 10))

grid.arrange(t7,h7)



t8 = treino %>%
  group_by(T8) %>% 
  summarise(gasto_total = sum(rv1)) %>% 
  ggplot(aes(x = T8, y = gasto_total)) +
  geom_line() + 
  ggtitle('Gasto de Energia por temperatura') +
  theme(text = element_text(size = 10))

h8 = treino %>%
  group_by(RH_8) %>% 
  summarise(gasto_total = sum(rv1)) %>% 
  ggplot(aes(x = RH_8, y = gasto_total)) +
  geom_line() + 
  ggtitle('Gasto de Energia por humidade') +
  theme(text = element_text(size = 10))

grid.arrange(t8,h8)



t9 = treino %>%
  group_by(T9) %>% 
  summarise(gasto_total = sum(rv1)) %>% 
  ggplot(aes(x = T9, y = gasto_total)) +
  geom_line() + 
  ggtitle('Gasto de Energia por temperatura') +
  theme(text = element_text(size = 10))

h9 = treino %>%
  group_by(RH_9) %>% 
  summarise(gasto_total = sum(rv1)) %>% 
  ggplot(aes(x = RH_9, y = gasto_total)) +
  geom_line() + 
  ggtitle('Gasto de Energia por humidade') +
  theme(text = element_text(size = 10))

grid.arrange(t9,h9)

colunas = c("Appliances","lights",'WeekStatus','Day_of_week')

#função para gerar boxplot de algumas variaveis
graf_boxplot = function(X){ 
  ggplot(treino, aes_string(x = X, y = 'rv1',group=X)) + 
    geom_boxplot()  
    
}

Map(graf_boxplot, colunas)


rotulos = c("date","Appliances","lights","T1",
            "RH_1","T2","RH_2","T3","RH_3","T4",
            "RH_4","T5","RH_5","T6","RH_6","T7",
            "RH_7","T8","RH_8","T9","RH_9","T_out",
            "Windspeed","Visibility","Tdewpoint","rv1",
            "NSM","WeekStatus","Day_of_week")


#função para gerar histogramas
graf_histo = function(X){
  ggplot(treino, aes_string(x = X)) + 
    geom_histogram(color="darkblue", fill="lightblue") +
    geom_density(alpha=.2, fill="#FF6666") 
}
  

Map(graf_histo, rotulos)


times = c(1, 4, 7, 9, 12, 15, 18, 21, 0) 

#criando funções para investigar se a diferença é grande
#em gasto de energia por hora
week_plot = function(times){
  ggplot(treino[treino$hours == times, ], aes(x = WeekStatus, y = rv1)) + 
    geom_bar(stat = "identity", color="darkblue", fill="lightblue") +
    ylab("Gasto de Energia") +
    labs(title = paste("Gasto de energia as ", as.character(times), ":00", sep = "")) +
    theme(text = element_text(size = 15)) 
    
}


lapply(times, week_plot)


day_plot = function(times){
  ggplot(treino[treino$hours == times, ], aes(x = Day_of_week, y = rv1, fill = Day_of_week)) + 
    geom_bar(stat = 'identity') +
    ylab("Gasto de Energia") +
    labs(title = paste("Gasto de energia as ", as.character(times), ":00", sep = "")) +
    theme(text = element_text(size = 12)) +
    theme_light()
}


lapply(times, day_plot)




#indice para dividir os dados em amostras de treino e teste
indice_divide_dados <- sample(x = nrow(treino),
                              size = 0.7 * nrow(treino),
                              replace = FALSE)
View(indice_divide_dados)


dados_treino = treino[indice_divide_dados,]
dados_teste = treino[-indice_divide_dados,]

sum(is.na(dados_treino))
sum(is.na(dados_teste))


#criação dos modelos sem mexer nos dados
modelo_svm = svm(rv1 ~ ., data = dados_treino, na.action = na.omit)
summary(modelo_svm)

svm_pred = predict(modelo_svm,dados_teste)
rmse(svm_pred,dados_teste$rv1)
ce(dados_teste$rv1,svm_pred)
mean(svm_pred == dados_teste$rv1)
mean(dados_teste$rv1 - svm_pred)^2

modelo_lr = glm(rv1 ~ ., data = dados_treino, family = "gaussian")
summary(modelo_lr)
lr_pred = predict(modelo_lr,dados_teste)
rmse(lr_pred,dados_teste$rv1)
ce(dados_teste$rv1,lr_pred)
mean(lr_pred == dados_teste$rv1)
mean(dados_teste$rv1 - lr_pred)^2


dados_treino$date = as.numeric(dados_treino$date)
dados_teste$date = as.numeric(dados_teste$date)

modelo_gb = gbm(rv1 ~ . , data = dados_treino ,distribution = "gaussian",n.trees = 5000,
                 shrinkage = 0.01, interaction.depth = 4)
summary(modelo_gb)

gb_pred = predict(modelo_gb,dados_teste)
rmse(gb_pred,dados_teste$rv1)
ce(dados_teste$rv1,gb_pred)
mean(gb_pred == dados_teste$rv1)
mean(dados_teste$rv1 - gb_pred)^2


str(dados_treino)

#criação de novos modelos com os dados normalizados
#passando as variaveis fatores para numericas para normaliza-las
dados_treino$WeekStatus = ifelse(dados_treino$WeekStatus == 'Weekday', 1, 0)
dados_treino$Day_of_week = ifelse(dados_treino$Day_of_week == 'Monday', 1, 
                                  ifelse(dados_treino$Day_of_week == 'Tuesday', 2, 
                                         ifelse(dados_treino$Day_of_week == 'Wednesday',3,
                                                ifelse(dados_treino$Day_of_week == 'Thursday',4,
                                                       ifelse(dados_treino$Day_of_week == 'Friday',5,
                                                              ifelse(dados_treino$Day_of_week == 'Saturday',6,7))))))

View(dados_treino)

#normalizando os dados
dados_treino_norm = scale(dados_treino, center = T, scale = T)


View(dados_treino_norm)
str(dados_treino_norm)

#No inicio eu havia notado duas variaveis iguais com a mesma informação
#aqui estou retirando das amostras de treino e teste uma dessas variaveis 
dados_treino_norm = dados_treino_norm[,-29]
dados_teste = dados_teste[,-29]

#fazendo um grid pro gradient boosting
grid = expand.grid(n.trees = c(1000,1500), interaction.depth=c(1:3), shrinkage=c(0.01,0.05,0.1), n.minobsinnode=c(20))

#aqui um cv para todos os modelos
ctrl = trainControl(method = "repeatedcv",number = 5, repeats = 2)





modelo_gb2 = train(rv1~.,data = dados_treino_norm,
                                                   method = "gbm", trControl = ctrl, tuneGrid = grid)




print(modelo_gb2)

modelo_lr2 = train(rv1~.,data = dados_treino_norm,
                                                   method = "glm", trControl = ctrl)


modelo_lr2



modelo_svm2 = train(rv1~.,data = dados_treino_norm,
                                                   method = 'svmRadial', trControl = ctrl)


print(modelo_svm2)


#Uma nova tentativa com feature engineering
#optei por fazer o feature engineering usando as 
#variaveis menos importantes presentes nos modelos de
#gradient boosting e logistic regression
gbimp = varImp(modelo_gb2)
lrimp = varImp(modelo_lr2)

plot(gbimp)
plot(lrimp)

#novo dataset com o feature engineering
dados_treino_norm2 = dados_treino_norm[,-c(3,5,8,11,17,20,33)]


View(dados_treino_norm2)


modelo_gb3 = train(rv1~.,data = dados_treino_norm2,
                   method = "gbm", trControl = ctrl, tuneGrid = grid)



print(modelo_gb3)

modelo_lr3 = train(rv1~.,data = dados_treino_norm2,
                   method = "glm", trControl = ctrl)


modelo_lr3



modelo_svm3 = train(rv1~.,data = dados_treino_norm2,
                    method = 'svmRadial', trControl = ctrl)


print(modelo_svm3)


modelo_svm4 = train(rv1~.,data = dados_treino_norm,
                    method = 'svmRadial', trControl = ctrl,
                    tune_Length = 5, preProc = c('center','scale'))


print(modelo_svm4)



dados_teste$WeekStatus = ifelse(dados_teste$WeekStatus == 'Weekday', 1, 0)
dados_teste$Day_of_week = ifelse(dados_teste$Day_of_week == 'Monday', 1, 
                                  ifelse(dados_teste$Day_of_week == 'Tuesday', 2, 
                                         ifelse(dados_teste$Day_of_week == 'Wednesday',3,
                                                ifelse(dados_teste$Day_of_week == 'Thursday',4,
                                                       ifelse(dados_teste$Day_of_week == 'Friday',5,
                                                              ifelse(dados_teste$Day_of_week == 'Saturday',6,7))))))





#feature engineering nos dados de teste
dados_teste2 = dados_teste[,-c(3,5,8,11,17,20,33)]



str(dados_teste)
#normalização dos dados de teste
dados_teste_norm = scale(dados_teste2, center = T, scale = T)

#previsao com o melhor modelo
previsao = predict(modelo_gb3,newdata = dados_teste_norm)

previsao_denorm = (previsao)*(max(dados_teste2)-min(dados_teste2))+min(dados_teste2)



accuracy(previsao_denorm,dados_teste2$rv1)


df = cbind(previsao_denorm,dados_teste2$rv1)
View(df)
