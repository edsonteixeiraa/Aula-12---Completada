                          #Aula 12 - Modelos  ARIMA

library("urca")                                #Carrega Pacote URCA
library(readxl)                                #Carrega Pacote readxl
library(pwt8)                                  #Carrega o pacote PWT8.0


data("pwt8.0")                                 #Carrega os dados elencados "pwt8.0" dispoiníveis no pacote
View(pwt8.0)                                   #Visualiza os dados na tabela pwt8.0


br <- subset(pwt8.0, country=="Brazil", 
             select = c("rgdpna","emp","xr"))  #Cria a tabela "br" com dados das linhas que assumem o valor "country" (país) igual a "Brazil", selecionando as colunas cujas variáveis são "rgdpna" (PIB), "avh" (TRABALHO)  e "xr" (CÂMBIO)

colnames(br) <-  c("PIB","Emprego","Câmbio")   #Renomeia as colunas para PIB, Trabalho e Câmbio

                                        #Separando as variáveis
PIB <- br$PIB[45:62]                    #Cria o vetor para variável PIB                  
EMPREGO <- br$Emprego[45:62]            #Cria o vetor para variável EMPREGO
CAMBIO <- br$Câmbio[45:62]              #Cria o vetor para variável CAMBIO
Anos <- seq(from=1994, to=2011, by=1)   #Cria um vetor para o tempo em anos de 1994 até 2011 


                                    #Analise para o Emprego

plot(PIB, type = "l")                            #Cria gráfico para o PIB
PIB <- ts(PIB, start = 1994, frequency = 1)  #Define como Série Temporal
plot(PIB, main="EVOLUÇÃO PIB", 
     ylab="PIB", 
     xlab="Ano")                                      #Cria gráfico da Série Temporal

acf(PIB)                                          #Função de Autocorrelação
pacf(PIB)                                         ##Função de Autocorrelação Parcial
reglinPIB <- lm(PIB ~ Anos)                       #Regressão linear simples do emprego em relação ao tempo
reglinPIB                                             #Exibe os resultados da regressão linear
summary(reglinPIB)
plot(PIB)                                         #Gráfcio dos dados
abline(reglinPIB, col="Blue")                         #Insere a linha de regressão linear estimada


#Removendo Tendência

residuosPIB <- reglinPIB$residuals                    #Salva os resíduos no vetor residuosEMP
reglinPIBres <- lm(residuosPIB ~ Anos)                #Regressão linear dos resíduos em função do tempo
plot(residuosPIB,type="l")                            #Gráfico dos resíduos
abline(reglinPIBres, col="Blue")                      #Insere a linha de regressão linear dos resíduos


#Removendo Tendência por meio da diferença

pdPIB <- diff(PIB)                                #Calcula a primeira diferença da série de dados
diferencaP <- (data.frame(PIB[2:18],pdPIB))       #Exibe a tabela da série original coma diferença <- 
DIFERENCAPIB <- ts(diferencaP, start = 1994, frequency = 1)  #Define serie temporal para a tabela diferenca1
plot(DIFERENCAPIB, plot.type="single", col=c("Black","Green")) #Cria o grafico com as duas series
plot(pdPIB, type="l")                                   #Cria gr´pafico somente para a serie da diferença

#Teste Dick-Fuller Aumentado conferindo se a serie se tornou estacionaria

pdPIB1 <- diff(PIB)                                            #Calculando-se a primeira diferença
TesteDF_PIB1_trend <- ur.df(pdPIB1, "trend", lags = 1)         #Teste DF-DickFuller com drift e com tendencia
summary(TesteDF_PIB1_trend) 

pdPIB2 <- diff(diff(PIB))                                      #Calculando-se a segunda diferença
TesteDF_PIB2_trend <- ur.df(pdPIB2, "trend", lags = 1)         #Teste DF-DickFuller com drift e com tendencia
summary(TesteDF_PIB2_trend)

#Estimando a série temporal

arima123P <- arima(PIB, c(1,2,3))

#ARMA
arima120P <- arima(PIB, c(1,2,0))
arima121P <- arima(PIB, c(1,2,1))
arima122P <- arima(PIB, c(1,2,2))

arima220P<- arima(PIB, c(2,2,0))
arima221P<- arima(PIB, c(2,2,1))
arima222P<- arima(PIB, c(2,2,2))
arima223P<- arima(PIB, c(2,2,3))
#MA
arima021P<- arima(PIB, c(0,2,1))
arima022P<- arima(PIB, c(0,2,2))
arima023P<- arima(PIB, c(0,2,3))
#AR
arima020P<- arima(PIB, c(0,2,3))

#Escolher o melhor modelo com base no menor AIC/BIC
estimacoes <- list(arima123P,arima120P,arima121P,
                   arima122P,arima220P,arima221P,
                   arima222P,arima223P,arima021P, arima022P,
                   arima023P,arima020P)
AIC <- sapply(estimacoes, AIC)
BIC <- sapply(estimacoes, BIC)
Modelo <-c(list("arima123P","arima120P","arima121P",
                "arima122P","arima220P","arima221P",
                "arima222P","arima223P","arima021P","arima022P",
                "arima023P","arima020P")) 
Resultados <- data.frame(Modelo,AIC,BIC)

#Análise para o Câmbio

#Análise para o PIB


