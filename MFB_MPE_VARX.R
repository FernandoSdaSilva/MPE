##PACOTES
install.packages(c("tidyquant","tidyverse","forecast", "magrittr", "MTS","vars",
"urca","rprojroot", "tbl2xts", "Quandl", "BETS", "texreg","seasonalview"))

#Pacotes para coletar dados
require(tidyquant)
require(Quandl)
require(BETS)
require(quantmod)

#Pacotes para manusear os dados
require(tidyverse)
require(tbl2xts)
require(readxl)

#Pacote para facilitar o uso de arquivos do projeto
require(rprojroot)

#Pacote para estimar o modelo var
require(vars)
require(urca)
require(MTS)
require(forecast)

#Pacote para gerar os gráficos
require(dygraphs)

#Pacote para gerar tabelas dos modelos estimados
require(texreg)

#PACOTES EXTRA
require(seasonalview)

##DEFININDO O DIRETÓRIO
setwd("/Volumes/GoogleDrive/My Drive/MPE_dissertação/modelo/DATABASE_FINAL_REV03")

##IMPORTANDO A BASE DE DADOS DO EXCEL (DATABASE)
db_MPE<-read_excel("MFB_MPE_variaveis_FINAL.xlsx")
View(db_MPE)
summary(db_MPE)

# EPORTANDO A BASE DE DADOS EM .CSV
write.csv(db_MPE, "/Volumes/GoogleDrive/My Drive/MPE_dissertação/modelo/modelo_R/exports_modelo_R/DATABASE_FINAL_exportR.csv")

#Quantidade de observações
n=nrow(db_MPE)
View(n)

#Série temporal da variação dos preços reais dos imóveis (série já deflacionada)
Precos_imoveis = xts(db_MPE$FZ_POA_total_deflacion, order.by = db_MPE$Data)
colnames(Precos_imoveis)="Precos_Imoveis"

#Série temporal da variação da inflação na RMPA
IPCA_POA=xts(db_MPE$IPCA_POA, order.by = db_MPE$Data)
colnames(IPCA_POA)="IPCA_POA"

#Série temporal da variação do custo de construção 
CUB_RS = xts(db_MPE$CUB_RS, order.by=db_MPE$Data)
colnames(CUB_RS)="CUB_RS"

#Série temporal da variação da rentabilidade do aluguel (RentalYield)
RentalYield = xts(db_MPE$Var_RY_BR_Total, order.by=db_MPE$Data)
colnames(RentalYield)="Rental_Yield"

#Série temporal da variação da taxa de financiamento imobiliário
Tx_financ=xts(db_MPE$Var_Tx_financ_imob, order.by=db_MPE$Data)
colnames(Tx_financ)="Tx_financ"

#Série temporal da variação do nível de atividade
IBCR_RS=xts(db_MPE$Var_IBCR_RS, order.by=db_MPE$Data)
colnames(IBCR_RS)="IBCR_RS"

#Série temporal da variação da taxa de desocupação
TX_desocup=xts(db_MPE$Var_Tx_desocup_BR, order.by = db_MPE$Data)
colnames(TX_desocup)="Tx_Desocup"

#Série temporal da variação da expectativa de inflação
Expect_Inflação=xts(db_MPE$Var_expect_infl, order.by = db_MPE$Data)
colnames(Expect_Inflação)="Expect_Inflação"

## ORGANIZAÇÃO E VISUALIZAÇÃO DOS DADOS

##União dos dados em um único objeto
data=xts::merge.xts(Precos_imoveis, IPCA_POA, CUB_RS, RentalYield, Tx_financ, IBCR_RS, TX_desocup, Expect_Inflação)
data=na.omit(data)
data_names=colnames(data)

##Estatística descritiva das séries
statNames = c("Média", "Desvio Padrão", "Assimetria", "Curtose", "Tamanho")
variavNames=c("Preço Imóveis", "IPCA POA", "CUB RS", "Rental Yield", "Tx. Financ.", "IBCR RS",
              "Tx. Desocup.", "Expect. Inflação")
series_stats_data=matrix(data=NA, nrow = length(data_names), ncol = length(statNames))
colnames(series_stats_data)=statNames
rownames(series_stats_data)=variavNames

for (i in 1:nrow(series_stats_data)) {
  series_stats_data[i,1]=round(mean(data[,i], na.rm=TRUE),3) 
  series_stats_data[i,2]=round(sd(data[,i], na.rm = TRUE),3)
  series_stats_data[i,3]=round(skewness(data[,i], na.rm =TRUE),3)
  series_stats_data[i,4]=round(kurtosis(data[,i], na.rm = TRUE),3)
  series_stats_data[i,5]=length(na.omit(data[,i]))
  }

#Mostrar a tabela com as estatísticas descritivas
print(series_stats_data)

# Gráficos exportados via PDF em função do tamanho
    # Definir diretório para gráficos exportados em PDF em função do tamanho
pdf("/Volumes/GoogleDrive/My Drive/MPE_dissertação/modelo/modelo_R/exports_modelo_R/seriestemp.pdf", paper = "USr", width = 10, height = 10)

#Gráfico das séries temporais - formato 2 linhas x 2 colunas
par(mfrow=c(2,2))
plot(data$Precos_Imoveis, xlab="período", ylab="Variação %a.m", type="l", main= "Preços Imóveis no RS - var %", cex=0.5)
plot(data$IPCA_POA, xlab="período", ylab="Variação %a.m", type="l", main="Inflação na RMPA - var %", cex=0.5)
plot(data$CUB_RS, xlab="período", ylab="Variação %a.m.", type="l", main="Custo da Construção no RS - var %", cex=0.5)
plot(data$Rental_Yield, xlab="período", ylab="Variação %a.m.", type="l", main="Rentabilidade do aluguel - Var %", cex=0.5)
plot(data$Tx_financ, xlab="período", ylab="Variação %a.m.", type="l", main="Taxas Financ. Imobiliário - Var %", cex=0.5)
plot(data$IBCR_RS, xlab="período", ylab="variação %a.m.", type="l", main="Nível de Atividade RS - Var %", cex=0.5)
plot(data$Tx_Desocup, xlab="período", ylab="Variação %a.m.", type="l", main="Taxa de desocupação - Var %", cex=0.5)
plot(data$Expect_Inflação, xlab="período", ylab="Variação %a.m.", type="l", main="Expectativa de Inflação - Var %", cex=0.5)

# Fechar exportação em PDF
dev.off()

## *****MODELO VAR***** ##

#PROCESSO DE ESTIMAÇÃO 

## 1. Visualizar os dados e identificar observarções fora do padrão (outliers, sazonalidade, tendência)

# Gráfico das séries temporais - formato 2 linhas x 2 colunas
par(mfrow=c(2,2))
plot(data$Precos_Imoveis, xlab="período", ylab="Variação %a.m", type="l", main= "Preços Imóveis no RS - var %", cex=0.5)
plot(data$IPCA_POA, xlab="período", ylab="Variação %a.m", type="l", main="Inflação na RMPA - var %", cex=0.5)
plot(data$CUB_RS, xlab="período", ylab="Variação %a.m.", type="l", main="Custo da Construção no RS - var %", cex=0.5)
plot(data$Rental_Yield, xlab="período", ylab="Variação %a.m.", type="l", main="Rentabilidade do aluguel - Var %", cex=0.5)
plot(data$Tx_financ, xlab="período", ylab="Variação %a.m.", type="l", main="Taxas Financ. Imobiliário - Var %", cex=0.5)
plot(data$IBCR_RS, xlab="período", ylab="variação %a.m.", type="l", main="Nível de Atividade RS - Var %", cex=0.5)
plot(data$Tx_Desocup, xlab="período", ylab="Variação %a.m.", type="l", main="Taxa de desocupação - Var %", cex=0.5)
plot(data$Expect_Inflação, xlab="período", ylab="Variação %a.m.", type="l", main="Expectativa de Inflação - Var %", cex=0.5)

##Estatística descritiva das séries
summary(data)

statNames = c("Média", "Desvio Padrão", "Assimetria", "Curtose", "Tamanho")
variavNames=c("Preço Imóveis", "IPCA POA", "CUB RS", "Rental Yield", "Tx. Financ.", "IBCR RS",
              "Tx. Desocup.", "Expect. Inflação")
series_stats_data=matrix(data=NA, nrow = length(data_names), ncol = length(statNames))
colnames(series_stats_data)=statNames
rownames(series_stats_data)=variavNames

for (i in 1:nrow(series_stats_data)) {
  series_stats_data[i,1]=round(mean(data[,i], na.rm=TRUE),3) 
  series_stats_data[i,2]=round(sd(data[,i], na.rm = TRUE),3)
  series_stats_data[i,3]=round(skewness(data[,i], na.rm =TRUE),3)
  series_stats_data[i,4]=round(kurtosis(data[,i], na.rm = TRUE),3)
  series_stats_data[i,5]=length(na.omit(data[,i]))
}

#Mostrar a tabela com as estatísticas descritivas
print(series_stats_data)

#Diagrama de caixa (Boxplot)
par(mfrow=c(1,1))
boxplot(data, cex=3)

# Função de autocorrelação e autocorrelação parcial das séries temporais
  # Gráficos de ACF e PACF - formato 1 linha x 2 colunas
par(mfrow=c(1,2))

acf(Precos_imoveis, lag.max = 60, type=c("correlation", "covariance", "partial"))
pacf(Precos_imoveis, lag.max = 60)

acf(IPCA_POA, lag.max=60, type=c("correlation", "covariance", "partial"))
pacf(IPCA_POA, lag.max = 60)

acf(CUB_RS, lag.max=60, type=c("correlation", "covariance", "partial"))
pacf(CUB_RS, lag.max = 60)

acf(RentalYield, lag.max=60, type=c("correlation", "covariance", "partial"))
pacf(RentalYield, lag.max = 60)

acf(Tx_financ, lag.max=60, type=c("correlation", "covariance", "partial"))
pacf(Tx_financ, lag.max = 60)

acf(IBCR_RS, lag.max=60, type=c("correlation", "covariance", "partial"))
pacf(IBCR_RS, lag.max = 60)

acf(TX_desocup, lag.max=60, type=c("correlation", "covariance", "partial"))
pacf(TX_desocup, lag.max = 60)

acf(Expect_Inflação, lag.max=60, type=c("correlation", "covariance", "partial"))
pacf(Expect_Inflação, lag.max = 60)

# A análise da ACF aponta para sazonalidade com decaimento lento nas séries do IBCR e Tx de desocupação.

#---------------

## 2. Se necessário, transformar os dados para estabilizar a variância (logaritmo ou retirar sazonalidade, por exemplo).

#Busca extensiva do melhor ajuste das séries via função auto.arima. Analisar a ordem de integração (d, D) 
#para cada série e, caso necessário, diferenciar. 

library(forecast)

ARIMA_PrImov<-auto.arima(Precos_imoveis, max.p=11, max.P=3, max.q=11, max.Q=3, max.d=1, max.D=1,
                         stepwise=FALSE, max.order=30, approximation=FALSE, xreg=NULL,
                         test="adf", ic="bic")
summary(ARIMA_PrImov)

ARIMA_IPCA_POA<-auto.arima(IPCA_POA, max.p=11, max.P=3, max.q=11, max.Q=3, max.d=1, max.D=1,
                           stepwise=FALSE, max.order=30, approximation=FALSE, xreg=NULL,
                           test="adf", ic="bic")
summary(ARIMA_IPCA_POA)

ARIMA_CUB_RS<-auto.arima(CUB_RS, max.p=11, max.P=3, max.q=11, max.Q=3, max.d=1, max.D=1,
                         stepwise=FALSE, max.order=30, approximation=FALSE, xreg=NULL,
                         test="adf", ic="bic")
summary(ARIMA_CUB_RS)

ARIMA_RentalYield<-auto.arima(RentalYield, max.p=11, max.P=3, max.q=11, max.Q=3, max.d=1, max.D=1,
                         stepwise=FALSE, max.order=30, approximation=FALSE, xreg=NULL,
                         test="adf", ic="bic")
summary(ARIMA_RentalYield)

ARIMA_Tx_financ<-auto.arima(Tx_financ, max.p=11, max.P=3, max.q=11, max.Q=3, max.d=1, max.D=1,
                              stepwise=FALSE, max.order=30, approximation=FALSE, xreg=NULL,
                              test="adf", ic="bic")
summary(ARIMA_Tx_financ)

ARIMA_IBCR_RS<-auto.arima(IBCR_RS, max.p=11, max.P=3, max.q=11, max.Q=3, max.d=1, max.D=1,
                            stepwise=FALSE, max.order=30, approximation=FALSE, xreg=NULL,
                            test="adf", ic="bic")
summary(ARIMA_IBCR_RS)

ARIMA_Tx_Desocup<-auto.arima(TX_desocup, max.p=11, max.P=3, max.q=11, max.Q=3, max.d=1, max.D=1,
                             stepwise=FALSE, max.order=17, approximation=FALSE, xreg=NULL,
                             test="adf", ic="bic")
summary(ARIMA_Tx_Desocup)

ARIMA_Expect_Inflação<-auto.arima(Expect_Inflação, max.p=11, max.P=3, max.q=11, max.Q=3, max.d=1, max.D=1,
                             stepwise=FALSE, max.order=30, approximation=FALSE, xreg=NULL,
                             test="adf", ic="bic")
summary(ARIMA_Expect_Inflação)

#Série da Rentabilidade do Alguel (RentYield) apresenta melhor ajuste na forma d=1 (0,1,0).
#Assim, tiramos a primeira diferença da série.

# Primeira diferença da série Rental Yield
RentYield.1diff<-diff(RentalYield)
plot(RentYield.1diff, xlab="período", ylab="Variação %a.m.", type="l", main="Rentabilidade do aluguel - Var %", cex=0.5)

#Novo auto.arima para a série RentalYield diferenciada.
ARIMA_RY1.diff<-auto.arima(RentYield.1diff, max.p=11, max.P=3, max.q=11, max.Q=3, max.d=1, max.D=1,
                           stepwise=FALSE, max.order=30, approximation=FALSE, xreg=NULL,
                           test="adf", ic="bic")
summary(ARIMA_RY1.diff)
#Série retornou um ruído branco (0,0,0), de forma que a diferenciação contribuiu para um melhor ajuste. 
#Trabalharemos com a série diferenciada.

#---------------

## 3. Avaliar a função de correlação cruzada para confirmar a possibilidade de modelagem multivariada

# Defasagens máximas
defasagens=12

# Definir diretório para gráficos exportados em PDF em função do tamanho
pdf("/Volumes/GoogleDrive/My Drive/MPE_dissertação/modelo/modelo_R/exports_modelo_R/graficosCCF.pdf", paper = "USr", width = 10, height = 10)

# Matriz de gráficos (4x4) - (kxk ficou muito grande para exportar)
par(mfrow=c(4,4))
# Adicionar uma "matriz" de gráficos a CCF de cada série contra ela mesma e as demais
for (i in 1:ncol(data)) {
  for (j in 1:ncol(data)) {
    ccf(drop(data[,i]), drop(data[,j]), lag.max=defasagens, 
        main="", ylab="FCC", xlab="Defasagens") 
    title(paste0(colnames(data)[i], "-", colnames(data)[j], adj=0.4, line=1))
  }
}

# Fechar exportação em PDF
dev.off()

#---------------

## 4. Testar se os dados são estacionários ou cointegrados.
#- Caso não tenha raiz unitária (sejam estacionários), estimar um VAR com as séries em nível
#- Caso tenha raiz unitária, mas sem cointegração, é preciso difrenciar os dados até se tornarem estacionários
#         e estimar um VAR com as séries diferenciadas.
# Caso tenha raiz unitária, mas com cointegração, devemos estimar o VEC com as séries em nível.


## 4.1 Teste de estacionariedade das séries (ADF).
## Pacote urca

#Interpretação: (hipótese nula (H0) é deque há raíz unitária),
#Para rejeitar a nula, o valor do teste (value of test statistic = t value para z.lag.1) 
#deve ser menor do que tauN para, pelo menos, 5%. 
#Se for maior, não se rejeita a nula e deve-se diferenciar até que a série não apresente 
#evidências de não estacionariedade (até o valor do teste ficar menor do que os valores críticos).

# Iinicialmente o teste ADF séra feito com todas as séries em nível (inclusive a do RentalYield).

urcaprimov=ur.df(data$Precos_Imoveis, type=c("drift"), lags=12, selectlags="BIC")
summary(urcaprimov)
urcaIPCA=ur.df(data$IPCA_POA, type=c("drift"), lags=12, selectlags="BIC")
summary(urcaIPCA)
urcaCUBRS=ur.df(data$CUB_RS, type=c("drift"), lags=12, selectlags="BIC")
summary(urcaCUBRS)
#série não diferenciada do RY. Testar com constante e tendência.
urcaRentYield=ur.df(data$Rental_Yield, type=c("trend"), lags=12, selectlags="BIC")
summary(urcaRentYield)
urcaTxFinanc=ur.df(data$Tx_financ, type=c("none"), lags=12, selectlags="BIC")
summary(urcaTxFinanc)
urcaIBCR_RS=ur.df(data$IBCR_RS, type=c("none"), lags=12, selectlags="BIC")
summary(urcaIBCR_RS)
urcaTx_Desocup=ur.df(data$Tx_Desocup, type=c("drift"), lags=12, selectlags="BIC")
summary(urcaTx_Desocup)
urcaExp_Infl=ur.df(data$Expect_Inflação, type=c("none"), lags=12, selectlags="BIC")
summary(urcaExp_Infl)

## TAXA DE DESOCUPAÇÃO 
# Não rejeita H0 (valor do teste= -1.1695 é maior do que os 
#valores críticos de tau2 em qualquer nível de significância).
urcaTx_Desocup@teststat
urcaTx_Desocup@cval

#Primeira diferença Sazonal da série da Tx de Desocupação (pois os lags sazonais estão
#com decaimento lento)
Tx_Desocu.1SazDiff<-diff(data$Tx_Desocup,12)
plot(Tx_Desocu.1SazDiff)
#Teste do ajuste da série diferenciada via função auto.arima.
ARIMA_Tx_Desocup.1SazDiff<-auto.arima(Tx_Desocu.1SazDiff, max.p=11, max.P=3, max.q=11, max.Q=3, max.d=1, max.D=1,
                             stepwise=FALSE, max.order=17, approximation=FALSE, xreg=NULL,
                             test="adf", ic="bic")
summary(ARIMA_Tx_Desocup.1SazDiff)
#Função auto.arima aponta que a ordem de integração da série está bem ajustada (1,0,0).
#Teste ADF para confirmar se a primeira diferença sazonal foi suficiente para estacionarizar a série. 
urcaTx_Desocup.1SazDiff=ur.df(na.omit(Tx_Desocu.1SazDiff), type=c("none"), lags=12, selectlags="BIC")
summary(urcaTx_Desocup.1SazDiff)
#Série com 1 diferença sazonal rejeita H0 a qualquer nível de significância. 

## RENTAY YIELD 
# Não passa com nível de significância de 5%, reforçando a necessidade de trabalharmos com a série diferenciada 
#(pois, além de melhor ajustada com a primeira diferença na busca do auto.arima, a série em nível não rejeita H0 a 5%).
urcaRentYield@teststat
urcaRentYield@cval

# Teste ADF da série em sua primeira diferença
urcaTx_RentYield.1diff=ur.df(na.omit(RentYield.1diff), type=c("none"), lags=12, selectlags = "BIC")
summary(urcaTx_RentYield.1diff)
#Teste com a série diferenciada rejeita H0 em qualquer nível de significância. 

# NÍVEL DE ATIVIDADE (IBCR_RS)
# Embora rejeite H0 no teste ADF com a série em nível, ou seja, não haja evidências suficientes de que seja
# não estacionária, apresenta decaimento lento nos lags sazonais. 
# Diferença sazonal da série IBCR_RS
IBCR_RS.1SazDiff<-diff(data$IBCR_RS, 12)
#Teste ADF da série diferenciada sazonalmente.
urcaIBCR.1SazDiff=ur.df(na.omit(IBCR_RS.1SazDiff), type=c("none"), lags=12, selectlags = "BIC")
summary(urcaIBCR.1SazDiff)
#Auto.Arima da série IBCR_RS diferenciada sazonalmente
ARIMA_IBCR_RS.1SazDiff<-auto.arima(IBCR_RS.1SazDiff, max.p=11, max.P=3, max.q=11, max.Q=3, max.d=1, max.D=1,
                          stepwise=FALSE, max.order=30, approximation=FALSE, xreg=NULL,
                          test="adf", ic="bic")
summary(ARIMA_IBCR_RS.1SazDiff)
#A função auto.arima da série diferenciada sazonalmente retorna um ruído branco (0,0,0) contra
#um AR 10 da série em nível (10,0,0). Além disso, a série diferenciada sazonalmente rejeita H0 no teste
#ADF com um valor de teste mais significativo do que a série em nível. Assim, trabalharemos com a série 
#em sua primeira diferença sazonal.

#---------------

## 5. Definir a ordem "p" para os dados em análise por meio de critérios de informação (escolher modelo com menor AIC, por exemplo)

# Função VARselect do pacote vars, que conta com as seguintes opções:
# - y: dados do modelo
# - lag.max: quantidade máxima de defasagens avaliadas (para cada defasagem um modelo VAR será estimado)
# - type: quais parâmetros determinísticos se quer incluir no modelo. Podem ser:
#     - "const" para uma constante nas equações
#     - "trend" para uma tendência nas equações
#     - "both" para constante e tendência
#     - "none" nenhum dos dois (apenas parâmetros das defasagens)
# - season: dummies sazonais caso os dados apresentem sazonalidade
# - exogen: variáveis exógenas do modelo (dummies, por exemplo)

# Determinação das variáveis exógenas e endógenas
endogen=data.frame(Precos_imoveis, CUB_RS)
exogen=data.frame(RentYield.1diff, Tx_Desocu.1SazDiff, Expect_Inflação, IPCA_POA, Tx_financ, IBCR_RS.1SazDiff)

#Remoção das observações incompletas (NAs) que surgiram com as diferenciações das séries anteriores.
exogen_omit<-na.omit(exogen)

#Seleção do número de linhas compatível com o database resultante das variáveis exógenas
#[linhainicial:linhafinal, coluna..]
endogen_omit<-endogen[13:119,]

#Seleção da ordem de defasagem do modelo
varorder=vars::VARselect(y=endogen_omit, lag.max=10, type="const", season=NULL, exogen=exogen_omit)
print(varorder)

# O modelo selecionado pelo critério AIC é o de ordem 1 VAR(1)

#---------------

## 6. Estimar o modelo escohido no passo anterior

# Usamos a função VAR do pacote vars, que tem as seguintes opções (além das já apresentadas no VARselect):
# - p: número de defasagens do modelo
modeloVAR=vars::VAR(y=endogen_omit, p=3, type="const", exogen = exogen_omit)
summary(modeloVAR)

#---------------

## 7. Verificar significância estatística do modelo estimado e, caso seja necessário, eliminar parâmetros não significantes.
# Realização do teste de causalidade de Granger para confirmar ou não a relação entre as variáveis e justificar a inclusão delas no VAR(p).

# Função causality do pacote vars para executar os testes. As opções são as seguintes:
#-x: modelo VAR estimado
#-causa: a variável de "causa"
#- vcov: permite especificar manualmente a matriz de covariância
vars::causality(modeloVAR, cause="Precos_Imoveis")$Granger
vars::causality(modeloVAR, cause="CUB_RS")$Granger

#Teste de causalidade de Granger com p-valor de 0.02615. Assim, rejeitamos a hipótese nula H0 de que 
#o CUB-RS não Granger-causa o preço dos imóveis (o teste aponta que o CUB_RS Granger-causa o preços dos imóveis).
#A relação inversa não se confirma, ou seja, aceitamos H0 de que o preço dos imóveis não Granger-causa 
#o CUB-RS.

## Algumas das variáveis exógenas aparentam não apresentar significância estatística no modelo.
## Restrição do modelo de forma que apenas variáveis que são estatísticamente significantes ao nível de 10%
#thresh: critical values of t for two tailed tests:
#thresh p/10%: 1.645, thresh p/5%: 1.960

#Função restrict do pacote Vars
VAR.restricted=vars::restrict(modeloVAR, method = "ser", thresh = 1.645)
summary(VAR.restricted)

#R2 ajustado melhorou sensivelmente para ambas as equações 
#(tanto do Pr. Imóveis quanto do CUB RS, mas especialmente para o CUB RS)

#---------------

#ANÁLISE DOS RESÍDUOS E HIPÓTESES DO MODELO

#8. EXAMINAR SO OS RESÍDUOS SE COMPORTAM COMO UM RUÍDO BRANCO E CONDIÇÕES DE ESTACIONARIEDADE DO MODELO.
#Caso contrário, voltar ao passo 3 ou 4.

## 8.1 -- Verificando a estabilidade do modelo -- 
# Verifiação dos autovalores (módulos devem ser menores do que a unidade).
roots(VAR.restricted)
#Todas as raízes são menores do que a unidade, indicando que o modelo é estável.

## 8.2 -- Análise dos resíduos --

## Gráfico para avaliação do ajuste da primeira equação do modelo
pdf("/Volumes/GoogleDrive/My Drive/MPE_dissertação/modelo/modelo_R/exports_modelo_R/residuals_primov.pdf", paper = "USr", width = 10, height = 10)
plot(VAR.restricted, names="Precos_Imoveis", lag.acf=60, lag.pacf=60)
dev.off( )

# Gráfico para avaliação do ajuste da primeira equação do modelo
pdf("/Volumes/GoogleDrive/My Drive/MPE_dissertação/modelo/modelo_R/exports_modelo_R/residuals_CUB_RS.pdf", paper = "USr", width = 10, height = 10)
plot(VAR.restricted, names="CUB_RS", lag.acf=60, lag.pacf=60)
dev.off( )

#Os gráficos de FAC e FACP  não indicam resíduos com autocorrelação (a partir da não significância estatística
#das defasagens).

## -Teste de Portmanteau para testar correlação serial -
#Interpretação (Kotzé): p-valor maior do que 5% indica que há ausência de correlação serial.
portmant_test<-serial.test(VAR.restricted, lags.pt = 12, type="PT.asymptotic")
portmant_test
#Hipótese nula do teste é de que as funções de autocorrelação de todas as séries 
#não têm elementos significantes para as defasagens selecionadas. 
#Para 12 defasagens (usual nos materiais), o P-valor retornou >10%, sugerindo que não há correlação serial dos resíduos.
#Aumentando para 60 defasagens, P-valor retorna 1, reforçando o resultado anterior e aceitando a nula de que não há
#autocorrelação nos resíduos.

## - Teste de heterocedasticidade nos resíduos ARCH Lagrange-Multiplier test -
#Interpretação (Kotzé): Novamente, p-valor maior do que 5% indica que há ausência de heterocedasticidade.
arch_lagrange_test<-arch.test(VAR.restricted, lags.multi = 12, multivariate.only=TRUE)
arch_lagrange_test
#P-valor retornou 0.19, indicando ausência de heterocedasticidade.

## -- Teste CUSUM para quebra estrutural nos resíduos -- 
cusum_test<-stability(VAR.restricted, type="OLS-CUSUM")
plot(cusum_test)
#Não há evidências de quebra nos intervalos de confiança

#---------------

# 9. UMA VEZ QUE OS RUÍDOS SÀO BEM COMPORTADOS E O MODELO É ESTÁVEL

## 9.1 -- Resposta ao Impulso -- 

# Função irf do pacot vars para obter as funções de resposta ao impulso do modelo. As opções são:
  #- x: modelo VAR que será analisado;
  #- impulse: a variável que se quer impulsionar (deve ser o mesmo da saída do modelo);
  #- response: varáveis que queremos obter a resposta. Caso se queira uma variável específica, 
#deve-se defini-la. Se for deixado NULL, será calculada a resposta em todas as demais variáveis do modelo.
#- n.ahead: passos à frente que se quer visualizar;
#- outras: ver help(irf)

#Resposta do Preço dos Imóveis a um choque no CUB
irf_primov<-irf(VAR.restricted, impulse = "CUB_RS", response = "Precos_Imoveis", 
           n.ahead = 40,  boot = TRUE)
plot(irf_primov)

#Resposta do CUB a um choque no Preço dos Imóveis
irf_cub=vars::irf(VAR.restricted, impulse="Precos_Imoveis", response="CUB_RS", 
                n.ahead=40, boot=TRUE)
plot(irf_cub)

