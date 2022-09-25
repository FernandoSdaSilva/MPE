##PACOTES
install.packages(c("tidyquant","tidyverse","forecast", "magrittr",
"MTS","vars","urca","rprojroot", "tbl2xts", "Quandl", "BETS"))

# Pacotes para coletar dados
suppressMessages(require(tidyquant))
suppressMessages(require(Quandl))
suppressMessages(require(BETS))
suppressMessages(require(quantmod))

# Pacotes para manusear os dados
suppressMessages(require(tidyverse))
suppressMessages(require(tbl2xts))
suppressMessages(require(readxl))

# Pacote para facilitar o uso de arquivos do projeto
suppressMessages(require(rprojroot))

# Pacote para estimar o modelo var
suppressMessages(require(vars))
suppressMessages(require(urca))
suppressMessages(require(MTS))
suppressMessages(require(forecast))

# Pacote para gerar os gráficos
suppressMessages(require(dygraphs))

# Pacote para gerar tabelas dos modelos estimados
install.packages("texreg")
require(texreg)

#PACOTES EXTRA
install.packages("seasonalview")
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
series_stats_data=matrix(data=NA, nrow = length(data_names), ncol = length(statNames))
colnames(series_stats_data)=statNames

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

# PACOTES
install.packages("fUnitRoots")
require(fUnitRoots)

install.packages("urca")
require(urca)

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

# Estatística descritiva das séries

summary(data)

statNames = c("Média", "Desvio Padrão", "Assimetria", "Curtose", "Tamanho")
series_stats_data=matrix(data=NA, nrow = length(data_names), ncol = length(statNames))
colnames(series_stats_data)=statNames

for (i in 1:nrow(series_stats_data)) {
  series_stats_data[i,1]=round(mean(data[,i], na.rm=TRUE),3) 
  series_stats_data[i,2]=round(sd(data[,i], na.rm = TRUE),3)
  series_stats_data[i,3]=round(skewness(data[,i], na.rm =TRUE),3)
  series_stats_data[i,4]=round(kurtosis(data[,i], na.rm = TRUE),3)
  series_stats_data[i,5]=length(na.omit(data[,i]))
}

par(mfrow=c(1,1))
boxplot(data, cex=3)

# Função de autocorrelação e autocorrelação parcial das séries temporais

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

# A análise da ACF aponta para sazonalidade nas séries do IBCR e Tx de desocupação.

## 2. Se necessário, transformar os dados para estabilizar a variância (logaritmo ou retirar sazonalidade, por exemplo)
# Retirar sazonalidade?

#---------------

## 3. Avaliar a função de correlação cruzada para confirmar a possibilidade de modelagem multivariada

# Defasagens máximas
defasagens=10

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

## 4. Testar se os dados são estacionários. Caso tenha raiz unitária, é preciso difrenciar os dados até se tornarem estacionários

# Função adfTest do pacote fUnitRoots para testar se há raíz unitária nas séries temporais consideradas.
   # Possibilidades do teste:
      # nc: regression w/ no intercept (constant) nor time trend (random walk)
      # c: regression w/ an intercept (constant) but no time trend (randm walk w/ drift)
      # ct: regression w/ an intercept (constant) and a time trend (random walk w/ constant and trend)
# Definimos que no máximo três defasagens da série devem ser utilizadas como variáveis explicativas da regressão do teste.

#Pacote fUnitRoots
      #obs: [,X] representa a coluna da planilha "data"sendo considerada no teste.

unitRootprimov=fUnitRoots::adfTest(data[,1], lags=3, type=c("ct"))
unitRootIPCA=fUnitRoots::adfTest(data[,2], lags=3, type=c("ct"))
unitRootCUBRS=fUnitRoots::adfTest(data[,3], lags=3, type=c("ct"))
unitRootRentYield=fUnitRoots::adfTest(data[,4], lags=3, type=c("ct"))
unitRootTxFinanc=fUnitRoots::adfTest(data[,5], lags=3, type=c("ct"))
unitRootIBCR_RS=fUnitRoots::adfTest(data[,6], lags=3, type=c("ct"))
unitRootTx_Desocup=fUnitRoots::adfTest(data[,7], lags=3, type=c("ct"))
unitRootExp_Infl=fUnitRoots::adfTest(data[,8], lags=3, type=c("ct"))

# Tabelas com os resultados do teste
adf_PrImov=c(unitRootprimov@test$statistic, unitRootprimov@test$p.value)
adf_IPCA=c(unitRootIPCA@test$statistic, unitRootprimov@test$p.value)
adf_CUBRS=c(unitRootCUBRS@test$statistic, unitRootprimov@test$p.value)
adf_RentYield=c(unitRootRentYield@test$statistic, unitRootprimov@test$p.value)
adf_TxFinanc=c(unitRootTxFinanc@test$statistic, unitRootprimov@test$p.value)
adf_IBCR_RS=c(unitRootIBCR_RS@test$statistic, unitRootprimov@test$p.value)
adf_Tx_Desocup=c(unitRootTx_Desocup@test$statistic, unitRootprimov@test$p.value)
adf_Exp_Infl=c(unitRootExp_Infl@test$statistic, unitRootprimov@test$p.value)
resultADF=cbind(adf_PrImov, adf_IPCA, adf_CUBRS, adf_RentYield, adf_TxFinanc, adf_IBCR_RS, adf_Tx_Desocup, adf_Exp_Infl)
colnames(resultADF)=c("PrImov", "IPCA_POA", "CUB_RS", "RentalYield", "Tx_Financ", "IBCR_RS", "Tx_Desocup", "Expect_Inflação")
rownames(resultADF)=c("Estatística do Teste ADF", "p-valor")
print(resultADF)

# Por este teste, rejeita-se a hipótese nula de raiz unitária em todas as séries. 
# Assim, não há necessidade de diferenciar as séries para incluí-las no modelo.

#Pacote urca

urcaprimov=ur.df(data$Precos_Imoveis, type=c("drift"), lags=10, selectlags="AIC")
summary(urcaprimov)
urcaIPCA=ur.df(data$IPCA_POA, type=c("drift"), lags=10, selectlags="AIC")
summary(urcaIPCA)
urcaCUBRS=ur.df(data$CUB_RS, type=c("drift"), lags=10, selectlags="AIC")
summary(urcaCUBRS)
urcaRentYield=ur.df(data$Rental_Yield, type=c("drift"), lags=10, selectlags="AIC")
summary(urcaRentYield)
urcaTxFinanc=ur.df(data$Tx_financ, type=c("drift"), lags=10, selectlags="AIC")
summary(urcaTxFinanc)
urcaIBCR_RS=ur.df(data$IBCR_RS, type=c("drift"), lags=10, selectlags="AIC")
summary(urcaIBCR_RS)
urcaTx_Desocup=ur.df(data$Tx_Desocup, type=c("drift"), lags=10, selectlags="AIC")
summary(urcaTx_Desocup)
urcaExp_Infl=ur.df(data$Expect_Inflação, type=c("drift"), lags=10, selectlags="AIC")
summary(urcaExp_Infl)

# Revisar RentalYield, Expectativa da Inflação e Taxa de desocupação c/ Fernando.

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
endogen=data.frame(data$Precos_Imoveis, data$CUB_RS, data$IBCR_RS)
exogen=data.frame(data$Rental_Yield, data$Tx_Desocup, data$Expect_Inflação, data$IPCA_POA, data$Tx_financ)

varorder=vars::VARselect(y=endogen, lag.max=10, type="const", season=NULL, exogen=exogen)
print(varorder)

# O modelo selecionado pelo critério AIC é o de ordem 10 VAR(10)

## 6. Estimar o modelo escohido no passo anterior

# Usamos a função VAR do pacote vars, que tem as seguintes opções (além das já apresentadas no VARselect):
# - p: número de defasagens do modelo
modeloVAR=vars::VAR(y=endogen, p=10, type="const", exogen = exogen)
summary(modeloVAR)

## 7. Verificar significância estatística do modelo estimado e, caso seja necessário, eliminar os parâmetros não significantes.

