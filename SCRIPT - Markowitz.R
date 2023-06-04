library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(tidyverse)

# Define os ativos que irão ser coletados

tickers <- c("BRBI11.SA", "OFSA3.SA", "ATOM3.SA", "SIMH3.SA",
             "ETER3.SA", "NTCO3.SA", "KEPL3.SA", "VAMO3.SA", "TFCO4.SA")

tickers <- c("PETR4.SA", "ITUB4.SA", "ABEV3.SA", "JBSS3.SA")

# Define a data de início da coleta

start <- as.Date("2022-12-29")

# Realiza a coleta dos preços diários

prices <- getSymbols(tickers,
                     auto.assign = TRUE,
                     warnings = FALSE,
                     from = start,
                     src = "yahoo") %>%
  map(~Cl(get(.))) %>%
  reduce(merge) %>%
  `colnames<-`(tickers)

# Transfroma os preços diários em mensais

prices_monthly <- to.monthly(prices,
                             indexAt = "lastof",
                             OHLC = FALSE)

# Calcula os retornos mensais

asset_returns <- Return.calculate(prices_monthly,
                                  method = "log") %>%
  na.omit()



# Define os nomes dos ativos na especificação

portfolio_spec <- portfolio.spec(assets = tickers)

# Considera que a soma dos pesos será igual a 1

portfolio_spec <- add.constraint(portfolio = portfolio_spec,
                                 type = "full_investment")

# Não permite vendas a descoberto

portfolio_spec <- add.constraint(portfolio = portfolio_spec,
                                 type = "long_only")

# Adiciona os objetivos - Retorno esperado através da média amostral

portfolio_spec <- add.objective(portfolio = portfolio_spec,
                                type = "return",
                                name = "mean")

# Adiciona os objetivos - Risco esperado através do desvio padrão

portfolio_spec <- add.objective(portfolio = portfolio_spec,
                                type = "risk",
                                name = "StdDev")



# Calcula a otimização do portfólio

opt <- optimize.portfolio(asset_returns,
                          portfolio = portfolio_spec,
                          optimize_method = "random",
                          trace = TRUE)

# Plota o gráfico de Risco x Retorno

chart.RiskReward(opt,
                 risk.col = "StdDev",
                 return.col = "mean",
                 chart.assets = TRUE,
                 main = "Risco x Retorno - Combinações dos ativos selecionados")
