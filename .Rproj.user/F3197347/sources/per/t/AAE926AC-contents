################################################################################
#                                                                              #
#     APLICAÇÃO DO MÉTODO AHP-GAUSSIANO NA ESCOLHA DE AÇÕES LISTADAS NA B3     #
#                                                                              #
################################################################################

# TCC - MBA USP-Esalq
# Turma: DSA 212
# Título: Aplicação do método Analytic Hierarchy Process Gaussiano na elaboração
#         de carteira acionária do mercado financeiro
# Nome: Clayton Del Tedesco Júnior
# Data inicial: 22/01/2023
# Data final: 26/01/2023

##################################################################################
#                  INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS              #
##################################################################################
#Pacotes utilizados
pacotes <- c("tidyverse", "readr", "writexl", "clusterSim",
             "PortfolioAnalytics", "PerformanceAnalytics", "quantmod")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

transformar_primeira_letra_cabecalhos_maiuscula <- function(df) {
  nomes_colunas <- colnames(df)
  nomes_colunas_transformados <- paste0(toupper(substr(nomes_colunas, 1, 1)), substr(nomes_colunas, 2, nchar(nomes_colunas)))
  colnames(df) <- nomes_colunas_transformados
  return(df)
}

# Contador utilizado para montar o arquivo com a carteira de ações
n_shares = 1

# Cria um dataframe vazio para alocar as melhores ações
bestShares <- data.frame()
rankingShares <- data.frame()

# Carrega uma primiera vez fora do loop para obter o cabeçlho do data frame a ser formado
sector <- read.csv("D:/Documents/Pós-USP Esalq/TCC/TCC 2/Base de Dados/Nova pasta/Setor 34.csv",
                   header = TRUE,
                   sep = ";",
                   dec = ",")

# Atribui o cabeçalho ao dataframe das melhores ações
bestShares <- sector[0, ]
bestShares <- bestShares %>% mutate(Ranking=NA)
rankingShares <- sector[0, ]
rankingShares <- rankingShares %>% mutate(Ranking=NA)

bestShares <- transformar_primeira_letra_cabecalhos_maiuscula(bestShares)

# Variáveis monotônicas de custo que serão utilizadas os seus inversos:
# "Cotação", "P.L", "PSR", "P.EBIT", "Dív.Brut..Patrim."
cols_to_inverse = c('Cotação', 'P.L', 'Psr', 'P.Cap.Giro', 'P.Ativo', 'P.EBIT', 'Dív.Brut..Patrim.')


# Foram considerados os 43 subsetores pertencentes à B3 (mesmo que vazios)
for (i in 1:43){

  # Caminho para abertura dos arquivos contendo as ações de cada setor
  PATH <- sprintf("D:/Documents/Pós-USP Esalq/TCC/TCC 2/Base de Dados/Nova pasta/Setor %s.csv", i)
  
  # Carrega a tabela contando as ações
  sector <- read.csv(PATH,
                     header = TRUE,
                     sep = ";",
                     dec = ",")

  emptySectorCheck = is.na(sector$papel)
  if (nrow(sector) == 0){
    #print(i)
    remove(sector)
    remove(sector_df)
    next
  } else {
    if (emptySectorCheck[1] == TRUE) {
      #print(i)
      remove(sector)
      remove(sector_df)
      next
    }
  }
        
  
  # Mantem o dataframe original criando um novo dataframe para cálculos
  sector_df <- sector
  
  
  # Aplicar a função para transformar a primeira letra em maiúscula
  sector_df <- transformar_primeira_letra_cabecalhos_maiuscula(sector_df)
#--------------Adequa variáveis que as vezes são 'characters' em 'doubles'-------------#
  for(typeVerify in cols_to_inverse){
    if(typeof(sector_df[,typeVerify]) == 'character'){
      sector_df[, typeVerify] <- gsub("\\.", "", sector_df[, typeVerify])
      sector_df[, typeVerify] <- gsub(",", ".", sector_df[, typeVerify])
      sector_df[, typeVerify] <- as.numeric(sector_df[, typeVerify])
    }
  }
  sector_df$Liq.2meses <- gsub("\\.", "", sector_df$Liq.2meses)
  sector_df$Liq.2meses <- gsub(",", ".", sector_df$Liq.2meses)
  sector_df$Liq.2meses <- as.numeric(sector_df$Liq.2meses)
  
  sector_df$Patrim..Líq <- gsub("\\.", "", sector_df$Patrim..Líq)
  sector_df$Patrim..Líq <- gsub(",", ".", sector_df$Patrim..Líq)
  sector_df$Patrim..Líq <- as.numeric(sector_df$Patrim..Líq)
  
  sector_df$EV.EBIT <- gsub("\\.", "", sector_df$EV.EBIT)
  sector_df$EV.EBIT <- gsub(",", ".", sector_df$EV.EBIT)
  sector_df$EV.EBIT <- as.numeric(sector_df$EV.EBIT)
  
  sector_df$P.VP <- gsub("\\.", "", sector_df$P.VP)
  sector_df$P.VP <- gsub(",", ".", sector_df$P.VP)
  sector_df$P.VP <- as.numeric(sector_df$P.VP)
  
  sector_df$P.Ativ.Circ.Liq <- gsub("\\.", "", sector_df$P.Ativ.Circ.Liq)
  sector_df$P.Ativ.Circ.Liq <- gsub(",", ".", sector_df$P.Ativ.Circ.Liq)
  sector_df$P.Ativ.Circ.Liq <- as.numeric(sector_df$P.Ativ.Circ.Liq)
#--------------------------------------------------------------------------------------#

  # Transforma variáveis monotônicas de custo em seu inverso:
  sector_df[cols_to_inverse] <- sector_df[cols_to_inverse]^(-1)

  # Trata divisões por zeros que resultam em "inf"
  sector_df <- mutate_all(sector_df, function(x) ifelse(is.infinite(x), 0, x))

    
#-------------------Transforma variáveis de porcentagem para decimal-------------------#
  sector_df$Div.Yield <- parse_number(sector_df$Div.Yield)/10000
  sector_df$Mrg.Ebit <- parse_number(sector_df$Mrg.Ebit)/10000
  sector_df$Mrg..Líq. <- parse_number(sector_df$Mrg..Líq.)/10000
  sector_df$Roic <- parse_number(sector_df$Roic)/10000
  sector_df$Roe <- parse_number(sector_df$Roe)/10000
  sector_df$Cresc..Rec.5a <- parse_number(sector_df$Cresc..Rec.5a)/10000
#--------------------------------------------------------------------------------------#
  summary(sector_df)
  # Normaliza as variáveis pelo método de Min-Max Normalization
  sector_df[,2:21] <- data.Normalization(sector_df[,2:21], type = "n4")

  
  # Trata NaN
  sector_df <- mutate_all(sector_df, function(x) ifelse(is.na(x), 0, x))

  # Cálculo do vetor de médias das variáveis
  means <- colMeans(sector_df[sapply(sector_df, is.numeric)])
    
  # Cálculo do vetor de desvios padrões das variáveis
  sds <- apply(sector_df[,2:21], 2, sd)
    
  # Cálculo do Fator Gaussiano
  gaussianFactor = sds/means
summary(gaussianFactor)
  
  # Trata NaN
  gaussianFactor[is.na(gaussianFactor)] <- 0
  
  # Normaliza o fator gaussiano
  gaussianFactor_norm <- data.Normalization(gaussianFactor, type = "n10")
    
  # Aplicação do fator gaussiano na base de dados
  sector_df[,2:21] <- sector_df[,2:21] * gaussianFactor_norm
    
  # Cria e calcula coluna de Ranking, realizando a soma das variáveis de cada observação
  sector_df$Ranking = rowSums(sector_df[, 2:21])
    
  # Ordena o dataframe em ordem decrescente pelo valor da coluna Ranking
  sector_df <- sector_df[order(-sector_df$Ranking), ]
  topShare <- sector_df$Papel[1]
    
    
  # Salva a melhor ação do setor no dataframe bestShares e rankingShares
  bestShares[n_shares, ] <- filter(sector, papel == topShare)
  rankingShares[n_shares, ] <- filter(sector_df, Papel == topShare)
  bestShares[n_shares, 22] <- rankingShares[n_shares, 22]
  n_shares = n_shares + 1
  remove(gaussianFactor)
  remove(gaussianFactor_norm)
}

rankingShares <- rankingShares[order(-rankingShares$Ranking), ]
bestShares$Ranking <- as.numeric(bestShares$Ranking)
bestShares <- bestShares[order(-bestShares$Ranking), ]

# Cria o arquivo com o dataframe das melhores ações
write_xlsx(bestShares, "bestShares.xlsx", format_headers = TRUE)

# Cria o arquivo com somente com as siglas e cotações das ações
write_xlsx(bestShares[c(1,2)], "Wallet.xlsx", format_headers = TRUE)

# Cria o arquivo co as ações ordenadas da melhor para a pior segundo o método
write_xlsx(rankingShares, "rankingShares.xlsx", format_headers = TRUE)
