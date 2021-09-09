# Código feito por Augusto D'Arruda - RA 157704

# Versão do R: 4.0.2 (Taking Off Again)
# Versão RStudio: 1.4.1103 (Wax Begonia)


##----------------------------------------------------------------
##                  SOBRE A IMPORTAÇÃO DOS DADOS                 -
##----------------------------------------------------------------

# Há dois jeitos de importar os dados.

# O primeiro é usando o pacote BatchGetSymbols
# O segundo é utilizando o arquivo .rds anexado
# Nesse script, está sendo utilizado o pacote BatchGetSymbols

##-----------------------
##  Pacotes necessários  
##-----------------------
# Certifique-se que todos pacotes estão instalados usando install.library()

library(BatchGetSymbols)
library(tidyverse)
library(magrittr)
library(rugarch)
library(cowplot)
library(forecast)
library(xts)

# OPÇÃO 1 DE BAIXAR OS DADOS
##--------------------------------------------
##  Baixando dados do pacote BatchGetSymbols  
##--------------------------------------------

ticket <- '^GSPC' # ticket S&P 500
series_name <- 'S&P 500' 

primeira_data <- '1990-01-01'
ultima_data <- '2021-05-18'

# Lendo dados
sp500 <- BatchGetSymbols(tickers = ticket, 
                         first.date = primeira_data, 
                         last.date = ultima_data)

# Modificando dados para o log retorno
df_prices <- sp500$df.tickers %>%
  select(ref.date, ticker, price.adjusted) %>%
  mutate(log_ret = log(price.adjusted/dplyr::lag(price.adjusted) ),
         series_name = series_name) %>%
  na.omit()

# save data into file
#rds_out <- 'sp500.rds'
#write_rds(df_prices, rds_out)


# OPÇÃO 2 DE BAIXAR OS DADOS
##------------------
##  Lendo os Dados  
##------------------

# Descomentar para rodar

# df_prices <- readRDS("sp500.rds")


##---------------------
##  INÍCIO DA ANÀLISE  
##---------------------

# Número de pontos
n_max <- 10

# Primeiro Plot (Figura 1 A)
p1 <- ggplot(df_prices, aes(x = ref.date, y = price.adjusted)) + 
  geom_line() + 
  labs(title = paste0('Preço da ', series_name),
       subtitle = paste0("Dados de ", primeira_data, "até " , ultima_data),
       x = '',
       y = 'Índice de Preço Ajustado',
       caption = 'Dados do Yahoo Finance') + 
  theme_bw(base_family = "TT Times New Roman")

p1

# agrupando por ticker e calculando os pontos maximos
largest_tab <- df_prices %>%
  group_by(ticker) %>%
  top_n(abs(log_ret), n = n_max)

# Segundo Plot (Figura 1 B)
p2 <- ggplot(df_prices, 
             aes(x = ref.date, y = log_ret)) + 
  geom_line() + 
  labs(title = paste0('Retornos em Log diários do ', series_name),
       subtitle = paste0('Pontos vermelhos representam os ', n_max, 
                         ' valores absolutos de variação na amostra'),
       x = '',
       y = 'Log do Retorno',
       caption = 'Dados do Yahoo Finance') + 
  theme_bw(base_family = "TT Times New Roman") + 
  geom_point(data = largest_tab, aes(x = ref.date, y = log_ret), 
             size = 2, color = 'red'  ) +
  scale_y_continuous(labels = scales::percent) + 
  labs(size = 'Variação de Preço Absoluto') + 
  scale_color_brewer(palette = 'BrBG')
p2

# Combinando os dois gráficos

p <- plot_grid(p1, p2, nrow = 2, labels = 'AUTO')
p

# Autocorrelograma

# build autocorrelagram
acf <- ggAcf(x = df_prices$log_ret, lag.max = 10) +
  labs(title = paste0('Autocorrelograma para o retorno em Log do ', series_name)) +
  theme_bw(base_family = "TT Times New Roman")
acf


# Construindo o modelo
ar_p <- 1 # AR(1)
ma_q <- 1 # MA (1)
arch_r <- 1 # GARCH(r,s)
garch_s <- 1 # GARCH(r,s)
modelo_garch <- c("sGARCH") # Standard GARCH
distr_est <- 'snorm' # Distribuição Normal


# opcoes do modelo
opcoes_modelo <- ugarchspec(variance.model = list(model = modelo_garch,
                                              garchOrder = c(arch_r, 
                                                             garch_s)),
                        mean.model = list(armaOrder = c(ar_p,
                                                        ma_q)), 
                        distribution.model = distr_est)
  
fit_armagarch <- ugarchfit(spec = opcoes_modelo, data = df_prices$log_ret)



# Modelo ARMA-GARCH                      
fit_armagarch

# Log Retorno
logret <- df_prices$log_ret

# Gráfico Comparação
qplot(y = logret , x = 1:7904 , geom = 'point') + 
  geom_point(colour = 'lightgrey' , size = 2) + 
  geom_line(aes(y = fit_armagarch@fit$sigma*(-1.644854) , x = 1:7904) , colour = 'red') +
  geom_hline(yintercept = sd(logret)*qnorm(0.05) , 
             colour = 'darkgreen' , 
             size = 0.8, 
             linetype = "twodash") + 
  theme_light() + 
  labs(x = '' , y = 'Retorno Diário' , 
       title = 'Comparação do Valor em Risco de Jan-1990 até Mai-2021')



# Procedimento Gráfico Ultimos 500 pontos
model.roll = ugarchroll(spec = opcoes_modelo , data = logret , n.start = 7404 , refit.every = 50 ,
                        refit.window = 'moving')



VaR95_td = mean(logret) + model.roll@forecast$density[,'Sigma']*qdist(distribution='std', shape=3.7545967917, p=0.05)

qplot(y = VaR95_td , x = 1:500 , geom = 'line') +
  geom_point(aes(x = 1:500 , y = logret[7405:7904] , 
                 color = as.factor(logret[7405:7904] < VaR95_td)) , 
             size = 1.3) + 
  scale_color_manual(values = c('gray' , 'red')) + 
  labs(y = 'Retornos Diários' , 
       x = '',
       title = "Log dos retornos diários dos últimos 500 pontos") + 
  theme_light() + 
  theme(legend.position = 'none')

