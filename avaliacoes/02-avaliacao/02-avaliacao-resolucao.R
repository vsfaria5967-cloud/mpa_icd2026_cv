# Arquivo: 02-avaliacao-resolucao.R
# Autor(a): Vinicius Silva Faria
# Data: 11/06/2026
# Objetivo: Resolução da Avaliação 2 - Introdução à Ciência de Dados


# Configurações globais  ----------------------------------------

# define opções globais para exibição de números
options(digits = 5, scipen = 999)

# Carrega os pacotes necessários
library(tidyverse) # dplyr, purrr::pmap_dbl() etc.
library(EnvStats)  # distribuição triangular: rtri()
library(tidyquant) # tq_get()


# Resolução da Questão 1 ----------------------------------------

# fixa a semente para reprodutibilidade
set.seed(2026)

# número de cenários simulados
n_sim <- 20000

# duração do projeto, em anos (conhecida)
duracao <- 5

# (a.1) Simula os parâmetros incertos de cada cenário, em R$ mil.
cenarios <- tibble(
  cenario        = seq_len(n_sim),
  investimento   = rtri(n_sim, min = 850, max = 1200, mode = 1000),
  receita_anual  = rtri(n_sim, min = 230, max = 350, mode = 290),
  valor_residual = rtri(n_sim, min = 100, max = 200, mode = 150),
  taxa_desconto  = rtri(n_sim, min = 0.11, max = 0.15, mode = 0.13)
)

# (a.2) Função que calcula o VPL de UM cenário.
# Traduz a fórmula do enunciado
calcular_vpl <- function(investimento, receita_anual,
                         valor_residual, taxa_desconto,
                         duracao = 5) {
  # cria o vetor 1, 2, ..., duracao
  anos <- seq_len(duracao)
  
  # valor presente das receitas (constantes) ao longo dos anos
  vp_receitas <- sum(receita_anual / (1 + taxa_desconto)^anos)
  
  # valor presente do valor residual, recebido ao final
  vp_residual <- valor_residual / (1 + taxa_desconto)^duracao
  
  # VPL do cenário (combine os três componentes acima)
  vp_receitas + vp_residual - investimento
}

# (a.3) Aplique a função a todos os cenários com
# a função apropriada do pacote purrr e armazene em 'vpl'
simulacoes <- cenarios |> 
  mutate(
    vpl= pmap_dbl(
     list(
        investimento   = investimento,
        receita_anual  = receita_anual,
        valor_residual = valor_residual,
        taxa_desconto  = taxa_desconto
      ),
      # nome da função a aplicar
      .f = calcular_vpl
    )
  )

# vetor de VPLs simulados (use-o nos itens b, c e d)
vpl_sim <- simulacoes$vpl


# (b) Probabilidade de o projeto destruir valor: P(VPL < 0).
prob_vpl_neg <- mean(vpl_sim < 0)

# exibe a probabilidade em percentual
prob_vpl_neg * 100

# Interpretação (item b): Dados os parâmetros adotados, constata-se que existe
# uma probabilidade de 23,21% de o projeto oferecer um VPL negativo.


# (c) VPL determinístico (use as MODAS de cada parâmetro)
# e comparação.
vpl_det <- calcular_vpl(
  investimento   = 1000,
  receita_anual  = 290,
  valor_residual = 150,
  taxa_desconto  = 0.13
)

# média dos VPLs simulados (compare com o VPL determinístico)
vpl_medio <- mean(vpl_sim)

# desvio-padrão dos VPLs simulados
vpl_desvio <- sd(vpl_sim)

# exibe os valores para comparação
vpl_det
vpl_medio
vpl_desvio

# Interpretação (item c): o VPL determinístico não coincide com o VPL simulado
# devido ao fato de o VPL simulado considerar uma variabilidade nos valores dos
# parâmetros adotados, na tentativa de simular cenários que podem ocorrer,
# enquanto que o VPL determinístico considera apenas um valor único para cada um
# dos parâmetros.


# (d) Histograma da distribuição simulada
hist(vpl_sim,
     breaks = 50,
     col = "lightblue",
     main = "Distribuição simulada do VPL",
     xlab = "VPL (R$ mil")

# linha vertical em VPL = 0
abline(v = 0, col = "red", lwd = 2, lty = 2)

# linha vertical no VPL médio simulado
abline(v = mean(vpl_sim), col = "blue", lwd = 2)

# legenda das duas linhas
legend("topright",
       legend = c("VPL = 0", "VPL médio simulado"),
     col = c("red", "blue"),
     lwd = 2, lty = c(2,1), bty = "n")


# Resolução da Questão 2 ----------------------------------------

# (a) Importe os preços (ajustados) e calcule os
# retornos log diários.
precos_vale3 <- "VALE3.SA" |> 
  tq_get(get = "stock.prices",
         from = "2024-01-01",
         to = "2026-06-08") |> 
  select(date, adjusted)

retornos_vale3 <- precos_vale3 |> 
  # fórmula do retorno log diário (use o preço ajustado)
  mutate(ret = log(adjusted / lag(adjusted))) |> 
  drop_na()

# extrai a coluna ret como um vetor
ret_vale3 <- retornos_vale3 |> pull(ret)

# (b) Parâmetros do problema (veja o enciado).
valor_carteira <- 25000
p <- 0.01

# (c) VaR histórico: quantil da cauda esquerda

# ordena os retornos do pior para o melhor
ret_ordenado <- sort(ret_vale3)

# posição do quantil de p
k <- ceiling(length(ret_ordenado) * p)

# retorno no ponto de corte
retorno_var <- ret_ordenado[k]

# converte o retorno em perda positiva (%)
var_percentual <- -retorno_var * 100

# VaR em reais
var_monetario <- -retorno_var * valor_carteira

# exibe os valores do VaR
var_percentual
var_monetario

# (d) Expected Shortfall

# média dos retornos nas posições 1 até k
retorno_medio_cauda <- mean(ret_ordenado[1:k])

# ES em %, como perda positiva
es_percentual <- -retorno_medio_cauda * 100

# ES em reais
es_monetario <- -retorno_medio_cauda * valor_carteira

# exibe os valores do ES
es_percentual
es_monetario

# (e) Interpretação: o VaR histórico encontrado, considerando o horizonte de um
# dia, foi de aproximadamente 3,85%, o que representa R$ 962,80. Esse é o limiar
# de perda diária que poderá ser ultrapassado em aproximadamente 1% dos dias.
# O Expected Shotfall histórico para o horizonte de um dia foi de
# aproximadamente 4,74% o que representa R$ 1.185,12. Essa é a média das perdas
# na cauda além do VaR.


# Resolução da Questão 3 ----------------------------------------

# (a) Importe os preços (ajustados), organize em formato largo
# e calcule os retornos SIMPLES diários de cada ação.
serie_precos <- c("ITUB4.SA", "VALE3.SA", "WEGE3.SA") |> 
  tq_get(get = "stock.prices",
         from = "2024-01-01",
         to = "2026-06-08") |> 
  select(symbol, date, adjusted) |> 
  pivot_wider(names_from = symbol,
              values_from = adjusted) |> 
  rename(dia = date,
         itub4 = ITUB4.SA,
         vale3 = VALE3.SA,
         wege3 = WEGE3.SA)

# retorno simples = preço / preço anterior - 1
retornos <- serie_precos |> 
  mutate(
    # exemplo: retorno simples do ITUB4
    ret_itub4 = itub4 / dplyr::lag(itub4) - 1,
    # aplique a mesma fórmula
    ret_vale3 = vale3 / dplyr::lag(vale3) - 1,
    ret_wege3 = wege3 / dplyr::lag(wege3) - 1
  ) |> 
  # remove dados faltantes
  drop_na()

# (b) Calcule o retorno diário da carteira.

# pesos da carteira, na ordem: itub4, vale3, wege3
pesos <- c(0.40, 0.35, 0.25)

retornos <- retornos |> 
  # combine os três retornos com os pesos
  mutate(
    ret_carteira = pesos[1] * ret_itub4 +
           pesos[2] * ret_vale3 +
           pesos[3] * ret_wege3
         )

# (c)-(d) Parâmetros e medidas de risco (VaR e ES)

# parâmetros do problema
valor_carteira <- 100000
p <- 0.01

# ordena os retornos do pior para o melhor
ret_ordenado <- sort(retornos$ret_carteira)

# posição do quantil de p
k <- ceiling(length(ret_ordenado) * p)

# VaR em %, como perda positiva
var_percentual <- -ret_ordenado[k] * 100

# VaR em reais
var_monetario <- -ret_ordenado[k] * valor_carteira

# ES em %, como perda positiva
es_percentual <- -mean(ret_ordenado[1:k]) * 100

# ES em reais
es_monetario <- -mean(ret_ordenado[1:k] * valor_carteira)

# exibe os valores do VaR e do ES
var_percentual
var_monetario
es_percentual
es_monetario

# (e) Interpretação:o VaR histórico encontrado, considerando o horizonte de um
# dia, foi de aproximadamente 2,48 % ou R$ 2.482,90. Esse é o limiar de perda
# diária que poderá ser ultrapassado em aproximadamente 1% dos dias.
# Expected Shortfall histórico para o horizonte de um dia foi de aproximadamente
# 3,16% ou R$ 3.157,10. Essa é a média das perdas mais severas.
# O Expected Shortfall tende a ser maior em magnitude porque ele resume perdas
# ainda mais severas do que o ponto de corte usado no VaR.

