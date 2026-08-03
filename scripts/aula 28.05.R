# Exercicio 1

# Carrega os pacotes necessários
library(tidyverse)
library(tidyquant)

# Importando os preços diários das ações PETR4.SA a partir de 2024-01-01
# tq_get(): baixa preços históricos do Yahoo Finance
serie_precos_petr4 <- tq_get("PETR4.SA", from = "2024-01-01")

# Calculando os retornos logarítmicos diários da ação
# log(close / lag(close)): retorno log-diário ln(P_t/P_{t-1})
serie_precos_petr4 <- serie_precos_petr4 |> mutate(ret = log(close / lag(close))) |>
  drop_na()

# Parâmetros
valor_carteira <- 10000
p <- 0.01
retornos <- serie_precos_petr4$ret

# Estimação do VaR
# Ordena os retornos crescentemente: 
# do pior retorno para o melhor retorno
ret_petr_ord <- sort(retornos)

# Encontra o índice associado ao quantil empírico de nível p 
# ceiling() seleciona uma observação dentro da cauda esquerda da amostra
op <- ceiling(length(ret_petr_ord) * p)

# Retorno selecionado: quantil empírico inferior dos retornos
retorno_var_hs <- ret_petr_ord[op]

# VaR histórico monetário: converte retorno adverso em perda positiva
var_petr_hs <- -retorno_var_hs * valor_carteira

# VaR histórico percentual
var_percentual_petr_hs <- -retorno_var_hs * 100

# ES: média dos retornos na cauda esquerda, expressa como perda positiva
es_percentual <- -mean(ret_petr_ord[1:op]) * 100

# ES: monetário
es_monetario <- es_percentual / 100 * valor_carteira

# RESULTADOS

# VaR histórico diário percentual
var_percentual_petr_hs

# VaR histórico diário monetário
var_petr_hs

# Expected Shortfall histórico diário percentual
es_percentual

# Expected Shortfall histórico diário monetário
es_monetario

# Interpretação: Existe 1% de probabilidade de a perda diária da carteira ultrapassar
# 5,58%, o que equivale a uma perda de R$ 558,21.
# Nos dias em que ocorrer essa ultrapassagem, a perda média esperada é de aproximadamente
# 6,64%, o que equivale a uma perda de R$ 664,37