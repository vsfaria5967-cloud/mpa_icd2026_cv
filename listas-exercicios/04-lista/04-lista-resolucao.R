# Arquivo: 04-lista-resolucao.R
# Autor(a): Vinícius Silva Faria
# Data: 14/04/2026
# Objetivo: Resolução da lista de exercícios 4

# Configuracoes globais  ------------------------------------

# define opções globais para exibição de números
options(digits = 5, scipen = 999)

# carrega os pacotes necessários
library(here)      # para usar caminhos relativos
library(tidyverse) # inclui readr, dplyr, tidyr, ggplot2 etc.


# Exercício 1 ------------------------------------------------

# Função que calcula o montante (capital + juros) de uma aplicação com juros
# compostos e capitalização mensal
calcular_montante_mensal <- function(capital, taxa_anual, meses) {
  montante_final <- capital * (1 + taxa_anual / 12)^meses
  return(montante_final)
}

# Teste da função com: capital = 5000, taxa_anual = 0.10, meses = 36
calcular_montante_mensal(capital = 5000, taxa_anual = 0.10, meses = 36)


# Exercício 2 ------------------------------------------------

# Função que recebe um retorno (em decimal) e retorna uma classificação textual
avaliar_investimento <- function(retorno) {
  if (retorno > 0.15) {
    return("Excelente")
  } else if (retorno > 0.05) {
    return("Bom")
  } else if (retorno > 0) {
    return("Fraco")
  } else if (retorno <= 0) {
    return("Negativo")
  }
}

# Teste da função
avaliar_investimento(0.20)
avaliar_investimento(0.08)
avaliar_investimento(0.02)
avaliar_investimento(-0.05)


# Exercício 3 ------------------------------------------------

# Função que recebe uma tibble com as colunas ativo, preco_compra, preco_atual e quantidade e retorna a mesma tibble acrescida das seguintes colunas:
# retorno: o retorno percentual de cada ativo, calculado como (preço_atual / preço_compra-1) * 100
# valor_investido: preco_compra * quantidade
# valor_atual: preco_atual * quantidade
# resultado: valor_atual - valor_investido
# situacao: "Ganho" se o resultado for positivo, "Perda" caso contrário
analisar_carteira <- function(dados) {
    dados |> 
      mutate(
        retorno         = (preco_atual / preco_compra -1) * 100,
        valor_investido = preco_compra * quantidade,
        valor_atual     = preco_atual * quantidade,
        resultado       = valor_atual - valor_investido,
        situacao        = ifelse(resultado > 0, "Ganho", "Perda")
      )
  }

# Teste da função
carteira <- tibble(
  ativo        = c("PETR4", "VALE3", "ITUB4", "WEGE3"),
  preco_compra = c(28.50, 68.20, 32.00, 45.00),
  preco_atual  = c(31.00, 65.40, 33.60, 48.50),
  quantidade   = c(200, 100, 300, 150)
)

analisar_carteira(carteira)


# Exercício 4 ------------------------------------------------

# map_dbl() para calcular o valor futuro de R$ 10.000 investidos por 20 anos para as seguintes taxas anuais:
# taxas_anuais <- c(0.04, 0.06, 0.08, 0.10, 0.12, 0.14, 0.16)
calcular_valor_futuro <- function(valor_presente, taxa, periodos) {
  valor_futuro <- valor_presente * (1 + taxa)^periodos
  return(valor_futuro)
}

taxas_anuais <- c(0.04, 0.06, 0.08, 0.10, 0.12, 0.14, 0.16)

# Armazenando os resultados em um vetor chamado vf_20_anos
vf_20_anos <- map_dbl(
  taxas_anuais,
  \(taxa) calcular_valor_futuro(10000, taxa, 20)
)

# tibble chamada comparacao_cenarios contendo as seguintes colunas:
# taxa: as taxas anuais utilizadas
# taxa_percentual: a taxa em porcentagem
# valor_futuro: o valor futuro calculado
# ganho_liquido: o valor futuro menos o valor investido (10.000)
comparacao_cenarios <- tibble(
  taxa            = taxas_anuais,
  taxa_percentual = taxas_anuais * 100,
  valor_futuro    = vf_20_anos,
  ganho_liquido   = valor_futuro - 10000
)

# Visualização da tibble
comparacao_cenarios


# Exercício 5 ------------------------------------------------

# Calcula o VPL de um projeto de investimento
# Argumentos:
#   investimento   - valor do investimento inicial
#   fluxos         - vetor de fluxos de caixa futuros
#   taxa           - taxa de desconto por período
#   valor_residual - valor residual ao final (padrão = 0)
# Retorna:
#   valor numérico correspondente ao VPL
calcular_vpl <- function(investimento, fluxos, taxa, valor_residual = 0) {
  n <- length(fluxos)     # número de períodos
  t <- seq_along(fluxos)  # sequência 1, 2, ..., n
  
  vpl <- -investimento +
    sum(fluxos / (1 + taxa)^t) +
    valor_residual / (1 + taxa)^n
  
  return(vpl)
}

# investimento inicial: R$ 300.000
# fluxos de caixa anuais: R$ 80.000, R$ 95.000, R$ 110.000 e R$ 100.000
# valor residual: R$ 30.000
taxas_desconto <- c(0.08, 0.10, 0.12, 0.14, 0.16, 0.18)

vpl <- map_dbl(
  taxas_desconto,
  \(taxa) calcular_vpl(
    investimento = 300000,
    fluxos = c(80000, 95000, 110000, 100000),
    taxa,
    valor_residual = 30000
  )
)

# Organizando os resultados em uma tibble chamada analise_projeto com as
# colunas:
# taxa_pct: a taxa em porcentagem
# vpl: o VPL calculado
# decisao: "Viável" se o VPL for positivo, "Inviável" caso contrário
analise_projeto <- tibble(
  taxa_pct = taxas_desconto * 100,
  vpl      = vpl,
  decisao  = ifelse(vpl > 0, "Viável", "Inviável")
)

# Visualização da tibble
analise_projeto


# Exercício 6 ------------------------------------------------
# (resolver em arquivo .qmd separado)


# Exercício 7 (Desafio) --------------------------------------

# Construção de uma simulação de Monte Carlo simplificada:
# investimento inicial: R$ 200.000
# taxa de desconto: 10% ao ano
# valor residual: R$ 20.000
# o projeto gera fluxos de caixa por 3 anos
# os fluxos de caixa não são conhecidos com certeza: estimamos que cada fluxo
# anual tem média de R$ 80.000 e desvio-padrão de R$ 15.000

# Função que calcula o VPL
calcular_vpl <- function(investimento, fluxos, taxa, valor_residual = 0) {
  n <- length(fluxos)
  t <- seq_along(fluxos)
  
  vpl <- -investimento +
    sum(fluxos / (1 + taxa)^t) +
    valor_residual / (1 + taxa)^n
  
  return(vpl)
}

# Define semente para reprodutibilidade
set.seed(123)

# Uso do map_dbl para simulação de 1.000 cenários
vpl_sim <- map_dbl(1:1000, \(i) {
  fluxos <- rnorm(3, mean = 80000, sd = 15000)
  calcular_vpl(200000, fluxos, 0.10, 20000)
})

# VPL médio dos 1.000 cenários
vpl_medio <- mean(vpl_sim)
vpl_medio

# Probabilidade de VPL positivo
probabilidade_vpl_positivo <- mean(vpl_sim > 0)
probabilidade_vpl_positivo

# Percentil 5% e percentil 95%
percentil_5 <- quantile(vpl_sim, 0.05)
percentil_95 <- quantile(vpl_sim, 0.95)
percentil_5
percentil_95
