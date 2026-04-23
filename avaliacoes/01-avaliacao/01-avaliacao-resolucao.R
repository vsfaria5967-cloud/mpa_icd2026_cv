# Arquivo: 01-avaliacao-resolucao.R
# Autor(a): Vinícius Silva Faria
# Data: 16/04/2026
# Objetivo: 
# Resolução da Avaliação 1 - Introdução à Ciência de Dados


# Configurações globais  ----------------------------------------

# define opções globais para exibição de números
options(digits = 5, scipen = 999)

# carrega os pacotes necessários
library(tidyverse)
library(here)

# Resolução da Questão 1


# 1.a) --------------------------------------------------------
agencias_csv <- here("data/raw/agencias.csv")
agencias <- read.csv(agencias_csv)
glimpse(agencias)

credito_csv <- here("data/raw/credito_trimestral.csv")
credito <- read.csv(credito_csv)
glimpse(credito)

inadimplencia_csv <- here("data/raw/inadimplencia.csv")
inadimplencia <- read.csv(inadimplencia_csv)
glimpse(inadimplencia)


# 1.b) ---------------------------------------------------------
credito_long <- credito |> 
  pivot_longer(
    cols = c("T1", "T2", "T3", "T4"),
    names_to = "trimestre",
    values_to = "volume_credito"
  )

credito_long


# 1.c) ---------------------------------------------------------
dados_completos <- credito_long |>
  left_join(agencias, by = "codigo_agencia") |>
  left_join(inadimplencia, by = c("codigo_agencia", "trimestre"))

dados_completos


# 1.d) ---------------------------------------------------------
dados_analise <- dados_completos |> 
  mutate(
    credito_por_cooperado = volume_credito * 1000 / num_cooperados,
    risco = case_when(
      taxa_inadimplencia < 3.0 ~ "Baixo",
      taxa_inadimplencia >= 3.0 & taxa_inadimplencia < 4.5 ~"Moderado",
      taxa_inadimplencia >= 4.5 ~ "Alto"
    )
  )

dados_analise


# 1.e) ---------------------------------------------------------
dados_analise_por_cidade <- dados_analise |> 
  group_by(cidade) |> 
  summarise(volume_total = sum(volume_credito),
            media_inadimplencia = mean(taxa_inadimplencia)) |> 
  arrange(desc(volume_total))

dados_analise_por_cidade


# Resolução da Questão 2


# 2.a) ---------------------------------------------------------
calcular_prestacao <- function(valor, taxa_anual, num_meses) {
  prestacao <- valor * ((taxa_anual / 12) * (1 + (taxa_anual / 12))^num_meses) / (((1 + (taxa_anual / 12))^num_meses)-1)
  return(prestacao)
}

calcular_prestacao(120000, 0.12, 60)


# 2.b) ---------------------------------------------------------
taxas_anuais <- c(0.08, 0.10, 0.12, 0.14, 0.16)

cenarios_taxas_de_juros <- map_dbl(
  taxas_anuais,
  \(taxa_anual) calcular_prestacao(120000, taxa_anual, 60)
)

resultado <- tibble(
  taxa_anual       = taxas_anuais,
  prestacao_mensal = cenarios_taxas_de_juros
)

resultado

# 2.c) ---------------------------------------------------------
resultado <- resultado |> 
  mutate(
    custo_total = prestacao_mensal * 60,
    juros_totais = custo_total - 120000,
    acessibilidade = case_when(
      prestacao_mensal < 2600 ~ "Acessível",
      prestacao_mensal >= 2600 & prestacao_mensal< 2800 ~"Moderado",
      prestacao_mensal > 2800 ~"Elevado"
    )
  )

resultado
