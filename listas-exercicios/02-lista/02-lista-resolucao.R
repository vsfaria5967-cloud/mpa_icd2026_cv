# ============================================================
# Disciplina: Introdução à Ciência de Dados
# ============================================================
# Arquivo: 02-lista-resolucao.R
# Autor(a): Vinícius Silva Faria
# Data: 19/03/2026
# Objetivo: Resolução da lista de exercícios 2

# Configuracoes globais  ------------------------------------

# define opções globais para exibição de números
options(digits = 5, scipen = 999)

# carrega os pacotes necessários
library(here)      # para usar caminho relativo
library(tidyverse) # meta-pacote que inclui readr, dplyr..
library(gapminder) # contém os dados gapminder

# carrega os dados do pacote gapminder
data(gapminder)
dplyr::glimpse(gapminder) #"dplyr::" usado apenas para me lembrar do pacote ao qual a função pertence

## Exercício 1
caminho_csv <- here("data/raw/productionlog_sample.csv")
dados_destilarias <- read.csv(caminho_csv)
glimpse(dados_destilarias)

## Exercício 2

dados_expectativa <- gapminder |>
  select(country, year, lifeExp)


## Exercício 3
dados_sem_pop_e_sem_gdpPercap <- gapminder |> 
  select(-pop, -gdpPercap)
dados_sem_pop_e_sem_gdpPercap

## Exercício 4

variaveis_com_c <- gapminder |>
  select(starts_with("c"))

## Exercício 5
variaveis_country_ate_pop <- gapminder |> 
  select(country:pop)

## Exercício 6
variaveis_contendo_ou_terminando_com_p <- gapminder |> 
  select(contains("p") | ends_with("p"))

## Exercício 7
paises_america_2007 <- gapminder |> 
  filter(continent == "Americas" & year == 2007)

## Exercício 8
dados_brazil <- gapminder |> 
  filter(country == "Brazil")

## Exercício 9
asia_maior_que_50_milhoes_em_2007 <- gapminder |> 
  filter(continent == "Asia" & pop > 50000000 & year == 2007)

## Exercício 10
exp_vida_acima_de_75_anos_com_gdpPercap_abaixo_de_10000_dolares_em_2007 <-
  gapminder |> 
  filter(lifeExp > 75) |> 
  filter(gdpPercap < 10000) |> 
  filter(year == 2007)

## Exercício 11
pop_em_milhoes <- gapminder |>
  mutate(pop_em_milhoes = pop / 1000000)

## Exercício 12
PIB_total <- gapminder |> 
  mutate(PIB_total = gdpPercap * pop)

## Exercício 13
economia_grande <- gapminder |> 
  mutate(economia_grande = ifelse(pop > 50000000, "Sim", "Não"))

## Exercício 14
classificacao_lifeExp <- gapminder |> 
  mutate(classificacao_lifeExp = ifelse(lifeExp < 60, "Baixa",
                                        ifelse(lifeExp < 75, "Média", "Alta")
                                        )
         )

## Exercício 15
expectativa_por_continente <- gapminder |> 
  group_by(continent) |> 
  summarise(expectativa_media = mean(lifeExp))

## Exercício 16
pop_por_continente_2007 <- gapminder |> 
  filter(year == 2007) |> 
  group_by(continent) |> 
  summarise(pop_por_continente_2007 = sum(pop))

## Exercício 17
desempenho_filiais <- gapminder |> 
  filter(year == 2007) |> 
  group_by(continent) |> 
  summarize(
    qtd_filiais = n(),
    pib_medio = mean(gdpPercap),
    melhor_filial = max(gdpPercap)
  )

## Exercício 18
evolucao_lifeExp_americas <- gapminder |> 
  filter(continent == "Americas") |> 
  group_by(year) |> 
  summarise(expectativa_media_de_vida = mean(lifeExp))

## Exercício 19
paises_ordenados <- gapminder |> 
  filter(year == 2007) |> 
  arrange(desc(lifeExp))

## Exercício 20
menor_gdpPercap_2007 <- gapminder |> 
  filter(year == 2007) |> 
  slice_min(gdpPercap, n = 5)

## Exercício 21
paises_america_2007_por_pop <- gapminder |> 
  filter(year == 2007, continent == "Americas") |> 
  arrange(desc(pop))

## Exercício 22
ranking_lifeExp_por_continente <- gapminder |> 
  filter(year == 2007) |> 
  group_by(continent) |> 
  summarise(expectativa_media_de_vida = mean(lifeExp)) |>
  arrange(desc(expectativa_media_de_vida))
