# Configuracoes globais do documento ----------------------------

# evita notacao cientifica e deixa os resultados mais legiveis
options(digits = 5, scipen = 999)

# carrega os pacotes usados ao longo da lista
library(tidyverse)


# Exercício 1 -------------------------------------------------------------

# Fixa o ponto inicial do gerador pseudoaleatório
set.seed(20260507)

# vetores com retornos e probabilidades
retornos_possiveis <- c(0.06, 0.02, -0.01, -0.04)
probabilidades <- c(0.15, 0.45, 0.25, 0.15)

# Número de simulações
n_simulacoes <- 10000

# Amostra
retornos <- sample(
  retornos_possiveis,
  size = n_simulacoes,
  replace = TRUE,
  prob = probabilidades
)

valor_esperado <- mean(retornos)
valor_esperado
variancia <- var(retornos)
variancia
desvio_padrao <- sd(retornos)
desvio_padrao
probabilidade_retorno_negativo_mensal <- mean(retornos < 0)
probabilidade_retorno_negativo_mensal

# Exercício 2 -------------------------------------------------------------

# Fixa o ponto inicial do gerador pseudoaleatório
set.seed(20260508)

# Lucros possíveis, probabilidades e número de operações individuais
lucros_possiveis <- c(900, 150, -3500)
probabilidades_lucro <- c(0.88, 0.08, 0.04)
n_operacoes_individuais <- 20000

# Simulação das operações individuais
lucros_operacao <- sample(
  lucros_possiveis,
  size = n_operacoes_individuais,
  replace = TRUE,
  prob = probabilidades_lucro
)

# Valor esperado do lucro por operação
valor_esperado_lucro_operacao <- mean(lucros_operacao)
valor_esperado_lucro_operacao

# Desvio-padrão do lucro por operação
desvio_lucro_operacao <- sd(lucros_operacao)
desvio_lucro_operacao

# Probabilidade de prejuízo por operação
probabilidade_prejuizo_operacao <- mean(lucros_operacao < 0)
probabilidade_prejuizo_operacao

# Número de carteira e número de operações por carteiras
n_carteiras <- 5000
n_operacoes_por_carteira <- 80

# cria uma expressao mais explícita para simular uma carteira
# dentro de cada repeticao, primeiro geramos os lucros das operacoes
# e depois somamos esses valores

# gera a distribuicao simulada do lucro total de 5.000 carteiras 
# independentes
# replicate() repete a mesma expressao muitas vezes
# neste item, cada repeticao gera uma amostra pseudoaleatoria de 
# 80 lucros por operacao e, em seguida, soma esses lucros
lucros_carteira <- replicate(
  n_carteiras,
  {
    lucros_carteira <- sample(
      lucros_possiveis,
      size = n_operacoes_por_carteira,
      replace = TRUE,
      prob = probabilidades_lucro
    )
    
    sum(lucros_carteira)
  }
)

# Valor esperado do lucro total por carteira
valor_esperado_lucro_carteira <- mean(lucros_carteira)
valor_esperado_lucro_carteira

# Parametrização proporcional dos lucros individuais para comparação
# com a carteira
lucro_medio_operacao_escalado <-
  n_operacoes_por_carteira * mean(lucros_operacao)
lucro_medio_operacao_escalado

# Diferença entre lucros da carteira e individuais na mesma proporção
diferenca_lucros_esperados <-
  valor_esperado_lucro_carteira - lucro_medio_operacao_escalado
diferenca_lucros_esperados

# NOTA: A pequena diferença entre as duas quantidades é erro de simulação,
# não uma diferença conceitual.


# Exercício 3 -------------------------------------------------------------

# Fixa o ponto inicial do gerador pseudoaleatório
set.seed(20260509)

# Probabilidade de estados da economia
prob_expansao <- 0.7
prob_recessao <- 0.3

# Retornos mensais de uma ação e suas probabilidades
retornos_expansao <- c(0.09, 0.04, -0.02)
probabilidades_retornos_expansao <- c(0.25, 0.50, 0.25)
retornos_recessao <- c(0.03, -0.04, -0.11)
probabilidades_retornos_recessao <- c(0.15, 0.45, 0.40)

# Número de meses da simulação
n_meses <- 20000

# Simulação do estado da economia
estado <- sample(
  c("expansao", "recessao"),
  size = n_meses,
  replace = TRUE,
  prob = c(prob_expansao, prob_recessao)
)

# cria uma funcao para sortear o retorno condicional ao estado observado
# separar essa etapa reduz o aninhamento do codigo principal
sortear_retorno <- function(estado_atual){
  if (estado_atual == "expansao") {
    # distribuiçãoo de retornos quando a economia está em expansão
    sample(
      retornos_expansao,
      size = 1,
      replace = TRUE,
      prob = probabilidades_retornos_expansao
    )
  } else {
    # distribuição de retornos quando a economia está em recessão
    sample(
      retornos_recessao,
      size = 1,
      replace = TRUE,
      prob = probabilidades_retornos_recessao
    )
  }
}

# map_dbl() aplica a funcao a cada estado e devolve um vetor numerico
retornos_acao <- map_dbl(estado, sortear_retorno)

# Cria um tibble com o estado da economia mensal e o respectivo retorno
dados_retorno <- tibble(
  estado = estado,
  retorno = retornos_acao
)

# Retorno esperado incondicional
retorno_esperado_incondicional <- mean(retornos_acao)
retorno_esperado_incondicional

# Retorno esperado condicional
retorno_esperado_condicional <- dados_retorno |> 
  group_by(estado) |> 
  summarise(
    valor_esperado = mean(retorno)
  )
retorno_esperado_condicional

# Probabilidade de retorno negativo
probabilidade_retorno_negativo_acao <- mean(retornos_acao < 0)
probabilidade_retorno_negativo_acao

# Exercício 4 -------------------------------------------------------------

# Fixa o ponto inicial do gerador pseudoaleatório
set.seed(20260510)

# Cenários de mercado e suas probabilidades
cenarios_mercado <- c("boom", "estabilidade", "crise")
prob_cenarios_mercado <- c(0.20, 0.50, 0.30)

# Número de meses simulados
n_meses_cenarios <- 15000

# Sorteio de cenários de mercado
cenarios <- sample(
  cenarios_mercado,
  size = n_meses_cenarios,
  replace = TRUE,
  prob = prob_cenarios_mercado
)

# case_when() recodifica cada cenario no retorno correspondente do ativo
retornos_ativo_a <- case_when(
  cenarios == "boom" ~ 0.12,
  cenarios == "estabilidade" ~ 0.03,
  cenarios == "crise" ~ -0.09
)

retornos_ativo_b <- case_when(
  cenarios == "boom" ~ 0.07,
  cenarios == "estabilidade" ~ 0.02,
  cenarios == "crise" ~ -0.04
)

# Retorno esperado e desvio padrão do ativo A
valor_esperado_ativo_a <- mean(retornos_ativo_a)
valor_esperado_ativo_a
desvio_padrao_ativo_a <- sd(retornos_ativo_a)
desvio_padrao_ativo_a

# Retorno esperado e desvio padrão do ativo B
valor_esperado_ativo_b <- mean(retornos_ativo_b)
valor_esperado_ativo_b
desvio_padrao_ativo_b <- sd(retornos_ativo_b)
desvio_padrao_ativo_b

# Covariância entre os retornos dos ativos
covariancia_ativos <- cov(retornos_ativo_a, retornos_ativo_b)
covariancia_ativos

# Correlação entre os retornos dos ativos
correlacao_ativos <- cor(retornos_ativo_a, retornos_ativo_b)
correlacao_ativos

# Probabilidade de retornos negativos para ambos os ativos no mesmo mês
probabilidade_perda_conjunta <-
  mean(retornos_ativo_a < 0 & retornos_ativo_b < 0)
probabilidade_perda_conjunta

# Cálculo dos retornos da carteira
retornos_carteira <- retornos_ativo_a * 0.60 + retornos_ativo_b * 0.40

# Valor esperado dos retornos da carteira
valor_esperado_carteira <- mean(retornos_carteira)
valor_esperado_carteira

# Desvio-padrão dos retornos da carteira
desvio_padrao_carteira <- sd(retornos_carteira)
desvio_padrao_carteira

# Análise: a carteira não oferece uma boa diversificação, tendo em vista que
# os ativos possuem uma forte correlação positiva e que os retornos dos dois
# ativos são parecidos, enquanto que o risco do ativo B é consideravelmente
# menor em comparação ao ativo A. O risco da carteira fica mais próximo do
# risco do ativo A em comparação ao risco do ativo B.

# Exercício 5 -------------------------------------------------------------

# Fixa o ponto inicial do gerador pseudoaleatório
set.seed(20260511)

# Cria a função simula_mes() para simular um único mês da operação
# Gera uma amostra pseudoaleatória de tamanho 1 para o cenário mês
simula_mes <- function(){
  cenario <- sample(
    c("aquecido", "normal", "fraco"),
    size = 1,
    replace = TRUE,
    prob = c(0.25, 0.50, 0.25)
  )
  # Define o número de contratos e probabilidade de inadimplência por cenário
  if (cenario == "aquecido") {
    n_contratos <- 120
    p_inadimplencia <- 0.03
  } else if (cenario == "normal") {
    n_contratos <- 90
    p_inadimplencia <- 0.06
  } else {
    n_contratos <- 60
    p_inadimplencia <- 0.10
  }
  # gera uma amostra pseudoaleatoria da variavel indicadora 
  # de inadimplencia: 0 = adimplente, 1 = inadimplente
  contratos <- sample(
    c(0, 1),
    size = n_contratos,
    replace = TRUE,
    prob = c(1 - p_inadimplencia, p_inadimplencia)
  )
  # conta quantos contratos ficaram inadimplentes
  n_inadimplentes <- sum(contratos)
  # calcula lucro total do mes:
  # adimplentes geram lucro de 300; 
  # inadimplentes geram prejuizo de 2500
  lucro <- 300 * (n_contratos - n_inadimplentes) -
    2500 * n_inadimplentes
  # armazena os resultados do mes simulado em uma linha
  # no próximo código, essa tibble permitirá juntar todos 
  # os meses com a função bind_rows()
  tibble(
    cenario = cenario,
    n_contratos = n_contratos,
    n_inadimplentes = n_inadimplentes,
    lucro = lucro
  )
}

# repete a mesma expressao 10.000 vezes
# simplify = FALSE mantem cada tibble como um elemento de uma lista
resultados_lista <- replicate(
  10000,
  simula_mes(),
  simplify = FALSE
)

# junta a lista de tibbles em uma unica base de dados
# cada linha representa um mes simulado
resultados_simulados <- bind_rows(resultados_lista)

# Valor esperado do lucro mensal
valor_esperado_lucro_mensal <- mean(resultados_simulados$lucro)
valor_esperado_lucro_mensal

# Desvio-padrão do lucro mensal
desvio_padrao_lucro_mensal <- sd(resultados_simulados$lucro)
desvio_padrao_lucro_mensal

# Valor esperado do lucro mensal em cada cenario
valor_esperado_lucro_cenario <- resultados_simulados |> 
  group_by(cenario) |> 
  summarise(
    valor_esperado_lucro = mean(lucro)
  )
valor_esperado_lucro_cenario

# Covariância entre o número de inadimplentes e o lucro mensal
cov_n_inadimplentes_lucro_mensal <- cov(resultados_simulados$n_inadimplentes,
                                        resultados_simulados$lucro)
cov_n_inadimplentes_lucro_mensal

# Correlação entre o número de inadimplentes e o lucro mensal
cor_n_inadimplentes_lucro_mensal <- cor(resultados_simulados$n_inadimplentes,
                                        resultados_simulados$lucro)
cor_n_inadimplentes_lucro_mensal

# Probabilidade de lucro mensal negativo
prob_lucro_mensal_negativo <- mean(resultados_simulados$lucro < 0)
prob_lucro_mensal_negativo
