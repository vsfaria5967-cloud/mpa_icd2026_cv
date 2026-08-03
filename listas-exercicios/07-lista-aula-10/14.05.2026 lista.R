# Arquivo: 14.05.2026-lista-resolucao.R
# Autor(a): Vinícius Silva Faria
# Data: 14/05/2026
# Objetivo: Resolução da lista de exercícios da aula 10


# Configuracoes globais  ------------------------------------

# define opções globais para exibição de números
options(digits = 5, scipen = 999)

# carrega os pacotes necessários
library(here)      # para usar caminhos relativos
library(tidyverse) # inclui readr, dplyr, tidyr, ggplot2 etc.


# Exercício 1 -------------------------------------------------------------

# Questão 1

# Como a=5, c=3, m=8 e X0=1, aplicamos:
# Xn = (5Xn-1 + 3) mod 8
# X0 = 1
# X1 = (5 * 1 + 3) mod 8 = 8 mod 8 = 0
# U1 = 0 / 8 = 0
# X2 = (5 * 0 + 3) mod 8 = 3 mod 8 = 3
# U2 = 3 / 8 = 0,375
# X3 = (5 * 3 + 3) mod 8 = 18 mod 8 = 2
# U3 = 2 / 8 = 0,25
# X4 = (5 * 2 + 3) mod 8 = 13 mod 8 = 5
# U4 = 5 / 8 = 0,625
# X5 = (5 * 5 + 3) mod 8 = 28 mod 8 = 4
# U5 = 4 / 8 = 0,5
# X6 = (5 * 4 + 3) mod 8 = 23 mod 8 = 7
# U6 = 7 / 8 = 0,875
# X7 = (5 * 7 + 3) mod 8 = 38 mod 8 = 6
# U7 = 6 / 8 = 0,75
# X8 = (5 * 6 + 3) mod 8 = 33 mod 8 = 1
# U8 = 1 / 8 = 0,125

# Parâmetros do gerador
a <- 5
c <- 3
m <- 8

calculo_manual_ex1 <- tibble(
  iteracao = 1:9,
  x_anterior = c(1, 0, 3, 2, 5, 4, 7, 6, 1)
  ) |> 
    mutate(
      valor_bruto = a * x_anterior + c,
      x = valor_bruto %% m,
      v = x/m
    )

# Exibe o resultado
calculo_manual_ex1

# Portanto os oito primeiros valores de Xn são:
# 0, 3, 2, 5, 4, 7, 6, 1
# Os valores correspondentes de Un são:
# 0, 0,375, 0,25, 0,625, 0,5, 0,875, 0,75, 0,125
# Na oitava iteração a sequência retorna à semente: X8 = 1 = X0

# Questão 2

library(tidyverse)

lcg <- function(n, seed, a, c, m) {
  # Crie vetores vazios para guardar os valores gerados.
  x <- numeric(n)
  u <- numeric(n)
  
  # No início, o valor anterior é a semente X_0.
  x_anterior <- seed
  
  for (i in 1:n) {
    # 1. Calcule a expressão a * X_{n-1} + c.
    valor_bruto <- a * x_anterior + c
    
    # 2. Guarde em x[i] o resto da divisão por m.
    x[i] <- valor_bruto %% m
    
    # 3. Divida x[i] por m para obter u[i], um valor em [0, 1).
    u[i] <- x[i] / m
    
    # 4. O valor guardado em x[i] será usado na próxima repetição.
    x_anterior <- x[i]
  }
  
  # Organize a sequência gerada em uma tabela.
  resultado <- tibble(
    iteracao = 1:n,
    x = x,
    u = u
  )
  
  return(resultado)
}

# Teste sua função com os parâmetros do cálculo manual.
teste_lcg <- lcg(n = 8, seed = 1, a = 5, c = 3, m = 8)
teste_lcg

# Questão 3

# Gera 20 termos
sequencia_20 <- lcg(n = 20, seed = 1, a = 5, c = 3, m = 8)

# Exibe o resultado
sequencia_20

# A sequência retorna à semente na oitava iteração.Os valores de U se repetem
# nas mesmas posições de Xn correspondentes.

# Questão 4

# Gera 100 termos com o segundo conjunto
sequencia_conjunto_2 <- lcg(n = 100, seed = 10, a = 11, c = 4, m = 23)

# Exibe o resultado
sequencia_100

# resume valores distintos e repeticoes
sequencia_conjunto_2 |>
  summarise(
    valores_distintos_na_sequencia = n_distinct(x),
    primeira_repeticao_da_semente = min(iteracao[x == 10]),
    primeira_repeticao_do_primeiro_valor_gerado =
      min(iteracao[x == first(x) & iteracao > 1])
  )

# Com esses parâmetros, aparecem 22 valores distintos de x na sequência de 100
# valores e, portanto, 22 valores distintos de u. A sequência retorna à semente
# na iteração 22. O primeiro valor gerado, volta a aparecer na iteração 23.

# Questão 5

# Os números gerados são pseudoaleatórios porque não surgem de um processo
# aleatório verdadeiro: eles são produzidos por uma regra determinística. Dada a
# mesma semente e os mesmos parâmetros, a sequência será sempre exatamente a
# mesma.

# A semente define o ponto inicial da sequência. Alterar a semente altera o
# ponto de partida e, portanto, a sequência observada.

# Eles não formam uma uniforme contínua verdadeira, pois pertencem a uma grade
# finita de valores. Ainda assim, com módulo grande e bons parâmetros, a
# sequência pode imitar algumas propriedades de variáveis pseudoaleatórias com
# distribuição uniforme.

# A existência de ciclos é uma limitação fundamental: em algum momento, como há
# apenas um número finito de estados possíveis, a sequência necessariamente se
# repete. Geradores modernos procuram tornar esse ciclo muito longo e melhorar
# as propriedades estatísticas da sequência.

# Números pseudoaleatórios uniformes são a base de muitas simulações de Monte
# Carlo. Primeiro geramos valores que imitam uma Uniforme(0,1); depois, podemos
# transformá-los para simular outras distribuições de probabilidade.

# Exercício 2 -------------------------------------------------------------

# Número de repetições
N <- 1000

# ITEM A

# Questão 1

# Definindo a função
f_a <- function(x) {
  x^2
}

# Questão 2

# Define a semente
set.seed(20260514)

# Questão 3

# limites de integracao
a <- 1
b <- 3

# gera valores pseudoaleatorios uniformes em [a, b]
u_a <- runif(N, min = a, max = b)

# Questão 4

# estimativa de Monte Carlo
estimativa_mc_a <- (b - a) * mean(f_a(u_a))
estimativa_mc_a

# Questão 5

# referencia numerica com integrate()
referencia_integrate_a <- integrate(f_a, lower = a, upper = b)$value
referencia_integrate_a

# Questão 6

# erro absoluto
erro_absoluto_a <- abs(estimativa_mc_a - referencia_integrate_a)
erro_absoluto_a

# Questão 7

# A estimativa por Monte Carlo fica próxima da referência numérica obtida com
# integrate() conforme o erro absoluto, mas não deve ser exatamente igual, pois
# foi obtida com apenas 1000 valores pseudoaleatórios mas, como sabemos, podemos
# melhorar a aproximação aumentando N.

# ITEM B

# Questão 1

# Definindo a função
f_b <- function(x) {
  sin(x)
}

# Questão 2

# Define a semente
set.seed(20260514)

# Questão 3

# limites de integracao
a <- 0
b <- pi

# gera valores pseudoaleatorios uniformes em [a, b]
u_b <- runif(N, min = a, max = b)

# Questão 4

# estimativa de Monte Carlo
estimativa_mc_b <- (b - a) * mean(f_b(u_b))
estimativa_mc_b

# Questão 5

# referencia numerica com integrate()
referencia_integrate_b <- integrate(f_b, lower = a, upper = b)$value
referencia_integrate_b

# Questão 6

# erro absoluto
erro_absoluto_b <- abs(estimativa_mc_b - referencia_integrate_b)
erro_absoluto_b

# Questão 7

# Novamente, a estimativa de Monte Carlo fica próxima da referência numérica
# obtida com integrate(), e, novamente, apresenta erro de simulação. Aumentar N
# tende a reduzir esse erro, de acordo com a Lei dos Grandes Números.
