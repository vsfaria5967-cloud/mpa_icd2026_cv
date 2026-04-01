# Arquivo: 03-lista-resolucao.R
# Autor(a): Vinícius Silva Faria
# Data: 26/03/2026
# Objetivo: Resolução da lista de exercícios 3

# Configuracoes globais  ------------------------------------

# define opções globais para exibição de números
options(digits = 5, scipen = 999)

# carrega os pacotes necessários
library(here) # para usar caminhos relativos
library(tidyverse) # meta-pacote que inclui readr, dplyr, tidyr...


# Exercício 1 ---------------------------------------------------------------


# importa os arquivos
caminho_csv <- here("data/raw/produtos.csv")
dados_produtos <- read.csv(caminho_csv)

caminho_csv <- here("data/raw/vendas.csv")
dados_vendas <- read.csv(caminho_csv)

caminho_csv <- here("data/raw/clientes.csv")
dados_clientes <- read.csv(caminho_csv)


# analisa os objetos importados
glimpse(dados_produtos)
glimpse(dados_vendas)
glimpse(dados_clientes)

# combina vendas com produtos e clientes
relatorio_vendas <- dados_vendas |> 
  left_join(dados_produtos, by = "codigo_produto") |> 
  left_join(dados_clientes, by = "id_cliente")

relatorio_vendas <- relatorio_vendas |> 
  select(id_venda, data_venda, nome_produto, categoria, quantidade,
         nome_cliente, cidade)

# exibe a estrutura do resultado
glimpse(relatorio_vendas)

# análise
## colunas nome_produto e categoria ficaram com registros NA pelo fato de
## existirem vendas na tabela dados_vendas com produtos que não estavam
## cadastrados na tabela dados_produtos;
## colunas nome_cliente e cidade ficaram com registros NA pelo fato de existirem
## vendas na tabela dados_vendas com clientes que não estavam
## cadastrados na tabela dados_clientes.

# opcional
relatorio_vendas_opcional <- dados_vendas |> 
  full_join(dados_produtos, by = "codigo_produto")
glimpse(relatorio_vendas_opcional)
## assim como no relatorio_vendas, as colunas nome_produto e categoria, além da
## coluna preco_unitario, na id_venda 5, ficaram com valores NA pelo fato de os
## produtos dessa venda em específico não estarem cadastrados na tabela
## dados_produtos. Além disso, foi acrescentada uma linha (observação) a esse
## último relatório pelo fato de existir um produto na tabela dados_produtos
## para o qual não existem vendas na tabela dados_vendas, ficando, por esse
## motivo, com valores NA nas colunas id_venda, id_cliente, data_venda e
## quantidade, as quais correspondem a colunas da tabela dados_vendas.

# Exercício 2 ---------------------------------------------------------------


# importa os arquivos
caminho_csv <- here("data/raw/governanca.csv")
dados_governanca <- read.csv(caminho_csv)

caminho_csv <- here("data/raw/risco.csv")
dados_risco <- read.csv(caminho_csv)

caminho_csv <- here("data/raw/contabeis.csv")
dados_contabeis <- read.csv(caminho_csv)

# analisa os objetos importados
glimpse(dados_governanca)
glimpse(dados_risco)
glimpse(dados_contabeis)

# combina governança, risco e dados contábeis
analise_integrada <- dados_governanca |> 
  left_join(dados_risco, by = "codigo_negociacao") |> 
  left_join(dados_contabeis, by = c("codigo_negociacao", "ano" = "ano"))


analise_integrada <- analise_integrada |> 
  select(empresa, codigo_negociacao, ano, indice_governanca, tipo_controlador,
         comite_auditoria, retorno_anual, volatilidade, beta, roa, alavancagem,
         tamanho_ativo)

# exibe a estrutura do resultado
glimpse(analise_integrada)

# análise
## As colunas retorno_anual, volatilidade, beta, roa, alavancagem e
## tamanho_ativo ficaram com valores NA para a empresa Magazine Luiza devido ao
## fato de não existirem dados para o codigo_negociacao dessa empresa nas
## tabelas dados_risco e dados_contabeis, das quais os dados dessas colunas
## foram importados.
## O uso de left_join é indicado quando desejamos preservar os dados apenas da
## base principal de dados para que não sejam adicionados dados desnecessários
## da outra base que não estejam presentes na base de dados principal.

# Exercício 3 ---------------------------------------------------------------


# importa os arquivos
caminho_csv <- here("data/raw/acoes.csv")
dados_acoes <- read.csv(caminho_csv)

caminho_csv <- here("data/raw/eventos_corporativos.csv")
dados_eventos_corporativos <- read.csv(caminho_csv)

# analisa os objetos importados
glimpse(dados_acoes)
glimpse(dados_eventos_corporativos)

# constrói a base do estudo de eventos
dados_estudo_eventos <- dados_acoes |> 
  inner_join(dados_eventos_corporativos,
             by = c("ticker", "data" = "data_anuncio"))

dados_estudo_eventos <- dados_estudo_eventos |> 
  select(ticker, data, tipo_evento, valor, retorno_diario, volume)

# exibe o objeto final
glimpse(dados_estudo_eventos)

# análise
## O objeto final possui menos linhas que dados_acoes pelo fato de terem sido
## consideradas as colunas data e data_anuncio como chave em conjunto com a
## coluna ticker. Caso as colunas referentes às datas não fossem consideradas
## como chave primária, o objeto final mostraria erroneamente os eventos
## corporativos como se tivessem ocorrido em todas as datas presentes na tabela
## dados_acoes.
## A função inner_join seleciona apenas as linhas cujas chaves aparecem nas duas
## tabelas. Portanto, como não existem eventos corporativos no objeto
## dados_eventos_corporativos para todas as datas do objeto dados_acoes, essas
## datas sem correspondência não serão salvas no objeto final.

# ------------------------- FIM ---------------------------------------------#
