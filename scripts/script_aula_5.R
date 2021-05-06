## Script da aula 5 ##

## Extrações básicas

# CSV: informações sobre as escolas públicas e privadas de Recife
escolas_recife <- read.csv2('http://dados.recife.pe.gov.br/dataset/b5b2cfab-8c7c-4ad9-91f3-e500777a46fc/resource/cd196f7e-661e-41f1-8488-9494b20ffee2/download/escolas.csv')

# JSON: relação de pessoas vacinadas contra a COVID-19 em Recife
install.packages("rjson")
library(rjson)

relacao_vacinados_recife <- fromJSON(file = 'http://dados.recife.pe.gov.br/dataset/f381d9ea-4839-44a6-b4fe-788239189900/resource/159fabd3-f156-411a-a949-e6b5c0bc2ad8/download/metadados_vacinados.json')

relacao_vacinados_recife <- as.data.frame(relacao_vacinados_recife)

# Texto com CSV2: relação de nomes e gênero
genero <- read.csv2('https://archive.ics.uci.edu/ml/machine-learning-databases/00591/name_gender_dataset.csv', sep = ',', encoding = 'UTF-8')

## Carga incremental

# Carregando pacote
library(dplyr)

# Carregando a base de dados original: atendimentos da Defesa Civil
atendimentos_recife <- read.csv2('http://dados.recife.pe.gov.br/dataset/99eea78a-1bd9-4b87-95b8-7e7bae8f64d4/resource/9afa68cf-7fd9-4735-b157-e23da873fef7/download/156_diario.csv', sep = ';', encoding = 'UTF-8')

# Retirando algumas linhas para realizar a atividade
atendimentos_recife <- atendimentos_recife[-(10:30),]

# Carregando a base completa para atualizar
novo_atendimentos_recife <- read.csv2('http://dados.recife.pe.gov.br/dataset/99eea78a-1bd9-4b87-95b8-7e7bae8f64d4/resource/9afa68cf-7fd9-4735-b157-e23da873fef7/download/156_diario.csv', sep = ';', encoding = 'UTF-8')

# Criando uma chave substituta para comparação
atendimentos_recife$chave_substituta = apply(atendimentos_recife[, c(5,6,7,11)], MARGIN = 1, FUN = function(i) paste(i, collapse = ""))

novo_atendimentos_recife$chave_substituta = apply(novo_atendimentos_recife[, c(5,6,7,11)], MARGIN = 1, FUN = function(i) paste(i, collapse = ""))

# Criando a base de comparação
incremento_atendimentos <- (!novo_atendimentos_recife$chave_substituta %in% atendimentos_recife$chave_substituta)

## Extração com scraping

# Carregando pacote
library(rvest)

# Carregando informações sobre os partidos políticos brasileiros do site do TSE
url <- "https://www.tse.jus.br/partidos/partidos-politicos"

url_tables <- url %>% read_html %>% html_nodes("table")

partidos <- as.data.frame(html_table(url_tables[1]))

partidos