## Script do exercício 3 ##

## Extraindo a base de dados de COVID em Pernambuco
covid <- read.csv("https://dados.seplag.pe.gov.br/apps/basegeral.csv", sep = ";", na.strings = "")

## Corrigindo os NAs da coluna "sintomas" através de imputação randômica
# Carregando pacotes
library(Hmisc)
library(funModeling)

# Checando a quantidade de valores ausentes na variável
status(covid)

# Imputando aleatoriamente
covid$sintomas <- impute(covid$sintomas, "random")

# Checando novamente a quantidade de NA
status(covid)

## Calculando o total de casos confirmados e negativos para cada município

# Carregando pacote
library(tidyverse)

# Criando uma variável binária para confirmados
covid$confirmados <- ifelse(covid$classe == "CONFIRMADO", 1, 0)

# Criando variável binária para negativos
covid$negativos <- ifelse(covid$classe == "NEGATIVO", 1, 0)

# Calculando os totais de confirmados por município
covid <- covid %>% group_by(cd_municipio) %>% mutate(casos_confirmados = sum(confirmados))

# Calculando os totais de negativos por município
covid <- covid %>% group_by(cd_municipio) %>% mutate(casos_negativos = sum(negativos))

## Calculando uma variável binária se sintomas inclui tosse e quantos casos negativos e positivos têm tosse

# Carregando pacote
library(stringr)

# Criando dummy com base no sintoma "tosse"
covid <- covid %>% mutate(tosse = ifelse(grepl(paste("TOSSE", collapse="|"), sintomas), 'Sim', 'Não'))

# Calculando os casos com tosse
covid <- covid %>% group_by(tosse) %>% mutate(tosse_confirmado = sum(confirmados))
# Confirmados com tosse: 399.832 casos; negativos com tosse: 115.921

## Estimando a média móvel de 7 dias para casos negativos e confirmados

# Carregando pacotes
library(zoo)
library(lubridate)

# Transformando a coluna dt_notificacao para as.Date
covid$dt_notificacao <- as.Date(covid$dt_notificacao, format = "%Y-%m-%d") 

# Criando variável PE
covid <- covid %>% mutate(estado = "PE")

# Agrupando casos confirmados por dia
covid <- covid %>% group_by(estado) %>% group_by(dt_notificacao) %>% mutate(soma_casos_dia = sum(confirmados))

# Agrupando casos negativos por dia
covid <- covid %>% group_by(estado) %>% group_by(dt_notificacao) %>% mutate(soma_negativos_dia = sum(negativos))

# Média móvel de casos confirmados
covid <- covid %>% mutate(confirmados_mm = round(rollmean(x = soma_casos_dia, 7, align = "right", fill = NA), 2))

# Média móvel de casos negativos
covid <- covid %>% mutate(negativos_mm = round(rollmean(x = soma_negativos_dia, 7, align = "right", fill = NA), 2))