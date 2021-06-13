## Script da aula 9 ##

## Valores ausentes

# Instalando e carregando pacotes
library(data.table)
library(funModeling)
library(tidyverse)

# Baixando os dados
escolas <- read.csv('http://dados.recife.pe.gov.br/dataset/e9c0d2a3-9f5d-4a4e-815d-66c2e736c7e5/resource/bb8b70d4-4204-40d3-bc77-409a1651b8b9/download/escolas2015.csv', sep = ";")

# Identificando NA's existentes
status(escolas) # A maior quantidade de NA's está na variável que mostra o número do FAX da escola

# Criando uma shadow matrix
shadow <- as.data.frame(abs(is.na(escolas)))
head(shadow)

# Procurando por padrões entre os NA
b1 <- shadow[which(sapply(shadow, sd) > 0)]

cor(b1)

cor(escolas, b1, use = "pairwise.complete.obs") 

# Existe um padrão nos casos faltantes, que era esperado de encontrar:
# associação forte entre as variáveis FAX e telefone da escola

## Outliers

# Carregando pacotes
library(dplyr)
library(data.table)
library(plotly)

# Baixando dados de COVID em Pernambuco
covid_pe <- fread('https://dados.seplag.pe.gov.br/apps/basegeral.csv')

covid_pe_mun <- covid_pe %>% count(municipio, sort = T, name = 'casos') %>% mutate(casos2 = sqrt(casos), casosLog = log10(casos))

# Procurando por outliers
# Distância interquartil
plot_ly(y = covid_pe_mun$casosLog, type = "box", text = covid_pe_mun$municipio, boxpoints = "all", jitter = 0.3) # Caso maior = Recife, caso menor = outro estado
boxplot.stats(covid_pe_mun$casos)$out

# Usando o filtro de Hamper
lower_bound <- median(covid_pe_mun$casosLog) - 3 * mad(covid_pe_mun$casosLog, constant = 1)
upper_bound <- median(covid_pe_mun$casosLog) + 3 * mad(covid_pe_mun$casosLog, constant = 1)
(outlier_ind <- which(covid_pe_mun$casosLog < lower_bound | covid_pe_mun$casosLog > upper_bound))

## Imputação

# Carregando pacote
library(Hmisc)

# Imputando valores pela média da variável
escolas$equipamentos_televisao <- impute(escolas$equipamentos_televisao, fun = mean) 

# Checando se ainda há NA's
status(escolas)

# Imputação aleatória
(escolas$equipamentos_videocassete <- impute(escolas$equipamentos_videocassete, "random"))

# Checando se ainda há NA's
status(escolas)