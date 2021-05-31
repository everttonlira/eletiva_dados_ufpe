## Script da aula 7 ##

## Descoberta

# Instalando pacotes
library(tidyverse)

# Observando os dados do pacote sobre mortalidade infantil nos países
glimpse(gapminder) # olhada nos dados

# Retorna a estrutura
status(gapminder) 

# Frequência das variáveis
freq(gapminder) 

# Explora variáveis numéricas
plot_num(gapminder) 

# Retorna estatísticas das variáveis numéricas
profiling_num(gapminder)

## Estruturação

# Carregando pacotes
library(data.table)
library(dplyr)

# Carregando dados sobre COVID
dados_gerais <- fread("https://covid.ourworldindata.org/data/owid-covid-data.csv") 

# Criando um vetor com 6 países da américa do Sul
sul <- c("Argentina", "Brazil", "Chile", "Colombia" , "Paraguay", "Uruguay") 

# Filtrando apenas os casos dos 6 países
america_sul <- dados_gerais %>% filter(location %in% sul)

# Criando matrizes
new_sul <- america_sul %>% group_by(location) %>% mutate(row = row_number()) %>% select(location, new_cases, row) 

# Filtrando para igualar número de casos
result <- new_sul %>% group_by(location) %>% filter(row == max(row))

new_sul <- new_sul %>% filter(row<=min(result$row)) 

# Pivotando o data frame do formato long para wide

sul_w <- new_sul %>% pivot_wider(names_from = row, values_from = new_cases) %>% remove_rownames %>% column_to_rownames(var="location") 

head(sul_w)

## Limpeza

# Procurando se há NA
is.na.data.frame(sul_w)

# Removendo NA
sul_w <- na.omit(dados)

## Enriquecimento

# Carregando pacotes
library(geobr)
library(xlsx)

# Baixando dados sobre municípios brasileiros
muni <- read_municipality(year = 2010)

# Carregando dados
ipea2 <- read_xlsx("ipeadatamun.xlsx")

# Juntando os bancos
muni <- dplyr::left_join(muni, ipea2, by = c("code_muni" = "Código"))

## Validação
install.packages("validate")
library(validate)

# Observando o banco
regras_sul <- validator(new_cases >= 0, new_deaths >= 0)

validacao_sul <- confront(america_sul, regras_sul)

summary(validacao_sul)

plot(validacao_sul)