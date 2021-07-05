## Avaliação da disciplina ##

## Extraindo a base de dados de COVID em Pernambuco
covid <- read.csv("https://dados.seplag.pe.gov.br/apps/basegeral.csv", sep = ";", na.strings = "")

## Calculando os totais de casos confirmados 

# Carregando pacotes
library(tidyverse)
library(lubridate)

# Criando uma variável binária para confirmados
covid$confirmados <- ifelse(covid$classe == "CONFIRMADO", 1, 0)

# Calculando o total de casos confirmados por município
covid <- covid %>% group_by(cd_municipio) %>% mutate(casos_confirmados = sum(confirmados))
# Exemplos: Recife (136.094 casos), Caruaru (31.942 casos), Petrolina (28.070)

# Calculando o total de casos confirmados por semana epidemiológica

# Transformando as variáveis dt_notificacao e dt_obito para as.Date
covid$dt_notificacao <- as.Date(covid$dt_notificacao, format = "%Y-%m-%d")
covid$dt_obito <- as.Date(covid$dt_obito, format = "%Y-%m-%d")

# Calculando a semana epidemiológica
covid <- covid %>% mutate(semana_epi = epiweek(covid$dt_notificacao))

# Criando dummy para óbito
covid$obitos <- ifelse(covid$evolucao == "OBITO", 1, 0)

# Calculando as mortes por município e por semana epidemiológica
covid_obitos <- covid %>% filter(evolucao == "OBITO") %>% group_by(municipio) %>% count(semana_epi)

## Enriquecendo a base com a população dos municípios

# Carregando os dados
ibge <- readxl::read_xlsx("bases_originais/tabela6579.xlsx")

# Renomeando colunas
names(ibge)[1] <- "municipio"
names(ibge)[2] <- "populacao"

# Retirando acentos
trim <- function (x) gsub("^\\s+|\\s+$", "", x) 
ibge$municipio <- trim(ibge$municipio)

# Colocando em letra maiúscula
ibge$municipio <- toupper(ibge$municipio) 

# Listando todos os acentos
unwanted_array <- list('S'='S', 's'='s', 'Z'='Z', 'z'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E','Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                       'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c','è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o','ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y' )

# Criando função para retirar os acentos 
for(i in seq_along(unwanted_array)) + ibge$municipio <- gsub(names(unwanted_array)[i],unwanted_array[i],ibge$municipio)

# Criando UF a partir dos últimos caracteres
ibge$UF <- str_sub(ibge$municipio,-4, -1) 

# Removendo os parênteses
ibge$UF <- gsub("[()]", " ", ibge$UF)

# Retirando os últimos dÍgitos da coluna
ibge$municipio <- str_sub(ibge$municipio, 1, str_length(ibge$municipio)-4)

# Renomeando a coluna UF
names(ibge)[3] <- "UF"

# Mergindo os bancos 
ibge_novo <- subset(ibge, uf == "PE")
covid_novo <- inner_join(covid, ibge)
covid_novo <- inner_join(covid_obitos, ibge)

## Calculando incidência e letalidade

# Calculando a incidência por 100 mil habitantes
incidencia <- covid %>% filter(classe == "CONFIRMADO") %>% group_by(municipio) %>% group_by(semana_epi) %>% mutate(incidencia = casos_confirmados/100000)

# Calculando a letalidade por 100 mil habitantes
letalidade <- covid %>% filter(evolucao == "OBITO") %>% group_by(municipio) %>% group_by(semana_epi) %>% mutate(letalidade = sum(obitos)/100000)
