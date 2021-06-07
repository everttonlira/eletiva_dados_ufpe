## Script da aula 8 ##

## Tipos e fatores

# Criando um fator
partidos <- factor(c("PT", "PSDB", "MDB", "REDE", "PSB"))
levels(partidos)

# Criando fator e etiquetas
alinhamento <- c("0", "0", "1", "0", "1")
recode <- c(alinhado = 1, nao_alinhado = 0)

alinhamento <- factor(alinhamento, levels = recode, labels = names(recode))

alinhamento

## Mais fatores

# Instalando e carregando pacotes
install.packages("ade4")
install.packages("arules")
library(ade4)
library(arules)
library(forcats)

# Lendo a base
facebook <- read.table("bases_originais/dataset_Facebook.csv", sep=";", header = T)
str(facebook)

# Convertendo em fatores
for(i in 2:7) {facebook[,i] <- as.factor(facebook[,i])} 

# Filtrando por tipo
fatores_fb <- unlist(lapply(facebook, is.factor))  
facebook_fator <- facebook[ , fatores_fb]
str(facebook_fator)

# Fazendo one hot encoding
dummy_fb <- acm.disjonctif(facebook_fator)

# Fazendo discretização
inter_fb <- unlist(lapply(facebook, is.integer))  
inter_facebook <- facebook[, inter_fb]
str(inter_facebook)

inter_facebook$Page.total.likes.Disc <- discretize(inter_facebook$Page.total.likes, method = "interval", breaks = 3, labels = c("Poucos", "Médio", "Muito"))

## Data table

# Carregando pacote
library(data.table)
library(dplyr)
library(gapminder)

gapminder_dt <- gapminder %>% setDT()

class(gapminder_dt)

# Regressão linear com o data.table
reg <- gapminder_dt[, lm(formula = lifeExp ~ pop + gdpPercap)]

summary(reg)

## Dplyr

# Sumarizar a expectativa de vida dos países
count(gapminder, lifeExp)

# Sumarizar com agrupamento por continente
gapminder %>% group_by(continent) %>% summarise(média = mean(lifeExp))

# Manipulando colunas: organizando por expectatvida de vida (maior para menor)
gapminder %>% select(country, year, lifeExp, pop, gdpPercap) %>% arrange(desc(lifeExp))

# Manipulando casos: criando uma nova informação
gapminder %>% group_by(country) %>% mutate(mean_gdp = mean(gdpPercap))