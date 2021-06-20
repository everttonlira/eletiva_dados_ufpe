## Script da aula 10 ##

## Trabalhando com texto

# Carregando pacotes
library(dplyr)
library(pdftools)
library(textreadr)
library(stringr)

# Lendo o PDF

pp <- read_pdf('bases_originais/avaliacao_pp.pdf', ocr = T)

# Juntando as páginas numa string

pp <- pp %>%
  group_by(element_id) %>%
  mutate(all_text = paste(text, collapse = " | ")) %>%
  select(element_id, all_text) %>%
  unique()

# Extraindo as datas do texto
datas <- str_extract_all(pp$all_text, "\\d{2}/\\d{2}")

# Trocando barras por hífens
pp_mod <- gsub("04/02", "04-02", pp$all_text)

# Extraindo por hífen
pp_modf <- str_extract_all(pp_mod, "\\d{2}-\\d{2}")

## Juntando e buscando textos

# Carregando pacote
library(geobr)
library(fuzzyjoin)

# Carregando bancos
# Baixando mapa das cidades
cidades <- read_neighborhood()

# Baixando o mapa de Recife
recife <- cidades %>% filter(name_muni == "Recife")

# Carregando banco
escolas <- read.csv('http://dados.recife.pe.gov.br/dataset/e9c0d2a3-9f5d-4a4e-815d-66c2e736c7e5/resource/bb8b70d4-4204-40d3-bc77-409a1651b8b9/download/escolas2015.csv', sep = ";")

# Reduzindo as bases
recife <- select(recife, name_neighborhood, geom, name_subdistrict)
escolas <- select(escolas, bairro, inep_escola)

# Mudando nome da coluna na base principal
names(recife)[1] <- "bairro"

# Juntando as bases
nova_base <- stringdist_join(recife, escolas, mode='left')

# Procurando texto
base2 <- nova_base %>% mutate(RPA = ifelse(grepl(paste("Rpa 04", collapse="|"), name_subdistrict), '1', '0'))