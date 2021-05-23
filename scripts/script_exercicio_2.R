## Script do exercício 2 ##

# Carregando os pacotes
library(ff)
library(ffbase)
library(tidyverse)
library(rio)
library(rvest)

# Extraindo as bases
alunos_20 <- read.csv2.ffdf(file = "http://dados.recife.pe.gov.br/dataset/ce5168d4-d925-48f5-a193-03d4e0f587c7/resource/9dc84eed-acdd-4132-9f1a-a64f7a71b016/download/situacaofinalalunos2020.csv")
alunos_19 <- read.csv2.ffdf(file = "http://dados.recife.pe.gov.br/dataset/ce5168d4-d925-48f5-a193-03d4e0f587c7/resource/3b03a473-8b20-4df4-8628-bec55541789e/download/situacaofinalalunos2019.csv")
alunos_18 <- read.csv2.ffdf(file = "http://dados.recife.pe.gov.br/dataset/ce5168d4-d925-48f5-a193-03d4e0f587c7/resource/8f3196b8-c21a-4c0d-968f-e2b265be4def/download/situacaofinalalunos2018.csv")
alunos_17 <- read.csv2.ffdf(file = "http://dados.recife.pe.gov.br/dataset/ce5168d4-d925-48f5-a193-03d4e0f587c7/resource/70c4e6fc-91d2-4a73-b27a-0ad6bda1c84d/download/situacaofinalalunos2017.csv")
alunos_16 <- read.csv2.ffdf(file = "http://dados.recife.pe.gov.br/dataset/ce5168d4-d925-48f5-a193-03d4e0f587c7/resource/f42a3c64-b2d7-4e2f-91e5-684dcd0040b9/download/situacaofinalalunos2016.csv")
alunos_15 <- read.csv2.ffdf(file = "http://dados.recife.pe.gov.br/dataset/ce5168d4-d925-48f5-a193-03d4e0f587c7/resource/264f0a37-ad1c-4308-9998-4f0bd3c6561f/download/situacaofinalalunos2015.csv")
alunos_14 <- read.csv2.ffdf(file = "http://dados.recife.pe.gov.br/dataset/ce5168d4-d925-48f5-a193-03d4e0f587c7/resource/0a2aec2f-9634-4408-bbb4-37e1f9c74aa1/download/situacaofinalalunos2014.csv")
alunos_13 <- read.csv2.ffdf(file = "http://dados.recife.pe.gov.br/dataset/ce5168d4-d925-48f5-a193-03d4e0f587c7/resource/95eb9ea8-cd75-4efa-a1ba-ba869f4e92b9/download/situacaofinalalunos2013.csv")
alunos_12 <- read.csv2.ffdf(file = "http://dados.recife.pe.gov.br/dataset/ce5168d4-d925-48f5-a193-03d4e0f587c7/resource/f6633c26-be36-4c27-81cb-e77d90316cff/download/situacaofinalalunos2012.csv")
alunos_11 <- read.csv2.ffdf(file = "http://dados.recife.pe.gov.br/dataset/ce5168d4-d925-48f5-a193-03d4e0f587c7/resource/9a694ab5-99ab-4ff1-ac6b-c97917c6a762/download/situacaofinalalunos2011.csv")

# Colocando as bases juntas
alunos_rec <- ffdfrbind.fill(alunos_11, alunos_12, alunos_13, alunos_14, 
                                     alunos_15, alunos_16, alunos_17, alunos_18, 
                                     alunos_19, alunos_20, clone = TRUE)

# Salvando base

saveRDS(alunos_rec, 'bases_originais/situacao_aluno_rec.rds')

#### lipando a staging area

rm(list=ls())