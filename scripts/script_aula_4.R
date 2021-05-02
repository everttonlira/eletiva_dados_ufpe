## Script da aula 4 ##

## ETL real

# Carregando a base de sinistros da prefeitura do Recife
sinistrosRecife2018Raw <- read.csv2("http://dados.recife.pe.gov.br/dataset/44087d2d-73b5-4ab3-9bd8-78da7436eed1/resource/2485590a-3b35-4ad0-b955-8dfc36b61021/download/acidentes_2018.csv", sep = ";", encoding = "UTF-8")

sinistrosRecife2019Raw <- read.csv2("http://dados.recife.pe.gov.br/dataset/44087d2d-73b5-4ab3-9bd8-78da7436eed1/resource/3531bafe-d47d-415e-b154-a881081ac76c/download/acidentes-2019.csv", sep = ";", encoding = "UTF-8")

sinistrosRecife2020Raw <- read.csv2('http://dados.recife.pe.gov.br/dataset/44087d2d-73b5-4ab3-9bd8-78da7436eed1/resource/fc1c8460-0406-4fff-b51a-e79205d1f1ab/download/acidentes_2020-novo.csv', sep = ';', encoding = 'UTF-8')

sinistrosRecife2021Raw <- read.csv2('http://dados.recife.pe.gov.br/dataset/44087d2d-73b5-4ab3-9bd8-78da7436eed1/resource/2caa8f41-ccd9-4ea5-906d-f66017d6e107/download/acidentes_2021-jan.csv', sep = ';', encoding = 'UTF-8')

# Procurando por variáveis diferentes
setdiff(names(sinistrosRecife2019Raw), names(sinistrosRecife2020Raw))

# Retirando as colunas que sobram
sinistrosRecife2019Mod <- sinistrosRecife2019Raw[, -(10:12)]
sinistrosRecife2018Mod <- sinistrosRecife2018Raw[, -(10:12)]

# Colocando o nome da primeira variável com letras minúsculas
names(sinistrosRecife2019Mod)[1] <- "data"
names(sinistrosRecife2018Mod)[1] <- "data"

# Juntando as bases 
sinistrosRecifeRaw <- rbind(sinistrosRecife2018Mod, sinistrosRecife2019Mod,
                            sinistrosRecife2020Raw, sinistrosRecife2021Raw)

# Transformando uma coluna em fator
sinistrosRecifeRaw$data <- as.factor(sinistrosRecifeRaw$vitimasfatais)

## Extração

# Criando a função NA zero
naZero <- function(x) {
  x <- ifelse(is.na(x), 0, x)
}

# Mantendo apenas a base final e a função NA Zero

rm(list = c("sinistrosRecife2017Raw", "sinistrosRecife2018Raw",
            "sinistrosRecife2018Mod", "sinistrosRecife2019Raw",
            "sinistrosRecife2019Mod", "sinistrosRecife2020Raw",
            "sinistrosRecife2021Raw"))

# Acessando quanto cada objeto está ocupando
for (itm in ls()) { 
  print(formatC(c(itm, object.size(get(itm))), 
                format="d", 
                width=30), 
        quote=F)
}

# O objeto sinistrosRecifeRaw é o que mais ocupa memória, seguido de dois
# subsets utilizado no exercício anterior

## Leitura

# Adicionando formato de exportação
write.csv2(sinistrosRecifeRaw, 'bases_tratadas/sinistrosRecife.csv2')
saveRDS(sinistrosRecifeRaw, 'bases_tratadas/sinistrosRecife.rds')
xlsx::write.xlsx(sinistrosRecifeRaw, file="sinistrosRecife.xlsx", 
                 sheetName="Sinistros Recife")

# Adicionando formato de leitura
sinistrosRecife <- read.csv2('bases_tratadas/sinistrosRecife.csv2', sep = ';')
sinistrosRecife <- readRDS('bases_tratadas/sinistrosRecife.rds')


# Comparando
install.packages("microbenchmark")
library(microbenchmark)

microbenchmark(a <- saveRDS(sinistrosRecifeRaw, "bases_tratadas/sinistrosRecife.rds"), b <- write.csv2(sinistrosRecifeRaw, "bases_tratadas/sinistrosRecife.csv2"), times = 30L)

microbenchmark(a <- readRDS('bases_tratadas/sinistrosRecife.rds'), b <- read.csv2('bases_tratadas/sinistrosRecife.csv2', sep = ';'), times = 10L)

# Para carregamento, o formato RDS carregou 3 vezes mais rápudo que o CSV2.
# Para leitura, o formato RDS obteve um tempo quase 9 vezes menor que o formato
# CSV2. 