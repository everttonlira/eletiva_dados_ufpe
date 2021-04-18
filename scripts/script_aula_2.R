## Script da aula 2 ##

## Tipos de objetos 

# Criando vetores 
municipio <- c("Recife", "Bezerros", "Caruaru", "João Pessoa", "São Paulo")
uf <- c("PE", "PE", "PE", "PB", "SP")
populacao <- as.integer(c(1640000, 60000, 300000, 900000, 14000000))
idade <- c(484, 151, 163, 435, 468)

# Criando um data frame
municipios <- data.frame(municipio = municipio, uf = uf, populacao = populacao,
                         idade = idade)

# Visualizando o data frame
municipios

## Simulações e sequência 

# Criando uma variável normal
var_normal <- rnorm(50)

# Criando uma variável binária
var_binaria <- rbinom(50, 1, 0.5)

# Criando uma variável index
var_index <- seq(1, length(var_normal))

## Amostragem e bootstraping

# Criando um bootstraping
set.seed(412)

boots_normal <- replicate(10, sample(var_normal, 10, replace = TRUE)) 
boots_normal

# Criando uma estatística com bootstraping
media_boots_normal_10 <-replicate(10, mean(sample(var_normal, 10, 
                                               replace = TRUE)))

media_boots_normal_10

media_boots_normal_50 <-replicate(50, mean(sample(var_normal, 10,
                                               replace = TRUE))) 

media_boots_normal_50

# Comparando as duas médias
mean(media_boots_normal_10)
mean(media_boots_normal_50)

## Calculando

# Criando uma medida de centralização
library(gapminder)

summary(gapminder)

hist(gapminder$lifeExp)

life_exp_central <- gapminder$lifeExp - mean(gapminder$lifeExp)

hist(life_exp_central)

## Index e operadores lógicos

# Procurando se algum município tem população menor que 100 mil habitantes
municipios$populacao < 100000

# Procurando se algum município pertence ao estado de Pernambuco
municipios$uf == "PE"

# Procurando se algum município tem 300 anos ou mais
municipios$idade >= 300

# Procurando se algum município pertence ao estado de pernambuco e tem mais que
# 1 milhão de habitantes
municipios$uf == "PE" & municipios$populacao > 1000000

## Estruturas de controle

# Criando uma estrutura condicional que cria uma dummy retornando valor 1
# toda vez que o município pertencer ao estado de Pernambuco
municipios$DummyPE <- ifelse(municipios$uf == "PE", 1, 0)

# Criando uma estrutura de repetição
par(mfrow = c(2,2))
lapply(gapminder[, 4:6], hist)

## Funções 

# Função básica
f <- function(nro) {
  if(nro < 45) {
    for(i in 1:nro) {
      cat("Hello, world!\n")
    }
  } else {
    cat("E tome hello, world!!!")
  }
}
f(20)
f(100)

# Criando função que retorna o desvio padrão de uma variável

desvio <- function(d) {d <- sd(d)
return(d)}

desvio(gapminder$lifeExp)

summary(gapminder)

## Funções vetorizadas com o banco gapminder
sapply(municipios[,-1:-2], mean) 

mapply(hist, gapminder[ , 4:6], MoreArgs=list(main='Histograma',
                                              xlab = 'Valores',
                                              ylab = 'Frequência'))

