## Script da aula 6 ##

## Small e medium data

library(data.table)

# Criando um banco de dados
casos= 9e6 

# Criando o data.frame com o total de casos definido acima
largeData1 = data.table(a=rpois(casos, 3),
                        b=rbinom(casos, 1, 0.7),
                        c=rnorm(casos),
                        d=sample(c("fogo","agua","terra","ar"), casos, replace=TRUE),
                        e=rnorm(casos),
                        f=rpois(casos, 3))

# Retornando o tamanho do objeto
object.size(largeData1)

# Mostrando as primeiras linhas
head(largeData1)

# Salvando na pasta
write.table(largeData1,"bases_originais/largeData1.csv",sep=",",row.names=FALSE,quote=FALSE) 

# Colocando o endereço
enderecoBase <- 'bases_originais/largeData1.csv'

# Extração direta via read.csv
system.time(extracaoLD1 <- read.csv2(enderecoBase))

# Extração via amostragem com read.csv
# Ler as primeiras 50 linhas
amostraLD1 <- read.csv2(enderecoBase, nrows=50)  

amostraLD1Classes <- sapply(amostraLD1, class) # encontra a classe da amostra amostra

# Fazendo a leitura passando as classes de antemão, a partir da amostra
system.time(extracaoLD2 <- data.frame(read.csv2("bases_originais/largeData1.csv", colClasses=amostraLD1Classes) ) ) 

## Large data

# O objeto já foi criado anteriormente
# Não criei um banco novo, e expliquei via email a razão (computacional) disso

# Criação de uma amostra
extracaoLD3 <- largeData1[sample(nrow(largeData1), 10000) , ]

# Realizando operações
mean(extracaoLD3[,5]) # Não roda por acusar NA's
sd(extracaoLD3[,6])

# Criando um modelo linear
lm(e ~ ., largeData1) # o intercepto foi positivo, e todas as outras variáveis negativas, com exceção da "f"
