## Script do exercício 1 ##

# Criando duas variáveis nominais de desvio padrão diferentes
norm1 <- rnorm(1000, sd = 1)
norm2 <- rnorm(1000, sd = 3.5)

# Criando uma distribuição de Poisson
poisson <- rpois(1000, lambda = 5)

# Criando uma distribuição binomial negativa
binomial_neg <- rnbinom(1000, size = 0.5, prob = 0.2)

# Criando uma distribuição binomial
binomial <- rbinom(1000, size = 1, prob = 0.5)

# Criando uma variável qualitativa binária
dummy <- binomial
dummy[dummy == 1] <- "Partido do Presidente"
dummy[dummy == 0] <- "Partido da coalizão"

# Criando uma variável index
index <- ifelse(poisson < "4", 1, 0)

# Criando um data frame com as variáveis acima
ex1 <- data.frame(norm1 = norm1, norm2 = norm2, poisson = poisson, 
                  binomial_neg = binomial_neg, binomial = binomial,
                  dummy = dummy, index = index)

# Centralizando as variáveis normais
norm1_central <- norm1 - mean(norm1)
norm2_central <- norm2 - mean(norm2)

# Trocando 0 por 1 nas variáveis de contagem
# Poisson
ex1$poisson[ex1$poisson == 0] <- 1

# Binomial negativa
ex1$binomial_neg[ex1$binomial_neg == 0] <- 1

# Binomial 
ex1$binomial[ex1$binomial == 0] <- 1

# Criando um novo data frame com uma amostra de 100 casos
ex2 <- ex1[sample(nrow(ex1), 100), ]