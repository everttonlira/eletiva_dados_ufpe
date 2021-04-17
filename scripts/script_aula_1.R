# Criando um objeto simples
origem <- c(1, 2, 3)

nivel <- c(1:10)

# Criando um objeto complexo: regressão em painel
d2 <- plm(pobreza ~ niveldemoc + tempodemoc + nepl + particip + popcresc + pibcresc + mort + educ + origem, 
          data = b3, model="random")
summary(d2)

# Verificando a complexidade
str(origem)
str(nivel)
str(d2)
