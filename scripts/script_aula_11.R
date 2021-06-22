## Script aula 11 ##

## Introdução a datas e tempos

# Criando objeto com datas
aniversario1 <- c("1995-09-26 18:05", "1979-02-23 09:00", "2016-02-05 10:00")

# Convertendo para as.Date
aniversario2 <- as.Date(c("1995-09-26 18:05", "1979-02-23 09:00", "2016-02-05 10:00"))
unclass(aniversario2)

# Convertendo para POSIXct
aniversario3 <- as.POSIXct(c("1995-09-26 18:05", "1979-02-23 09:00", "2016-02-05 10:00"))
unclass(aniversario3)

# Conversão para POSIXlt
aniversario4 <- as.POSIXlt(c("1995-09-26 18:05", "1979-02-23 09:00", "2016-02-05 10:00"))
unclass(aniversario4)

# Extraindo componentes
# Carregando pacote
library(lubridate)

# Mês
month(aniversario4)

# Mês pelo nome
month(aniversario4, label = T)

# Dia da semana
wday(aniversario4, label = T, abbr = T) 

# Operações
# Período
aniversario4 + minutes(45) 

# Duração
aniversario4 + dminutes(-60) 

## Datas na prática

# Transformando a URL da base em objeto
url <- 'https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-states.csv' 

# Baixando a base de COVID
covid_br <- read.csv2(url, encoding='latin1', sep = ',') 

# Filtrando por Pernambuco
covid_pe <- subset(covid_br, state == 'PE') 

# Observando a classe
str(covid_pe) 

# Transformando a coluna data para as.Date
covid_pe$date <- as.Date(covid_pe$date, format = "%Y-%m-%d") 

# Observando a classe após a transformação
str(covid_pe) # observar a mudança na classe

# Criando sequencial de dias para a predição
covid_pe$dia <- seq(1:length(covid_pe$date))

# Cria vetor para predição
pred_dia = data.frame(dia = covid_pe$dia)

# Cria segundo vetor
pred_seq = data.frame(dia = seq(max(covid_pe$dia)+1, max(covid_pe$dia)+180))

# Juntando os dois
pred_dia <- rbind(pred_dia, pred_seq)

# Instalando e carregando pacote para predição
install.packages("drc")
library(drc) 

# Fazendo a previsão para o total de mortes
fit_ll <- drm(deaths ~ dia, fct = LL2.5(),
             data = covid_pe, robust = 'mean') 

# Observa o ajuste
plot(fit_ll, log="", main = "Log logistic") 

# Prevendo para frente
pred_ll <- data.frame(predicao = ceiling(predict(fit_ll, pred_dia))) 

# Cria uma sequência para corresponder aos dias
pred_ll$data <- seq.Date(as.Date('2020-03-12'), by = 'day', length.out = length(pred_dia$dia))

# Juntando as informações na base original
pred_ll <- merge(pred_ll, covid_pe, by.x ='data', by.y = 'date', all.x = T) 

# Carregando pacote e plotando resultados
library(plotly) 

plot_ly(pred_ll) %>% add_trace(x = ~data, y = ~predicao, type = 'scatter', mode = 'lines', name = "Mortes - Predição") %>% add_trace(x = ~data, y = ~deaths, name = "Mortes - Observados", mode = 'lines') %>% layout(
  title = 'Predição de mortes por COVID 19 em Pernambuco', 
  xaxis = list(title = 'Data', showgrid = FALSE), 
  yaxis = list(title = 'Mortes acumuladas por dia', showgrid = FALSE),
  hovermode = "compare")

# Carregando pacotes
library(zoo) # biblioteca para manipulação de datas e séries temporais

# Média móvel de mortes em 7 dias
covid_pe <- covid_pe %>% mutate(newDeathsMM7 = round(rollmean(x = newDeaths, 7, align = "right", fill = NA), 2))

# Valor defasado de mortes em 7 dias
covid_pe <- covid_pe %>% mutate(newDeathsL7 = dplyr::lag(newDeaths, 7)) 

# Plotando para comparação
plot_ly(covid_pe) %>% add_trace(x = ~date, y = ~newDeaths, type = 'scatter', mode = 'lines', name = "Novas mortes") %>% add_trace(x = ~date, y = ~newDeathsMM7, name = "Novas mortes (média móvel em 7 dias)", mode = 'lines') %>% layout(
  title = 'Novos mortes por COVID19 em Pernambuco', 
  xaxis = list(title = 'Data', showgrid = FALSE), 
  yaxis = list(title = 'Novos mortes por dia', showgrid = FALSE),
  hovermode = "compare") 