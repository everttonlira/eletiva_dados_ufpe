# Carregando pacotes
library(glue)
library(cowplot)
library(magrittr)
library(plotly)
library(tidyverse)
library(widyr)
library(hms)
library(lubridate) 
library(tidytext)
library(tm)
library(wordcloud)
library(igraph)
library(networkD3)
library(rtweet)

# Retirando tweets da timeline de Jair Bolsonaro
tweets <- get_timeline("jairbolsonaro", n = 100, include_rts = FALSE)

# Passando para data.frame
tweets <- as.data.frame(tweets)
class(tweets)

# Normalizando os textos dos tweets
tweets <- tweets %>% 
  # Convert to lowercase. 
  mutate(Text = text %>% str_to_lower) %>% 
  # Remove unwanted characters. 
  mutate(Text= text %>% str_remove_all(pattern = '\\n')) %>% 
  mutate(Text = text %>% str_remove_all(pattern = '&amp')) %>% 
  mutate(Text = text %>% str_remove_all(pattern = 'https://t.co/[a-z,A-Z,0-9]*')) %>% 
  mutate(Text = text %>% str_remove_all(pattern = 'http://t.co/[a-z,A-Z,0-9]*')) %>% 
  mutate(Text = text %>% str_remove_all(pattern = 'https')) %>% 
  mutate(Text = text %>% str_remove_all(pattern = 'http')) %>% 
  # Remove hashtags.
  mutate(Text = text %>% str_remove_all(pattern = '#[a-z,A-Z]*')) %>% 
  # Remove accounts.
  mutate(Text = text %>% str_remove_all(pattern = '@[a-z,A-Z]*')) %>% 
  # Remove retweets.
  mutate(Text = text %>% str_remove_all(pattern = 'rt [a-z,A-Z]*: ')) %>% 
  mutate(Text = text %>% str_remove(pattern = '^(rt)')) %>% 
  mutate(Text = text %>% str_remove_all(pattern = '\\_')) 

# Replace accents. 
replacement.list <- list('á' = 'a', 'ã' = 'a', 'é' = 'e', 'í' = 'i', 'ó' = 'o', 'õ' = 'o' , 'ú' = 'u')

tweets %<>% 
  mutate(Text = chartr(old = names(replacement.list) %>% str_c(collapse = ''), 
                       new = replacement.list %>% str_c(collapse = ''),
                       x = text))

# Criando um corpus
corpus <- Corpus(x = VectorSource(x = tweets$Text))

tweets.text <- corpus %>% 
  tm_map(removePunctuation) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removeWords, stopwords('pt')) %>% 
  tm_map(PlainTextDocument) # %>% 
# We could also use stemming by uncommenting the folowing line. 

# Recover data into original tibble.
tweets %<>% mutate(Text = tweets.text[[1]]$content)

# Criando contagem de palavras
# Removendo stop words excessivas
stopwords.df <- tibble(
  word = c(stopwords(kind = 'pt'), 
           # We have some tweets in english.
           stopwords(kind = 'en')))

words.df <- tweets %>% 
  unnest_tokens(input = Text, output = word) %>% 
  anti_join(y = stopwords.df, by = 'word')

word.count <- words.df %>% count(word, sort = TRUE)

word.count %>% head(10)

# Visualizando contagem de palavras num barplot
plt <- word.count %>% 
  # Set count threshold. 
  filter(n > 10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  theme_light() + 
  geom_col(fill = 'black', alpha = 0.8) +
  xlab(NULL) +
  coord_flip() +
  ggtitle(label = 'Top Word Count')

plt %>% ggplotly()

# Criando wordcloud
wordcloud(
  words = word.count$word, 
  freq = word.count$n, 
  min.freq = 5, 
  colors = brewer.pal(8, 'Dark2')
)

# Definindo a rede

# Criando um bigram (palavras que aparecem juntas)
bi.gram.words <- tweets %>% 
  unnest_tokens(
    input = Text, 
    output = bigram, 
    token = 'ngrams', 
    n = 2
  ) %>% 
  filter(! is.na(bigram))

bi.gram.words %>% 
  select(bigram) %>% 
  head(20)

# Filtrando por stopwords
bi.gram.words %<>% 
  separate(col = bigram, into = c('word1', 'word2'), sep = ' ') %>% 
  filter(! word1 %in% stopwords.df$word) %>% 
  filter(! word2 %in% stopwords.df$word) %>% 
  filter(! is.na(word1)) %>% 
  filter(! is.na(word2)) 

# Agrupa e conta
bi.gram.count <- bi.gram.words %>% 
  count(word1, word2, sort = TRUE) %>% 
  # We rename the weight column so that the 
  # associated network gets the weights (see below).
  rename(weight = n)

bi.gram.count %>% head()

# Distribuição de pesos
bi.gram.count %>% 
  ggplot(mapping = aes(x = weight)) +
  theme_light() +
  geom_histogram() +
  labs(title = "Bigram Weight Distribution")

# Colocando em log
bi.gram.count %>% 
  mutate(weight = log(weight + 1)) %>% 
  ggplot(mapping = aes(x = weight)) +
  theme_light() +
  geom_histogram() +
  labs(title = "Bigram log-Weight Distribution")


# Criando o gráfico
threshold <- 2

# For visualization purposes we scale by a global factor. 
ScaleWeight <- function(x, lambda) {
  x / lambda
}

network <-  bi.gram.count %>%
  filter(weight > threshold) %>%
  mutate(weight = ScaleWeight(x = weight, lambda = 2E3)) %>% 
  graph_from_data_frame(directed = FALSE)

network

# Visualizando bigram
plot(
  network, 
  vertex.size = 1,
  vertex.label.color = 'black', 
  vertex.label.cex = 0.7, 
  vertex.label.dist = 1,
  edge.color = 'gray', 
  main = 'Bigram Count Network', 
  sub = glue('Weight Threshold: {threshold}'), 
  alpha = 50
)

# Visualizar a partir da quantidade de vezes que os nodos aparecem
# Store the degree.
V(network)$degree <- strength(graph = network)

# Compute the weight shares.
E(network)$width <- E(network)$weight/max(E(network)$weight)

plot(
  network, 
  vertex.color = 'lightblue',
  # Scale node size by degree.
  vertex.size = 2*V(network)$degree,
  vertex.label.color = 'black', 
  vertex.label.cex = 0.6, 
  vertex.label.dist = 1.6,
  edge.color = 'gray', 
  # Set edge width proportional to the weight relative value.
  edge.width = 3*E(network)$width ,
  main = 'Bigram Count Network', 
  sub = glue('Weight Threshold: {threshold}'), 
  alpha = 50
)

# Plotando um gráfico mais interativo
network <-  bi.gram.count %>%
  filter(weight > threshold) %>%
  graph_from_data_frame(directed = FALSE)

# Store the degree.
V(network)$degree <- strength(graph = network)
# Compute the weight shares.
E(network)$width <- E(network)$weight/max(E(network)$weight)

# Create networkD3 object.
network.D3 <- igraph_to_networkD3(g = network)
# Define node size.
network.D3$nodes %<>% mutate(Degree = (1E-2)*V(network)$degree)
# Degine color group (I will explore this feature later).
network.D3$nodes %<>% mutate(Group = 1)
# Define edges width. 
network.D3$links$Width <- 10*E(network)$width

forceNetwork(
  Links = network.D3$links, 
  Nodes = network.D3$nodes, 
  Source = 'source', 
  Target = 'target',
  NodeID = 'name',
  Group = 'Group', 
  opacity = 0.9,
  Value = 'Width',
  Nodesize = 'Degree', 
  # We input a JavaScript function.
  linkWidth = JS("function(d) { return Math.sqrt(d.value); }"), 
  fontSize = 12,
  zoom = TRUE, 
  opacityNoHover = 1
)

