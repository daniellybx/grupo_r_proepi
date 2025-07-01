# Aula 12 - Rastreamento de Contatos e Análises de Questionários

# Objetivo: Nesta aula, vamos aprender a:
# - Criar e analisar redes de transmissão com dados simulados
# - Utilizar o pacote `igraph` diretamente para explorar essas redes
# - Simular e analisar dados de um questionário (survey)
# - Realizar análises descritivas e explorar associações simples com base nas respostas
# Esta aula é baseada nos capítulos correspondentes do "Manual de R para Epidemiologistas".

# =============================================================
# 1. Carregando pacotes necessários
# =============================================================

library(tidyverse)     # Manipulação de dados
library(lubridate)     # Manipulação de datas
library(igraph)        # Análise e visualização de redes
library(survey)        # Análises complexas de surveys

# =============================================================
# 2. Simulando base de contatos entre indivíduos
# =============================================================

set.seed(123)
n <- 200  # número de indivíduos

# Simulando identificadores únicos
id <- paste0("c", seq_len(n))

# Simulando datas de sintomas (aleatórias entre jan e fev/2023)
data_sintomas <- sample(seq(as.Date("2023-01-01"), as.Date("2023-02-28"), by = "day"), n, replace = TRUE)

# Simulando variáveis demográficas básicas
idade <- round(runif(n, 0, 90))
sexo <- sample(c("Masculino", "Feminino"), n, replace = TRUE)
bairro <- sample(c("Centro", "Zona Norte", "Zona Sul", "Zona Leste", "Zona Oeste"), n, replace = TRUE)

# Simulando quem infectou quem (com base em ID)
infector <- sample(c(NA, id), n, replace = TRUE)

# Criando a linelist com os dados dos indivíduos
linelist <- tibble(
  id = id,
  infector = infector,
  data_sintomas = data_sintomas,
  idade = idade,
  sexo = sexo,
  bairro = bairro
)

# Criando base de contatos (arestas do grafo)
contacts <- linelist %>%
  filter(!is.na(infector)) %>%
  select(from = infector, to = id)

# Criando grafo com igraph
grafo <- graph_from_data_frame(d = contacts, vertices = linelist, directed = TRUE)

# =============================================================
# 3. Explorando a rede de contatos
# =============================================================

# Plotando a rede de contatos
plot(grafo, vertex.size = 5, vertex.label = NA, main = "Rede de Transmissão Simulada")

# Identificando indivíduos com maior grau de saída (mais transmissores)
degree_tab <- igraph::degree(grafo, mode = "out")
top_infectores <- sort(degree_tab, decreasing = TRUE)[1:5]
top_infectores

# Visualizando sub-rede de um indivíduo específico
c50_subgrafo <- induced_subgraph(grafo, vids = unlist(ego(grafo, order = 1, nodes = "c50", mode = "out")))
plot(c50_subgrafo, vertex.size = 5, vertex.label = NA, main = "Sub-rede a partir do caso c50")

# =============================================================
# 4. Simulando dados de questionário (survey)
# =============================================================

set.seed(456)
n_survey <- 100
id_survey <- sample(id, n_survey)

# Variáveis do questionário
uso_mascara <- sample(c("Sempre", "Às vezes", "Nunca"), n_survey, replace = TRUE, prob = c(0.6, 0.3, 0.1))
contato_animais <- sample(c("Sim", "Não"), n_survey, replace = TRUE, prob = c(0.2, 0.8))
viajou <- sample(c("Sim", "Não"), n_survey, replace = TRUE, prob = c(0.3, 0.7))
vacinado <- sample(c("Sim", "Não"), n_survey, replace = TRUE, prob = c(0.8, 0.2))

# Criando o data frame
dados_survey <- tibble(
  id = id_survey,
  uso_mascara = uso_mascara,
  contato_animais = contato_animais,
  viajou = viajou,
  vacinado = vacinado
)

# =============================================================
# 5. Análises descritivas do questionário
# =============================================================

dados_survey %>%
  count(uso_mascara) %>%
  mutate(percentual = round(100 * n / sum(n), 1))

dados_survey %>%
  count(viajou) %>%
  mutate(percentual = round(100 * n / sum(n), 1))

dados_survey %>%
  count(vacinado, uso_mascara) %>%
  pivot_wider(names_from = uso_mascara, values_from = n, values_fill = 0)

# =============================================================
# 6. Unindo a base do survey com a linelist
# =============================================================

survey_completo <- linelist %>%
  left_join(dados_survey, by = "id")

survey_completo %>%
  filter(!is.na(vacinado)) %>%
  count(bairro, vacinado) %>%
  group_by(vacinado) %>%
  mutate(prop = round(100 * n / sum(n), 1))

# =============================================================
# 7. Conclusão
# =============================================================

# Com esta aula, aprendemos a:
# - Criar redes de transmissão simuladas com `igraph`
# - Explorar sub-redes e identificar supertransmissores
# - Simular dados de survey com variáveis comuns
# - Realizar análises descritivas simples de questionários
# - Integrar respostas de surveys com dados epidemiológicos
# Essa abordagem é útil para apoiar decisões durante surtos e monitorar comportamentos associados à transmissão.
