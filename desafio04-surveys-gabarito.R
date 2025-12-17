# =============================================================
# Desafio 04 - Análise de Questionários (SURVEY)
# Grupo de Estudos R | Gabarito
# =============================================================

# Objetivo: Analisar uma base simulada de respostas de questionário,
# contendo informações sobre vacinas, uso de máscara, contato com animais e viagens.
# Será usada a base de contatos criada na aula anterior (linelist), unida às respostas do survey.

# =============================================================
# 1. Carregando pacotes
# =============================================================

library(tidyverse)

# =============================================================
# 2. Simulando base de casos
# =============================================================

set.seed(123)
n <- 200
id <- paste0("c", seq_len(n))

linelist <- tibble(
  id = id,
  idade = sample(0:90, n, replace = TRUE),
  sexo = sample(c("Masculino", "Feminino"), n, replace = TRUE),
  bairro = sample(c("Centro", "Zona Norte", "Zona Sul", "Zona Leste", "Zona Oeste"), n, replace = TRUE)
)

# =============================================================
# 3. Simulando respostas do questionário
# =============================================================

set.seed(456)
n_survey <- 100
id_survey <- sample(id, n_survey)

dados_survey <- tibble(
  id = id_survey,
  uso_mascara = sample(c("Sempre", "Às vezes", "Nunca"), n_survey, replace = TRUE, prob = c(0.6, 0.3, 0.1)),
  contato_animais = sample(c("Sim", "Não"), n_survey, replace = TRUE, prob = c(0.2, 0.8)),
  viajou = sample(c("Sim", "Não"), n_survey, replace = TRUE, prob = c(0.3, 0.7)),
  vacinado = sample(c("Sim", "Não"), n_survey, replace = TRUE, prob = c(0.8, 0.2))
)

# =============================================================
# 4. Unindo dados do survey com a linelist
# =============================================================

survey_completo <- linelist %>%
  left_join(dados_survey, by = "id")

# =============================================================
# 5. Proporção de vacinados por bairro
# =============================================================

survey_completo %>%
  filter(!is.na(vacinado)) %>%
  count(bairro, vacinado) %>%
  group_by(bairro) %>%
  mutate(prop = round(100 * n / sum(n), 1))

# =============================================================
# 6. Tabela cruzada entre vacinado e uso de máscara
# =============================================================

survey_completo %>%
  filter(!is.na(vacinado)) %>%
  count(vacinado, uso_mascara) %>%  pivot_wider(names_from = uso_mascara, values_from = n, values_fill = 0)

