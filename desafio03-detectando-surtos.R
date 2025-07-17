# DESAFIO 03 – DETECÇÃO DE SURTOS

# Objetivo: A partir de uma base simulada com dados semanais de um município,
# vamos identificar possíveis surtos por meio de:
# - Cálculo da média móvel de casos
# - Estimativa simplificada do número efetivo de reprodução (Rt)
# - Visualização dos resultados com gráficos
#
# Este desafio se baseia nos conteúdos abordados nas aulas 10 (séries temporais)
# e 11 (modelagem de epidemias).

# =============================================================
# 1. Carregando pacotes necessários
# =============================================================

library(tidyverse)     # Manipulação de dados e visualizações
library(lubridate)     # Manipulação de datas
library(zoo)           # Cálculo de médias móveis

# =============================================================
# 2. Simulando base semanal de casos
# =============================================================

set.seed(123)
data_semanal <- seq.Date(from = as.Date("2023-01-01"), to = as.Date("2023-06-30"), by = "week")
n <- length(data_semanal)

# Criando padrão com tendência + ruído aleatório + surtos
casos <- 15 +
  0.3 * seq_len(n) +                         # tendência crescente
  5 * sin(2 * pi * seq_len(n)/12) +         # variação cíclica
  rnorm(n, mean = 0, sd = 2)                # ruído

# Adicionando surtos artificiais
casos[c(10, 15)] <- casos[c(10, 15)] + 20

infectados <- round(casos * runif(n, 0.5, 0.8))  # proporção de infectados

serie <- tibble(
  semana = data_semanal,
  casos = round(casos),
  infectados = infectados
)

# Visualização inicial da série
serie %>%
  ggplot(aes(x = semana, y = casos)) +
  geom_line(color = "darkblue") +
  labs(title = "Casos semanais simulados", x = "Semana", y = "Número de casos") +
  theme_minimal()

# =============================================================
# 3. Calcule a média móvel de 3 semanas
# =============================================================

# Use a função rollmean para calcular a média móvel dos casos
# Crie uma nova coluna chamada "media_movel" dentro do objeto "serie"

# Em seguida, crie um gráfico com:
# - Linha cinza para os casos
# - Linha vermelha para a média móvel de 3 semanas

# =============================================================
# 4. Estime o Rt semanal de forma simplificada
# =============================================================

serie <- serie %>%
  mutate(
    casos_passado = lag(casos),
    rt_estimado = casos / casos_passado
  )

# Gráfico do Rt com linha azul e linha vermelha pontilhada em y = 1
serie %>%
  ggplot(aes(x = semana, y = rt_estimado)) +
  geom_line(color = "blue") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(title = "Estimativa simplificada do Rt",
       x = "Semana", y = "Rt estimado") +
  theme_minimal()

# =============================================================
# 5. Conclusão
# =============================================================

# Interprete os gráficos:
# - Quando há indícios de surtos?
# - Quando o Rt ficou acima de 1?
# - A média móvel ajuda a entender melhor os padrões?
