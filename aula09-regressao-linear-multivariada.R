# =============================================================
# Aula 09 - Regressão Linear Múltipla
# Objetivo: Aplicar regressão linear múltipla a um problema epidemiológico realista
# envolvendo doenças infecciosas e parasitárias, com base em dados agregados por município.
# Serão abordadas as etapas de simulação de dados, tratamento de missing values e outliers,
# análise de normalidade, visualização, modelagem, diagnóstico, multicolinearidade e
# seleção de preditoras.
#
# PROBLEMA EPIDEMIOLÓGICO:
# Estuda-se a relação entre fatores ambientais e estruturais e a taxa de esquistossomose
# em diferentes municípios ribeirinhos de uma região endêmica. A unidade de análise é o município.
# A variável resposta é a taxa de esquistossomose por 100 mil habitantes.
# =============================================================

# =============================================================
# 1. Carregando pacotes necessários
# =============================================================

library(tidyverse)
library(naniar)
library(nortest)
library(GGally)
library(car)

# =============================================================
# 2. Simulando base com missing values e outliers
# =============================================================

set.seed(123)
n <- 1000
municipio <- paste("Município", 1:n)

porcentagem_exposicao_agua <- runif(n, 0, 1)
cobertura_saneamento <- runif(n, 0, 1)
indice_pobreza <- rnorm(n, 0.5, 0.2)
numero_prof_saude <- rpois(n, 5)
populacao_total <- round(runif(n, 500, 10000))

taxa_esquistossomose <- 300 * porcentagem_exposicao_agua -
  200 * cobertura_saneamento +
  150 * indice_pobreza -
  5 * numero_prof_saude +
  rnorm(n, 0, 50)

base <- tibble(
  municipio,
  porcentagem_exposicao_agua,
  cobertura_saneamento,
  indice_pobreza,
  numero_prof_saude,
  populacao_total,
  taxa_esquistossomose
)

set.seed(456)
base[sample(1:n, 30), "indice_pobreza"] <- NA
base[sample(1:n, 30), "numero_prof_saude"] <- NA

set.seed(789)
outlier_idx <- sample(which(!is.na(base$taxa_esquistossomose)), 10)
base$taxa_esquistossomose[outlier_idx] <- base$taxa_esquistossomose[outlier_idx] + rnorm(10, 800, 100)

# =============================================================
# 3. Tratamento de missing values e outliers
# =============================================================

gg_miss_var(base)
base_clean <- base %>% drop_na()

Q1 <- quantile(base_clean$taxa_esquistossomose, 0.25)
Q3 <- quantile(base_clean$taxa_esquistossomose, 0.75)
IQR_val <- Q3 - Q1
base_clean <- base_clean %>%
  filter(taxa_esquistossomose >= (Q1 - 1.5 * IQR_val),
         taxa_esquistossomose <= (Q3 + 1.5 * IQR_val))

# =============================================================
# 4. Análise de normalidade
# =============================================================

base_clean %>%
  ggplot(aes(x = taxa_esquistossomose)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
  geom_density(color = "darkblue", size = 1) +
  geom_vline(aes(xintercept = mean(taxa_esquistossomose)), linetype = "dashed", color = "red") +
  geom_vline(aes(xintercept = mean(taxa_esquistossomose) + sd(taxa_esquistossomose)), linetype = "dotted", color = "purple") +
  geom_vline(aes(xintercept = mean(taxa_esquistossomose) - sd(taxa_esquistossomose)), linetype = "dotted", color = "purple") +
  labs(title = "Distribuição da taxa de esquistossomose", x = "Taxa por 100 mil hab.", y = "Densidade")

ad.test(base_clean$taxa_esquistossomose)

# =============================================================
# 5. Análise exploratória
# =============================================================

vars <- c("porcentagem_exposicao_agua", "cobertura_saneamento", "indice_pobreza", "numero_prof_saude")
for (v in vars) {
  p <- ggplot(base_clean, aes_string(x = v, y = "taxa_esquistossomose")) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    labs(title = paste("Taxa vs", v)) +
    theme_minimal()
  print(p)
}

# =============================================================
# 6. Avaliação de multicolinearidade
# =============================================================

modelo_temp <- lm(taxa_esquistossomose ~ porcentagem_exposicao_agua + cobertura_saneamento + indice_pobreza + numero_prof_saude, data = base_clean)
car::vif(modelo_temp)

# =============================================================
# 7. Regressão Linear Múltipla
# =============================================================

modelo_multi <- modelo_temp
summary(modelo_multi)
coef(modelo_multi)

# =============================================================
# 8. Avaliação do modelo
# =============================================================

par(mfrow = c(2, 2))
plot(modelo_multi)
par(mfrow = c(1, 1))

# =============================================================
# 9. Considerações finais
# =============================================================

# Esta modelagem permite avaliar a associação entre características estruturais e ambientais
# de municípios e a carga de esquistossomose. A análise exploratória mostrou relações claras,
# e os testes de multicolinearidade indicaram baixa redundância entre preditoras.
# Pressupostos foram avaliados por gráficos de resíduos.
# As variáveis foram mantidas no modelo com base em sua significância e ausência de colinearidade.
