# Gabarito - Desafio 02: Testes estatísticos com base simulada

# Objetivo: Realizar dois testes estatísticos simples com uma base simulada:
# 1. Teste qui-quadrado para associação entre sexo e faixa etária
# 2. Teste t para comparação da idade média entre sexos

# =============================================================
# 1. Carregar pacotes necessários
# =============================================================

library(tidyverse)
library(janitor)

# =============================================================
# 2. Gerar base simulada com 1000 observações
# =============================================================

set.seed(123)

base_simulada <- tibble(
  sexo = sample(c("Masculino", "Feminino"), size = 1000, replace = TRUE),
  idade = round(rnorm(1000, mean = 40, sd = 15))
) %>% 
  filter(idade >= 0 & idade <= 90) %>% 
  mutate(
    faixa_etaria = case_when(
      idade < 10 ~ "0-9",
      idade < 20 ~ "10-19",
      idade < 30 ~ "20-29",
      idade < 40 ~ "30-39",
      idade < 50 ~ "40-49",
      idade < 60 ~ "50-59",
      idade < 70 ~ "60-69",
      idade < 80 ~ "70-79",
      TRUE ~ "80+"
    )
  )

# =============================================================
# 3. Teste Qui-quadrado: sexo vs faixa_etaria
# =============================================================

tabela_contingencia <- table(base_simulada$sexo, base_simulada$faixa_etaria)
chisq.test(tabela_contingencia)

chisq.test(table(base_simulada$sexo, base_simulada$faixa_etaria))
# =============================================================
# 4. Teste t: idade média entre sexos
# =============================================================

t.test(idade ~ sexo, data = base_simulada)