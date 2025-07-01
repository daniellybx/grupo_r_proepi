# Aula 07 - Tabelas descritivas | Testes estatísticos simples

# Objetivo: Nesta aula, vamos aprender a construir tabelas de frequência,
# tabelas de contingência cruzando múltiplas variáveis, calcular medidas
# como Risco Relativo e Odds Ratio a partir de tabelas 2x2 e aplicar testes
# estatísticos simples como Qui-quadrado e Teste Exato de Fisher.

# =============================================================
# 1. Carregando pacotes necessários
# =============================================================

library(dplyr)
library(ggplot2)
library(tidyr)
library(epitools)
library(gmodels)
library(janitor)
library(forcats)
library(knitr)

# =============================================================
# 2. Simulando base de dados epidemiológica
# =============================================================

set.seed(123)
n <- 1000

base <- tibble(
  sexo = sample(c("Masculino", "Feminino"), n, replace = TRUE),
  faixa_etaria = sample(c("0-4", "5-9", "10-14", "15-19"), n, replace = TRUE),
  estado = sample(c("SP", "RJ", "MG", "RS"), n, replace = TRUE),
  exposicao = sample(c("Exposto", "Não exposto"), n, replace = TRUE),
  desfecho = sample(c("Óbito", "Recuperado"), n, replace = TRUE)
)

# =============================================================
# 3. Tabelas descritivas
# =============================================================

# 3.1 Tabelas de frequência
base %>%
  count(sexo) %>%
  rename(Frequencia = n)

# 3.2 Tabelas de frequência com percentual
base %>%
  count(faixa_etaria) %>%
  mutate(Percentual = round(100 * n / sum(n), 1)) %>%
  rename(Frequencia = n)

# 3.3 Tabelas de contingência 2x2
cont_2x2 <- table(base$exposicao, base$desfecho)
cont_2x2

# 3.4 Tabela de contingência 3 variáveis
tab3 <- base %>%
  tabyl(exposicao, desfecho, sexo)
tab3

# 3.5 Tabela de contingência 4 variáveis com contagem manual
base %>%
  count(estado, sexo, exposicao, desfecho) %>%
  arrange(desc(n)) %>%
  head(10)

# =============================================================
# 4. Medidas a partir de tabelas 2x2
# =============================================================

# 4.1 Risco Relativo (RR) e Odds Ratio (OR)

# Vamos calcular separadamente o Risco Relativo e o Odds Ratio,
# para mostrar claramente a diferença entre as duas medidas.
# Ambas partem da mesma tabela 2x2, mas representam conceitos diferentes:
# - RR compara probabilidades
# - OR compara chances

# A tabela 2x2 tem o formato:
#                   Desfecho
# Exposição       | Óbito | Recuperado
# -------------------------------------
# Exposto         |   a   |     b
# Não exposto     |   c   |     d
#
# O Risco Relativo (RR) é calculado como:
#   RR = (a / (a + b)) / (c / (c + d))
# Ou seja, a proporção de óbitos nos expostos sobre a proporção de óbitos nos não expostos
#
# O Odds Ratio (OR) é calculado como:
#   OR = (a/b) / (c/d) = (a * d) / (b * c)
# É a razão entre as chances de óbito entre expostos e não expostos.
#
# Vamos calcular ambas medidas com intervalo de confiança de 95%:

# Risco Relativo (Risk Ratio): razão entre as probabilidades de óbito
rr <- riskratio(cont_2x2)
rr

# Odds Ratio: razão entre as chances de óbito
or <- oddsratio(cont_2x2)
or

# =============================================================
# 5. Testes estatísticos simples
# =============================================================

# 5.1 Teste Qui-quadrado
chisq.test(cont_2x2)

# 5.2 Teste Exato de Fisher
fisher.test(cont_2x2)

# 5.3 Quando usar cada um?
# - Use o teste Qui-quadrado quando todas as frequências esperadas forem > 5.
# - Use o teste de Fisher quando alguma célula tiver valor < 5 (mais conservador).

# 5.4 Gráfico da distribuição teórica do qui-quadrado
x <- seq(0, 10, 0.1)
df <- 1
q_chisq <- dchisq(x, df = df)

ggplot(data.frame(x, q_chisq), aes(x, q_chisq)) +
  geom_line(color = "blue") +
  labs(title = "Distribuição Qui-quadrado teórica", x = "Valor", y = "Densidade") +
  theme_minimal()

# 5.5 Gráfico da distribuição das tabelas 2x2 reais
# Visualizando proporção de óbito entre expostos e não expostos
base %>%
  group_by(exposicao, desfecho) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = exposicao, y = prop, fill = desfecho)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Distribuição dos desfechos por exposição",
       y = "Proporção", x = "Exposição") +
  theme_minimal()

# 5.6 Comparação empírica com distribuição Qui-quadrado
# Gerando estatísticas de qui-quadrado a partir de reamostragens simuladas
simulacoes <- replicate(1000, {
  tab <- table(
    sample(base$exposicao),
    sample(base$desfecho)
  )
  chisq.test(tab)$statistic
})

# Histograma dos valores empíricos de Qui-quadrado comparado à curva teórica
sim_df <- data.frame(stat = simulacoes)

ggplot(sim_df, aes(x = stat)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
  stat_function(fun = dchisq, args = list(df = 1), color = "red", size = 1.2) +
  labs(title = "Distribuição empírica vs. teórica do Qui-quadrado",
       x = "Estatística Qui-quadrado", y = "Densidade") +
  theme_minimal()

# Aula concluída!
