# Aula 06 - Campos faltantes | Outliers | Normalização de taxas

# Objetivo: Trabalhar com dados faltantes, detectar e tratar outliers,
# e realizar normalização de taxas para análises em epidemiologia.

# =============================================================
# 1. Carregando pacotes
# =============================================================

library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(scales)
library(stringr)
library(forcats)
library(knitr)
library(PHEindicatormethods)
library(purrr)

# =============================================================
# 2. Simulando base epidemiológica com estrutura semelhante ao SIH
# =============================================================

set.seed(123)
n <- 1000

id_aih <- sprintf("ID%010d", 1:n)

base <- data.frame(
  id_aih = id_aih,
  sexo = sample(c("Feminino", "Masculino"), n, replace = TRUE),
  idade = sample(0:100, n, replace = TRUE),
  nasc = sample(seq(as.Date("1920-01-01"), as.Date("2020-12-31"), by="day"), n, replace = TRUE),
  dt_inter = sample(seq(as.Date("2023-01-01"), as.Date("2023-06-30"), by="day"), n, replace = TRUE),
  qt_diarias = sample(1:30, n, replace = TRUE),
  val_sp = round(rnorm(n, mean = 400, sd = 150), 2),
  val_tot = round(rnorm(n, mean = 1200, sd = 400), 2),
  cid_principal = sample(c("A90", "A900", "J12", "B33"), n, replace = TRUE),
  uf = sample(c("SP", "RJ", "MG", "RS"), n, replace = TRUE),
  pop_mun = sample(c(50000, 150000, 500000, 1200000), n, replace = TRUE)
)

base$dt_saida <- base$dt_inter + base$qt_diarias

# Introduzindo campos faltantes
base$val_sp[sample(1:n, 30)] <- NA
base$qt_diarias[sample(1:n, 20)] <- NA

# =============================================================
# 3. Análise de campos faltantes com visualização
# =============================================================

percent_missing <- sapply(base, function(x) mean(is.na(x)))
missing_summary <- data.frame(
  Variavel = names(percent_missing),
  Preenchido = round(100 * (1 - percent_missing), 1),
  Faltante = round(100 * percent_missing, 1)
)

missing_long <- missing_summary %>%
  pivot_longer(cols = c("Preenchido", "Faltante"), names_to = "Status", values_to = "Percentual")

ggplot(missing_long, aes(x = reorder(Variavel, Percentual), y = Percentual, fill = Status)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Preenchido" = "gray70", "Faltante" = "red")) +
  coord_flip() +
  labs(title = "Proporção de preenchimento e ausência por variável",
       x = "Variável", y = "Percentual") +
  theme_minimal()

base_limpa <- base %>% filter(!(is.na(val_sp) & is.na(qt_diarias)))

# =============================================================
# 4. Outliers - Boxplots e padronização
# =============================================================

zscore <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

z_data <- data.frame(z = rnorm(10000, mean = 0, sd = 1))

ggplot(z_data, aes(x = z)) +
  geom_histogram(aes(y = ..density..), bins = 50, fill = "lightblue", color = "black") +
  geom_density(color = "blue", size = 1.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkgreen", size = 1) +
  geom_vline(xintercept = 1, linetype = "dotted", color = "red", size = 1) +
  geom_vline(xintercept = -1, linetype = "dotted", color = "red", size = 1) +
  labs(title = "Distribuição Normal Padrão (Z-score)",
       x = "Z-score", y = "Densidade") +
  theme_minimal()

base <- base %>%
  mutate(
    z_val_sp = zscore(val_sp),
    z_val_tot = zscore(val_tot),
    z_qt_diarias = zscore(qt_diarias)
  )

# Boxplots
base %>%
  pivot_longer(cols = c(val_sp, val_tot, qt_diarias), names_to = "variavel", values_to = "valor") %>%
  ggplot(aes(x = variavel, y = valor)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Boxplots das variáveis contínuas") +
  theme_minimal()

# Remoção de outliers extremos
base_filtrada <- base %>%
  filter(abs(z_val_sp) <= 3, abs(z_val_tot) <= 3, abs(z_qt_diarias) <= 3)

# =============================================================
# 5. Normalização de taxas - usando phe_dsr()

# =============================================================
# A normalização de taxas é uma etapa essencial na análise epidemiológica.
# Ela permite comparar taxas de eventos (como óbitos ou hospitalizações) entre grupos
# com diferentes estruturas populacionais (por exemplo, países com diferentes
# distribuições etárias).

# Por exemplo, um país com mais idosos terá naturalmente uma taxa bruta de mortalidade maior,
# mesmo que o risco individual não seja maior. A normalização remove esse viés.

# Existem dois tipos principais de normalização:
# - Direta: utiliza as taxas específicas por estrato e aplica a uma população padrão.
# - Indireta: utiliza as proporções da população local para aplicar taxas padrão (ex: SMR).

# Neste exemplo, usamos a abordagem direta com o pacote PHEindicatormethods,
# utilizando a função `phe_dsr()` que calcula taxas normalizadas por idade e sexo,
# com base em uma população padrão fornecida.
# =============================================================

# Simulação de dados populacionais e óbitos por idade e sexo
faixas_idade <- c("0-4", "5-9", "10-14", "15-19", "20-24")
populacao <- expand.grid(Country = c("A", "B"), age_cat5 = faixas_idade, Sex = c("Male", "Female")) %>%
  mutate(Population = sample(50000:200000, n(), replace = TRUE))

obitos <- populacao %>%
  mutate(Deaths = round(Population * runif(n(), min = 0.001, max = 0.01)))

standard_pop <- expand.grid(age_cat5 = faixas_idade, Sex = c("Male", "Female")) %>%
  mutate(pop = sample(9000:12000, n(), replace = TRUE))

all_data <- obitos %>%
  left_join(standard_pop, by = c("age_cat5", "Sex")) %>%
  mutate(
    Country = fct_relevel(Country, "A", "B"),
    Sex = fct_relevel(Sex, "Male", "Female"),
    age_cat5 = fct_relevel(age_cat5, faixas_idade)
  )

mortality_ds_rate <- all_data %>%
  group_by(Country) %>%
  phe_dsr(
    x = Deaths,
    n = Population,
    stdpop = pop,
    stdpoptype = "field"
  )

kable(mortality_ds_rate)

# Aula concluída
