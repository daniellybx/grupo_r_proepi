# Aula 13 - Análise de Sobrevida

# Objetivo: Desenvolver uma análise de sobrevida completa a partir de um cenário epidemiológico simulado.
# Abordaremos desde a criação do contexto clínico, simulação da base, análise descritiva, curvas de Kaplan-Meier,
# modelo de Cox ajustado por covariáveis, gráficos comparativos e métricas de avaliação.

# =============================================================
# 1. Criando um contexto epidemiológico realista
# =============================================================

# Imagine que uma cidade brasileira enfrenta um surto de uma nova doença infecciosa respiratória grave (ex: fictício "Vírus X").
# O hospital municipal passou a monitorar pacientes internados com confirmação do Vírus X.
# Para avaliar a gravidade e fatores de risco associados à mortalidade, decidiu-se realizar uma análise de sobrevida,
# com acompanhamento dos pacientes desde a data de internação até o desfecho (óbito ou alta hospitalar).
# Variáveis de interesse incluem idade, sexo e presença de comorbidades.

# =============================================================
# 2. Carregando pacotes necessários
# =============================================================

library(tidyverse)      # Para manipulação e visualização de dados
library(survival)       # Para modelagem de sobrevida (Kaplan-Meier, Cox)
library(survminer)      # Para visualizações específicas da análise de sobrevida

# =============================================================
# 3. Simulando base de dados (n = 1000)
# =============================================================

set.seed(123)  # Garante reprodutibilidade
n <- 1000      # Número de pacientes

# Simulando variáveis demográficas e clínicas
idade <- round(rnorm(n, mean = 60, sd = 15))  # Idade média 60 anos, com desvio padrão de 15 anos
idade[idade < 18] <- 18                       # Menores de 18 não incluídos

sexo <- sample(c("Masculino", "Feminino"), n, replace = TRUE)  # Sexo biológico dos pacientes
comorbidade <- sample(c("Sim", "Não"), n, replace = TRUE, prob = c(0.4, 0.6))  # 40% com comorbidade

# Simulando tempo até o evento (óbito ou censura)
base_taxa <- 0.03  # Taxa base
taxa_idade <- ifelse(idade > 70, 0.06, base_taxa)
taxa_final <- ifelse(comorbidade == "Sim", taxa_idade + 0.03, taxa_idade)
tempo <- rexp(n, rate = taxa_final)
tempo <- round(tempo)
tempo[tempo == 0] <- 1

# Evento: 1 = óbito, 0 = censura (alta ou perda de seguimento)
evento <- rbinom(n, 1, prob = 0.6)

data_inicio <- as.Date("2023-01-01")
data_evento <- data_inicio + tempo

# Data frame final
pacientes <- tibble(
  idade = idade,
  sexo = factor(sexo, levels = c("Feminino", "Masculino")),
  comorbidade = factor(comorbidade, levels = c("Não", "Sim")),
  tempo = tempo,
  evento = evento,
  data_inicio = data_inicio,
  data_evento = data_evento
)

# Criando faixas etárias para análise estratificada
pacientes <- pacientes %>%
  mutate(faixa_etaria = case_when(
    idade < 40 ~ "<40",
    idade < 60 ~ "40-59",
    idade < 75 ~ "60-74",
    TRUE ~ "75+"
  )) %>%
  mutate(faixa_etaria = factor(faixa_etaria, levels = c("<40", "40-59", "60-74", "75+")))

# =============================================================
# 4. Criando objeto de sobrevida
# =============================================================

sobrevida_obj <- Surv(time = pacientes$tempo, event = pacientes$evento)

# =============================================================
# 5. Estimativa de Kaplan-Meier
# =============================================================

km_global <- survfit(sobrevida_obj ~ 1, data = pacientes)

ggsurvplot(km_global, data = pacientes, conf.int = TRUE,
           xlab = "Dias desde internação",
           ylab = "Probabilidade de sobrevivência",
           title = "Curva de Kaplan-Meier - Estimativa Global")

km_sexo <- survfit(sobrevida_obj ~ sexo, data = pacientes)

ggsurvplot(km_sexo, data = pacientes, conf.int = TRUE,
           pval = TRUE, risk.table = TRUE,
           xlab = "Dias desde internação",
           ylab = "Probabilidade de sobrevivência",
           title = "Sobrevida por Sexo")

km_idade <- survfit(sobrevida_obj ~ faixa_etaria, data = pacientes)

ggsurvplot(km_idade, data = pacientes, conf.int = TRUE,
           pval = TRUE, risk.table = TRUE,
           xlab = "Dias desde internação",
           ylab = "Probabilidade de sobrevivência",
           title = "Sobrevida por Faixa Etária")

# =============================================================
# 6. Teste de Log-Rank
# =============================================================

logrank_sexo <- survdiff(sobrevida_obj ~ sexo, data = pacientes)
logrank_idade <- survdiff(sobrevida_obj ~ faixa_etaria, data = pacientes)

# =============================================================
# 7. Modelo de Cox proporcional
# =============================================================

modelo_cox <- coxph(Surv(tempo, evento) ~ idade + sexo + comorbidade, data = pacientes)
summary(modelo_cox)

# =============================================================
# 8. Avaliação do modelo de Cox
# =============================================================

# Índice de concordância (C-index)
concord <- summary(modelo_cox)$concordance
cat("\nÍndice de concordância (C-index):", round(concord[1], 3), "\n")

# =============================================================
# 9. Visualização alternativa dos efeitos do modelo
# =============================================================

# Visualização alternativa com ggplot
exp_coef <- exp(coef(modelo_cox))
conf_int <- exp(confint(modelo_cox))

cox_plot_data <- tibble(
  Variável = names(exp_coef),
  HR = exp_coef,
  HR_inf = conf_int[, 1],
  HR_sup = conf_int[, 2]
)

cox_plot_data %>%
  ggplot(aes(x = Variável, y = HR)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = HR_inf, ymax = HR_sup), width = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(title = "Modelo de Cox - Razões de Risco (HR) com IC 95%",
       y = "Hazard Ratio (HR)", x = "Variável") +
  theme_minimal()

# =============================================================
# 10. Conclusão
# =============================================================

# Conduzimos uma análise completa de sobrevida baseada em cenário epidemiológico simulado de hospitalização por uma doença infecciosa.
# Exploramos curvas de sobrevivência globais e estratificadas, testes de log-rank e modelo de Cox.
# O modelo evidenciou associação entre idade e comorbidade com maior risco de óbito.
# Essa análise é essencial na saúde pública para identificar fatores de risco e otimizar recursos hospitalares.
