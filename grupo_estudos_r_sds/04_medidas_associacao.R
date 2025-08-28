##############################################################
# AULA 4 - MEDIDAS DE ASSOCIAÇÃO E TESTES ESTATÍSTICOS
##############################################################

# OBJETIVOS DA AULA:
# 1. Calcular medidas epidemiológicas de associação a partir de tabelas 2x2:
#    - Risco Relativo (RR)
#    - Razão de Chances (Odds Ratio - OR)
#    - Risco Atribuível (RA)
#    - Razão de Prevalência (RP)
#
# 2. Demonstrar a importância de trabalhar com objetos do tipo matriz
#    para análises epidemiológicas com tabelas de contingência
#
# 3. Executar testes de hipótese para associação entre variáveis:
#    - Teste do Qui-quadrado (Chi-Square Test)
#    - Teste exato de Fisher (Fisher’s Exact Test)
#
# 4. Executar o teste t de Student para comparação de médias entre duas amostras independentes
#    - Exemplo: comparação da média de quantidade de dias de internação por sexo e grupo de idade
#
# 5. Executar o teste Wilcoxon (Mann-Whitney) para comparação de médias entre duas amostras independentes
#    - Exemplo: comparação da média de quantidade de dias de internação por sexo e grupo de idade

##############################################################
# 1. IMPORTAÇÃO DO ARQUIVO
##############################################################
library(tidyverse)

# Importa os dados do arquivo salvo anteriormente (dados do SIH 2024 - DF)
sih_df_2024 <- readRDS("dados/sih_df_2024.rds")

# Verifica a estrutura dos dados
glimpse(sih_df_2024)

##############################################################
# 2. PREPARAÇÃO DAS VARIÁVEIS DE EXPOSIÇÃO E DESFECHO
##############################################################

# Desfecho: variável binária MORTE
# Vamos assumir que já está codificada como 0 = não morreu, 1 = morreu
# Verificando a estrutura
table(sih_df_2024$MORTE, useNA = "ifany")

# VARIÁVEL DE EXPOSIÇÃO PARA RISCO RELATIVO: SEXO
# A variável já foi tratada anteriormente como "Masculino", "Feminino", "Ignorado"
# Vamos usar apenas Masculino e Feminino

# Transformando a variável SEXO:
# 1 → "Masculino"
# 3 → "Feminino"
# Outros valores ou NA → "Ignorado"
sih_df_2024 <- sih_df_2024 %>%
  mutate(SEXO = case_when(
    SEXO == 1 ~ "Masculino",
    SEXO == 3 ~ "Feminino",
    TRUE ~ "Ignorado"
  ))

# Selecionando apenas observações válidas para cálculo do risco relativo
df_rr <- sih_df_2024 %>%
  filter(SEXO %in% c("Masculino", "Feminino")) %>%
  select(SEXO, MORTE)

# Conferência da distribuição
table(df_rr$SEXO, df_rr$MORTE)

# VARIÁVEL DE EXPOSIÇÃO PARA ODDS RATIO: IDOSO
# Consideramos "idoso" quem tem 65 anos ou mais com base na coluna IDADE original
sih_df_2024 <- sih_df_2024 %>%
  mutate(IDOSO = if_else(IDADE >= 65, "Idoso", "Adulto"))

# Seleciona apenas linhas com IDOSO e MORTE não nulas
df_or <- sih_df_2024 %>%
  filter(!is.na(IDOSO), !is.na(MORTE)) %>%
  select(IDOSO, MORTE)

# Conferência da distribuição
table(df_or$IDOSO, df_or$MORTE)

# VARIÁVEL DE EXPOSIÇÃO PARA RISCO ATRIBUÍVEL: CATEGORIA_DIARIAS
# Expostos: QT_DIARIAS > 30
sih_df_2024 <- sih_df_2024 %>%
  mutate(CATEGORIA_DIARIAS = if_else(QT_DIARIAS > 30, "30+", "<=30"))

df_ra <- sih_df_2024 %>%
  filter(!is.na(CATEGORIA_DIARIAS), !is.na(MORTE)) %>%
  select(CATEGORIA_DIARIAS, MORTE)

# Conferência da distribuição
table(df_ra$CATEGORIA_DIARIAS, df_ra$MORTE)

# VARIÁVEL DE EXPOSIÇÃO PARA RAZÃO DE PREVALÊNCIA: UTI em mês específico
# Seleciona apenas um mês (ex: março de 2024)
df_rp <- sih_df_2024 %>%
  mutate(
    DT_INTER = ymd(DT_INTER),  # converte corretamente a data de internação
    UTI = case_when(
      is.na(VAL_UTI) ~ "Ignorado",
      VAL_UTI > 0 ~ "Sim",
      VAL_UTI == 0 ~ "Não"
    )
  ) %>%
  filter(UTI %in% c("Sim", "Não")) %>%   # mantém apenas categorias binárias
  filter(!is.na(MORTE)) %>%             # exclui linhas sem informação de desfecho
  select(UTI, MORTE)

# Conferência da distribuição
table(df_rp$UTI, df_rp$MORTE)

##############################################################
# 3. MEDIDAS DE ASSOCIAÇÃO A PARTIR DE MATRIZES 2x2
##############################################################

# Criamos as tabelas de contingência para cada par exposição-desfecho

# RISCO RELATIVO (RR) - SEXO x MORTE
matriz_rr <- table(df_rr$SEXO, df_rr$MORTE)
print(matriz_rr)

# a = óbitos entre mulheres
# b = não óbitos entre mulheres
# c = óbitos entre homens
# d = não óbitos entre homens
a <- matriz_rr["Feminino", "1"]
b <- matriz_rr["Feminino", "0"]
c <- matriz_rr["Masculino", "1"]
d <- matriz_rr["Masculino", "0"]

# Calculando riscos
risco_mulheres <- a / (a + b)
risco_homens <- c / (c + d)

# Risco Relativo (RR)
rr <- risco_homens / risco_mulheres
rr

# ODDS RATIO (OR) - IDOSO x MORTE
matriz_or <- table(df_or$IDOSO, df_or$MORTE)
print(matriz_or)

# Acessa os elementos corretamente
a <- matriz_or["Idoso", "1"]   # Idosos que morreram
b <- matriz_or["Idoso", "0"]   # Idosos que não morreram
c <- matriz_or["Adulto", "1"]  # Adultos que morreram
d <- matriz_or["Adulto", "0"]  # Adultos que não morreram

# Cálculo da Odds Ratio (OR)
or <- (a / b) / (c / d)
or

# RISCO ATRIBUÍVEL (RA) - QT_DIARIAS x MORTE
matriz_ra <- table(df_ra$CATEGORIA_DIARIAS, df_ra$MORTE)
print(matriz_ra)

a <- matriz_ra["30+", "1"]     # óbitos entre expostos (mais de 30 dias)
b <- matriz_ra["30+", "0"]     # não óbitos entre expostos
c <- matriz_ra["<=30", "1"]    # óbitos entre não expostos
d <- matriz_ra["<=30", "0"]    # não óbitos entre não expostos

# Cálculo dos riscos
risco_expostos <- a / (a + b)
risco_nao_expostos <- c / (c + d)

# Risco Atribuível
ra <- risco_expostos - risco_nao_expostos
ra

# 3.4 RAZÃO DE PREVALÊNCIA (RP) - UTI x MORTE
matriz_rp <- table(df_rp$UTI, df_rp$MORTE)
print(matriz_rp)

a <- matriz_rp["Sim", "1"]
b <- matriz_rp["Sim", "0"]
c <- matriz_rp["Não", "1"]
d <- matriz_rp["Não", "0"]

# Prevalência nos dois grupos
prev_uti <- a / (a + b)
prev_nao_uti <- c / (c + d)

# Razão de Prevalência
rp <- prev_uti / prev_nao_uti
rp

##############################################################
# 4. TESTES ESTATÍSTICOS DE ASSOCIAÇÃO
##############################################################

# TESTE DO QUI-QUADRADO (χ²)
# Aplicado quando queremos testar se há associação entre duas variáveis categóricas
# No nosso caso: SEXO (Masculino/Feminino) e MORTE (Sim/Não)

# MATRIZ JÁ CRIADA ANTERIORMENTE:
print(matriz_rr)

# CONDIÇÕES PARA USO DO TESTE QUI-QUADRADO:
# - A tabela deve ser 2x2 ou maior (duas variáveis categóricas)
# - As observações devem ser independentes
# - Espera-se que pelo menos 80% das células tenham frequência esperada ≥ 5
# - Nenhuma célula deve ter frequência esperada < 1

# Executando o teste
teste_chi <- chisq.test(matriz_rr)

# Resultado do teste
print(teste_chi)

# Frequências esperadas (para verificar se o teste é adequado)
teste_chi$expected

##############################################################
# 4.2 TESTE EXATO DE FISHER (FISHER’S EXACT TEST)
##############################################################

# O teste de Fisher é indicado quando:
# - As frequências esperadas em alguma célula são < 5
# - Especialmente útil em tabelas 2x2 com amostras pequenas
# - É um teste exato (não usa aproximação), ideal para dados escassos

# Vamos criar uma matriz simulada com valores baixos
# Exemplo: associação entre "uso de medicamento" e "ocorrência de evento adverso"

#             Evento Adverso
#             Sim   Não
# Usou        1     4
# Não Usou    6     2

matriz_fisher <- matrix(c(1, 6, 4, 2),
                        nrow = 2,
                        byrow = TRUE)

# Nomeando linhas e colunas
rownames(matriz_fisher) <- c("Usou", "Não Usou")
colnames(matriz_fisher) <- c("Sim", "Não")

# Visualizando a matriz
print(matriz_fisher)

# Aplicando o teste exato de Fisher
teste_fisher <- fisher.test(matriz_fisher)

# Exibindo o resultado
print(teste_fisher)

##############################################################
# 5. TESTE t DE STUDENT PARA DUAS AMOSTRAS INDEPENDENTES
##############################################################

# O teste t é usado para comparar a média de uma variável numérica entre dois grupos independentes

# CRITÉRIOS PARA USO DO TESTE t:
# - A variável resposta é contínua (ex: número de diárias)
# - Os grupos são independentes (ex: masculino vs feminino)
# - A variável segue uma distribuição aproximadamente normal em cada grupo
# - As variâncias dos dois grupos devem ser semelhantes (pode-se testar com var.test ou usar versão com Welch)

# Vamos comparar a média de QT_DIARIAS entre:
# 1. Sexo (Masculino vs Feminino)
# 2. Idoso vs Adulto

# 5.1 TESTE t - QT_DIARIAS por SEXO

# Seleciona apenas linhas com SEXO válido e QT_DIARIAS não nulo
df_t_sexo <- sih_df_2024 %>%
  filter(SEXO %in% c("Masculino", "Feminino"), !is.na(QT_DIARIAS)) %>%
  mutate(SEXO = factor(SEXO))  # força a variável como fator com 2 níveis

# Verifica os níveis para segurança
levels(df_t_sexo$SEXO)

# Executa o teste t
t_sexo <- t.test(QT_DIARIAS ~ SEXO, data = df_t_sexo, var.equal = FALSE)
print(t_sexo)

# 5.2 TESTE t - QT_DIARIAS por IDOSO vs ADULTO

# Criando variável binária: Idoso (>=65) vs Adulto (<65)
df_t_idoso <- sih_df_2024 %>%
  mutate(GRUPO_IDADE = if_else(IDADE >= 65, "Idoso", "Adulto")) %>%
  filter(!is.na(GRUPO_IDADE), !is.na(QT_DIARIAS))

# Teste t entre idosos e adultos
t_idoso <- t.test(QT_DIARIAS ~ GRUPO_IDADE, data = df_t_idoso, var.equal = FALSE)
print(t_idoso)

##############################################################
# 5.3 VISUALIZAÇÃO: HISTOGRAMAS SOBREPOSTOS POR SEXO
##############################################################

ggplot(df_t_sexo, aes(x = QT_DIARIAS, fill = SEXO)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  scale_fill_manual(values = c("Feminino" = "red", "Masculino" = "blue")) +
  labs(
    title = "Distribuição do número de diárias por sexo",
    x = "Número de diárias",
    y = "Frequência"
  ) +
  theme_minimal()

##############################################################
# 5.4 VISUALIZAÇÃO: BOXPLOT POR GRUPO DE IDADE
##############################################################

ggplot(df_t_idoso, aes(x = GRUPO_IDADE, y = QT_DIARIAS, fill = GRUPO_IDADE)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Adulto" = "lightblue", "Idoso" = "orange")) +
  labs(
    title = "Comparação de número de diárias entre adultos e idosos",
    x = "Grupo etário",
    y = "Número de diárias"
  ) +
  theme_minimal()

##############################################################
# 5.5 TESTE NÃO PARAMÉTRICO DE WILCOXON (MANN–WHITNEY)
##############################################################

# Este teste é uma alternativa ao teste t de Student quando:
# - As distribuições são assimétricas ou não seguem a normalidade
# - Ainda estamos comparando a tendência central de duas amostras independentes

# COMPARAÇÃO DE QT_DIARIAS ENTRE SEXOS

# Usamos o df_t_sexo criado anteriormente (com SEXO já recodificado e filtrado)

# Aplicando o teste de Wilcoxon para QT_DIARIAS por SEXO
teste_wilcox_sexo <- wilcox.test(QT_DIARIAS ~ SEXO, data = df_t_sexo)

# Resultado do teste
print(teste_wilcox_sexo)

# COMPARAÇÃO DE QT_DIARIAS ENTRE ADULTOS E IDOSOS

# Criando e preparando os dados
df_t_idoso <- sih_df_2024 %>%
  mutate(GRUPO_IDADE = if_else(IDADE >= 65, "Idoso", "Adulto")) %>%
  filter(!is.na(GRUPO_IDADE), !is.na(QT_DIARIAS)) %>%
  mutate(GRUPO_IDADE = factor(GRUPO_IDADE))

# Aplicando o teste de Wilcoxon para QT_DIARIAS por GRUPO_IDADE
teste_wilcox_idoso <- wilcox.test(QT_DIARIAS ~ GRUPO_IDADE, data = df_t_idoso)

# Resultado do teste
print(teste_wilcox_idoso)