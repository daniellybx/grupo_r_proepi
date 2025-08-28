##############################################################
# AULA 2 - IMPORTANDO E EXPORTANDO DADOS
##############################################################

# OBJETIVOS DA AULA:
# 1. Apresentar o uso do pacote microdatasus para importar dados do DATASUS
# 2. Demonstrar diferentes formas de seleção de variáveis no SIH
# 3. Realizar a importação de dados reais do SIH 2024 (DF)
# 4. Exportar dados em diferentes tipos de arquivo
# 5. Criar e apagar pastas

##############################################################
# 1. CARREGANDO O PACOTE microdatasus
##############################################################

# Caso não tenha instalado na Aula 1:
# install.packages("remotes")
# remotes::install_github("rfsaldanha/microdatasus")

library(microdatasus)

##############################################################
# 2. IMPORTAÇÃO DE DADOS DO SIH COM SELEÇÃO DE VARIÁVEIS
##############################################################

# Vamos importar dados do SIH-RD (Autorizações de Internação Hospitalar) para o DF,
# no mês de dezembro de 2008, usando três abordagens diferentes.

# Definindo um vetor de variáveis a ser usado na função
vars_select <- c("MORTE", "CID_MORTE", "DIAG_PRINC")

sih_df_vars1 <- fetch_datasus(
  year_start = 2008, year_end = 2008,
  month_start = 12, month_end = 12,
  uf = "DF", vars = vars_select,
  information_system = "SIH-RD"
)

# Definindo diretamente as variáveis dentro da função
sih_df_vars2 <- fetch_datasus(
  year_start = 2008, year_end = 2008,
  month_start = 12, month_end = 12,
  uf = "DF", vars = c("MORTE", "CID_MORTE", "DIAG_PRINC"),
  information_system = "SIH-RD"
)

##############################################################
# 3. IMPORTANDO DADOS DO SIH-RD PARA O ANO DE 2024 (DF)
##############################################################

# Agora, vamos importar os dados reais de 2024 do SIH-RD para o Distrito Federal,
# para usarmos nas etapas seguintes da aula (medidas descritivas e associações).

# Este dataset será usado em toda a aula 2:
sih_df_2024 <- fetch_datasus(
  year_start = 2024, year_end = 2024,
  month_start = 10, month_end = 12,
  uf = "DF",
  information_system = "SIH-RD"
)

##############################################################
# 4. EXPLORANDO E EXPORTANDO DATAFRAMES
##############################################################

# Vamos usar o dataframe sih_df_2024 importado anteriormente
# Nesta seção, exploramos diferentes formas de inspecionar e manipular o dataframe

##############################
# 4.1 VISUALIZAÇÃO COM R BASE
##############################

# Ver as primeiras linhas
head(sih_df_2024)

# Ver as últimas linhas
tail(sih_df_2024)

# Ver estrutura do objeto (tipos de variáveis)
str(sih_df_2024)

# Ver nomes das colunas
names(sih_df_2024)

# Número de linhas e colunas
nrow(sih_df_2024)
ncol(sih_df_2024)
dim(sih_df_2024)

# Sumário estatístico
summary(sih_df_2024)

##############################
# 4.2 VISUALIZAÇÃO COM TIDYVERSE
##############################

library(tidyverse)

# Visualizar as 5 primeiras linhas
sih_df_2024 %>% 
  slice_head(n = 5)

# Visualizar colunas específicas
sih_df_2024 %>% 
  select(MUNIC_RES, SEXO, IDADE)

# Resumo rápido com glimpse
sih_df_2024 %>% 
  glimpse()

# Contagem de valores únicos
sih_df_2024 %>% 
  count(SEXO)

# Contagem com proporções
sih_df_2024 %>% 
  count(SEXO) %>%
  mutate(prop = n / sum(n))

##############################
# 4.3 EXPORTANDO OS DADOS
##############################

# Exportar como CSV
write.csv(sih_df_2024, "dados/sih_df_2024.csv", row.names = FALSE)

# Exportar como Excel (necessário pacote readxl/writexl)
install.packages("writexl")
library(writexl)

write_xlsx(sih_df_2024, "dados/sih_df_2024.xlsx")

# Exportar como RDS (formato nativo do R)
saveRDS(sih_df_2024, "dados/sih_df_2024.rds")

##############################
# 4.4 IMPORTANDO OS DADOS
##############################

# Importar CSV
sih_lido_csv <- read.csv("dados/sih_df_2024.csv")

# Importar Excel
install.packages("readxl")
library(readxl)

sih_lido_xlsx <- read_excel("dados/sih_df_2024.xlsx")

# Importar RDS
sih_lido_rds <- readRDS("dados/sih_df_2024.rds")

# Verificando estrutura de um dos arquivos importados
str(sih_lido_rds)

################################
# 5. Criando e apagando pastas
################################
# Criar uma pasta chamada "teste"
dir.create("teste")

# Apagar a pasta "teste"
# Se a pasta estiver vazia, ela será apagada sem problemas
# Se a pasta contiver arquivos ou subpastas, use o argumento recursive = TRUE

unlink("teste", recursive = TRUE)

# Atenção:
# Ao usar unlink() com recursive = TRUE, todos os arquivos e subpastas dentro da pasta serão apagados permanentemente.
# Essa ação é irreversível, por isso deve ser usada com cuidado.