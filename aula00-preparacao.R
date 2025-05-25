# ======================================================
# SEÇÃO 1: Criar Pasta de Dados
# ======================================================

# Cria a pasta "dados" se ela ainda não existir
dir.create("dados", showWarnings = FALSE)

# ======================================================
# SEÇÃO 2: Instalar e Carregar o Pacote do Livro Epi R
# ======================================================

# Instala a versão mais recente do pacote epirhandbook do GitHub
pacman::p_install_gh("appliedepi/epirhandbook", force = TRUE)

# Carrega o pacote para a sessão atual
pacman::p_load(epirhandbook)

# ======================================================
# SEÇÃO 3: Baixar o Livro do Epi R Offline
# ======================================================

# Baixa a versão offline do livro (abre janela para salvar localmente)
download_book()

# Observação:
# O arquivo baixado é um HTML completo, com imagens e índice lateral.
# Pode levar um ou dois minutos para carregar após abrir no navegador.

# ======================================================
# SEÇÃO 4: Baixar os Dados de Exemplo
# ======================================================

# Baixar todos os dados de exemplo diretamente para a pasta "dados"
get_data("all", path = "dados")

# Baixar apenas o conjunto de dados 'linelist_cleaned.rds' na pasta "dados"
get_data(file = "linelist_cleaned.rds", path = "dados")

# ======================================================
# SEÇÃO 5: Importar Arquivos Diretamente do GitHub
# ======================================================

# Instala e carrega o pacote rio para importação de dados
pacman::p_load(rio)

# Importa o dicionário de limpeza
cleaning_dict <- import("https://github.com/appliedepi/epirhandbook_eng/raw/master/data/case_linelists/cleaning_dict.csv")

# Importa dados Likert diretamente
likert_data <- import("https://raw.githubusercontent.com/appliedepi/epirhandbook_eng/master/data/likert_data.csv")

# Dados para padronização - País A
demo_A <- import("https://github.com/appliedepi/epirhandbook_eng/raw/master/data/standardization/country_demographics.csv")
deaths_A <- import("https://github.com/appliedepi/epirhandbook_eng/raw/master/data/standardization/deaths_countryA.csv")

# Dados para padronização - País B
demo_B <- import("https://github.com/appliedepi/epirhandbook_eng/raw/master/data/standardization/country_demographics_2.csv")
deaths_B <- import("https://github.com/appliedepi/epirhandbook_eng/raw/master/data/standardization/deaths_countryB.csv")

# População padrão global
standard_pop_data <- import("https://github.com/appliedepi/epirhandbook_eng/raw/master/data/standardization/world_standard_population_by_sex.csv")

# ======================================================
# SEÇÃO 6: Importar Dados Filogenéticos
# ======================================================

# Carrega pacote ape para árvores filogenéticas
pacman::p_load(ape)

# Importa a árvore filogenética (arquivo .txt)
shigella_tree <- read.tree("dados/Shigella_tree.txt")

# Importa metadados da árvore
sample_data <- import("dados/sample_data_Shigella_tree.csv")

# ======================================================
# SEÇÃO 7: Considerações Finais
# ======================================================

# Após executar este script, você terá:
# - O livro do Epi R salvo offline
# - Todos os dados de exemplo disponíveis localmente na pasta "dados"
# - Acesso facilitado aos arquivos mais usados diretamente do GitHub

# Para importar os arquivos .rds baixados:
# exemplo: linelist <- readRDS("dados/linelist_cleaned.rds")

