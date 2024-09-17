# Função para garantir que os pacotes necessários estão instalados e carregados
ensure_packages <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
    library(pkg, character.only = TRUE)
  }
}

# Lista de pacotes necessários para o projeto
required_packages <- c("dplyr", "ggplot2", "readr", "ez", 
                       "lavaan", "semPlot", "pracma")

# Garantir que os pacotes estão instalados
ensure_packages(required_packages)

# Função para verificar se todas as colunas necessárias estão presentes
check_required_columns <- function(data, required_columns) {
  missing_columns <- setdiff(required_columns, colnames(data))
  if (length(missing_columns) > 0) {
    stop("Colunas ausentes no arquivo de dados: ", paste(missing_columns, collapse = ", "))
  }
}

# Função para ler o arquivo CSV e retornar os dados
load_data <- function(file_path) {
  data <- readr::read_csv(file_path, show_col_types = FALSE)
  return(data)
}

# Função para imprimir um resumo dos dados carregados
print_data_summary <- function(data) {
  print(summary(data))
}