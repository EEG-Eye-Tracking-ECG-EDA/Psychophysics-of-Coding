# Função para garantir que os pacotes necessários estão instalados e carregados
ensure_packages <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
    library(pkg, character.only = TRUE)
  }
}

# Pacotes necessários para o processamento
required_packages <- c("dplyr", "readr", "ez", "lavaan", "ggplot2", "pracma", "signal")

# Garantir que os pacotes estão instalados
ensure_packages(required_packages)

# Função para importar e verificar os dados
import_data <- function(file_path) {
  # Leitura do arquivo CSV com readr para maior eficiência
  data <- readr::read_csv(file_path, show_col_types = FALSE)
  return(data)
}

# Função para verificar se todas as colunas necessárias estão presentes
check_required_columns <- function(data, required_columns) {
  missing_columns <- setdiff(required_columns, colnames(data))
  if (length(missing_columns) > 0) {
    stop("Colunas ausentes no arquivo de dados: ", paste(missing_columns, collapse = ", "))
  }
}

# Lista de colunas necessárias
required_columns <- c("Participante", "Condição", "EDA_mean", "ECG_HR", 
                      "EEG_alpha", "EEG_beta", "EyeTracking_Fixations", 
                      "STAI_score", "SPSES_score")

# Função para garantir que os tipos de dados estão corretos
validate_column_types <- function(data) {
  data <- data %>%
    mutate(
      Participante = as.factor(Participante),
      Condição = as.factor(Condição),
      EDA_mean = as.numeric(EDA_mean),
      ECG_HR = as.numeric(ECG_HR),
      EEG_alpha = as.numeric(EEG_alpha),
      EEG_beta = as.numeric(EEG_beta),
      EyeTracking_Fixations = as.numeric(EyeTracking_Fixations),
      STAI_score = as.numeric(STAI_score),
      SPSES_score = as.numeric(SPSES_score)
    )
  return(data)
}

# Função final para importar, verificar e validar os dados
import_and_validate_data <- function(file_path) {
  # Tentar importar o arquivo CSV
  data <- tryCatch({
    import_data(file_path)
  }, error = function(e) {
    stop("Erro ao importar o arquivo CSV: ", e$message)
  })
  
  # Verificar se as colunas necessárias estão presentes
  check_required_columns(data, required_columns)
  
  # Validar e converter os tipos de dados
  data <- validate_column_types(data)
  
  return(data)
}

# Exemplo de uso da função
file_path <- "dados_neurofisiologicos.csv"
data <- import_and_validate_data(file_path)