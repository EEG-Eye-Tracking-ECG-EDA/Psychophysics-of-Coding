# Carregar pacotes necessários
ensure_packages <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
    library(pkg, character.only = TRUE)
  }
}

# Pacotes necessários para o processamento
required_packages <- c("dplyr", "readr", "ggplot2", "ez", "pracma", "signal")

# Garantir que os pacotes estão instalados
ensure_packages(required_packages)

# Função para verificar colunas obrigatórias
check_required_columns <- function(data, required_columns) {
  missing_columns <- setdiff(required_columns, colnames(data))
  if (length(missing_columns) > 0) {
    stop("Colunas ausentes no arquivo de dados: ", paste(missing_columns, collapse = ", "))
  }
}

# Função para ler o arquivo CSV
import_data <- function(file_path) {
  data <- readr::read_csv(file_path, show_col_types = FALSE)
  return(data)
}

# Função para validar e formatar tipos de colunas
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

# Função para visualização de séries temporais de EEG Alpha
plot_time_series <- function(data) {
  ggplot2::ggplot(data, ggplot2::aes(x = Condição, y = EEG_alpha, color = Participante)) +
    ggplot2::geom_line() +
    ggplot2::labs(title = "EEG Alpha ao longo das condições", x = "Condição", y = "EEG Alpha")
}

# Função principal para execução no servidor do Jamovi
run_analysis <- function(dataset, options) {
  
  # Verificar se os dados estão disponíveis
  if (is.null(dataset)) {
    stop("Nenhum dado foi carregado.")
  }
  
  # Colunas obrigatórias
  required_columns <- c("Participante", "Condição", "EDA_mean", "ECG_HR", 
                        "EEG_alpha", "EEG_beta", "EyeTracking_Fixations", 
                        "STAI_score", "SPSES_score")
  
  # Verificar se as colunas necessárias estão presentes
  check_required_columns(dataset, required_columns)
  
  # Validar tipos de dados
  dataset <- validate_column_types(dataset)
  
  # Visualizar dados: Série temporal de EEG Alpha
  plot <- plot_time_series(dataset)
  
  # Realizar análise: ANOVA de medidas repetidas
  results <- ez::ezANOVA(
    data = dataset,
    dv = .(EEG_alpha),
    wid = .(Participante),
    within = .(Condição)
  )
  
  # Retornar resultados e gráfico para o Jamovi
  return(list(results = results, plot = plot))
}