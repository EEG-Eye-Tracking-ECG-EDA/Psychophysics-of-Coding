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
  # Leitura do arquivo CSV
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
  
  return(data)
}

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

# Função para pré-processar sinais de EEG e ECG (exemplo de filtragem)
preprocess_signals <- function(data) {
  # Aplicar filtro passa-banda ao EEG e ECG
  # Exemplo: Filtro de 0.5 a 30 Hz para EEG
  data$EEG_alpha <- pracma::bandpass(data$EEG_alpha, 0.5, 30, Fs = 256)  # Fs é a taxa de amostragem
  data$EEG_beta <- pracma::bandpass(data$EEG_beta, 0.5, 30, Fs = 256)
  data$ECG_HR <- pracma::bandpass(data$ECG_HR, 0.5, 40, Fs = 256)  # Exemplo de filtragem de ECG
  
  return(data)
}

# Função para realizar ANOVA com medidas repetidas usando o pacote ez
run_anova <- function(data) {
  anova_results <- ez::ezANOVA(
    data = data,
    dv = .(EEG_alpha),  # Variável dependente
    wid = .(Participante),
    within = .(Condição),
    return_aov = TRUE
  )
  
  return(anova_results)
}

# Função para construir e ajustar um modelo SEM com o lavaan
run_sem <- function(data) {
  model <- '
    # Regressões
    SPSES_score ~ ECG_HR + EDA_mean
    STAI_score ~ ECG_HR + SPSES_score
  '
  
  fit <- lavaan::sem(model, data = data)
  summary(fit, fit.measures = TRUE)
}
5. Visualização dos dados
Finalmente, você pode definir funções para gerar gráficos como séries temporais, gráficos de barras comparando as condições e mapas de calor de Eye Tracking.

r

Copiar
# Função para visualizar séries temporais
plot_time_series <- function(data) {
  ggplot2::ggplot(data, ggplot2::aes(x = Condição, y = EEG_alpha, color = Participante)) +
    ggplot2::geom_line() +
    ggplot2::labs(title = "EEG Alpha ao longo das condições", x = "Condição", y = "EEG Alpha")
}
