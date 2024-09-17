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

# Função para detectar e corrigir artefatos em EDA
correct_eda_artifacts <- function(data) {
  # Exemplo de detecção simples de artefatos com base em um limiar
  data$EDA_mean <- ifelse(data$EDA_mean > 1 | data$EDA_mean < 0, NA, data$EDA_mean)
  
  # Substituir os valores ausentes (NA) pela média dos valores válidos
  data$EDA_mean <- ifelse(is.na(data$EDA_mean), mean(data$EDA_mean, na.rm = TRUE), data$EDA_mean)
  
  return(data)
}

# Função para escalonar os sinais neurofisiológicos (normalização)
scale_signals <- function(data) {
  data <- data %>%
    mutate(
      EDA_mean = scale(EDA_mean),
      ECG_HR = scale(ECG_HR),
      EEG_alpha = scale(EEG_alpha),
      EEG_beta = scale(EEG_beta),
      EyeTracking_Fixations = scale(EyeTracking_Fixations)
    )
  
  return(data)
}