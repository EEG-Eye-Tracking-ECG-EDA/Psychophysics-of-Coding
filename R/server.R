# Função principal para execução no servidor do Jamovi
run_analysis <- function(dataset, options) {
  
  # Verificar se os dados estão disponíveis
  if (is.null(dataset)) {
    stop("Nenhum dado foi carregado.")
  }
  
  # Verificar se as colunas necessárias estão presentes
  required_columns <- c("Participante", "Condição", "EDA_mean", "ECG_HR", 
                        "EEG_alpha", "EEG_beta", "EyeTracking_Fixations", 
                        "STAI_score", "SPSES_score")
  
  check_required_columns <- function(data, required_columns) {
    missing_columns <- setdiff(required_columns, colnames(data))
    if (length(missing_columns) > 0) {
      stop("Colunas ausentes no arquivo de dados: ", paste(missing_columns, collapse = ", "))
    }
  }
  
  check_required_columns(dataset, required_columns)
  
  # Validar tipos de dados
  dataset <- validate_column_types(dataset)
  
  # Pré-processamento dos sinais EEG e ECG
  dataset <- preprocess_signals(dataset)
  
  # Realizar análise ANOVA ou SEM com base em opções do usuário
  results <- list()
  
  if (options$perform_anova) {
    results$anova <- perform_anova(dataset)
  }
  
  if (options$perform_sem) {
    results$sem <- perform_sem(dataset)
  }
  
  # Visualizações
  plots <- list()
  
  if (options$plot_time_series) {
    plots$time_series <- plot_time_series(dataset)
  }
  
  if (options$plot_bar_comparison) {
    plots$bar_comparison <- plot_bar_comparison(dataset, options$comparison_variable)
  }
  
  if (options$plot_heatmap_eye_tracking) {
    plots$eye_tracking <- plot_heatmap_eye_tracking(dataset)
  }
  
  # Retornar resultados e gráficos para o Jamovi
  return(list(results = results, plots = plots))
}