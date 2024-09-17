# Função para executar ANOVA com medidas repetidas e retornar os resultados
perform_anova <- function(data) {
  
  # Executar a ANOVA para cada variável neurofisiológica
  anova_results <- list(
    EEG_alpha = ez::ezANOVA(
      data = data,
      dv = .(EEG_alpha),
      wid = .(Participante),
      within = .(Condição),
      return_aov = TRUE
    ),
    EEG_beta = ez::ezANOVA(
      data = data,
      dv = .(EEG_beta),
      wid = .(Participante),
      within = .(Condição),
      return_aov = TRUE
    ),
    ECG_HR = ez::ezANOVA(
      data = data,
      dv = .(ECG_HR),
      wid = .(Participante),
      within = .(Condição),
      return_aov = TRUE
    ),
    EDA_mean = ez::ezANOVA(
      data = data,
      dv = .(EDA_mean),
      wid = .(Participante),
      within = .(Condição),
      return_aov = TRUE
    )
  )
  
  return(anova_results)
}

# Função para executar a análise SEM e retornar o modelo ajustado
perform_sem <- function(data) {
  
  # Definir o modelo SEM
  sem_model <- '
    # Regressões
    SPSES_score ~ ECG_HR + EDA_mean
    STAI_score ~ ECG_HR + SPSES_score
  '
  
  # Ajustar o modelo com lavaan
  sem_fit <- lavaan::sem(sem_model, data = data)
  
  # Resumo dos resultados
  sem_summary <- summary(sem_fit, fit.measures = TRUE)
  
  return(sem_summary)
}

# Função para gerar um gráfico de séries temporais
plot_time_series <- function(data) {
  ggplot2::ggplot(data, ggplot2::aes(x = Condição, y = EEG_alpha, group = Participante, color = Participante)) +
    ggplot2::geom_line() +
    ggplot2::labs(
      title = "Variação de EEG Alpha ao longo das Condições",
      x = "Condição",
      y = "EEG Alpha"
    ) +
    ggplot2::theme_minimal()
}

# Função para gerar um gráfico de barras comparando as condições
plot_bar_comparison <- function(data, var) {
  ggplot2::ggplot(data, ggplot2::aes(x = Condição, y = !!sym(var), fill = Condição)) +
    ggplot2::geom_bar(stat = "summary", fun = "mean", position = "dodge") +
    ggplot2::labs(
      title = paste("Comparação de", var, "entre Condições"),
      x = "Condição",
      y = var
    ) +
    ggplot2::theme_minimal()
}

# Função para gerar mapa de calor com dados de Eye Tracking
plot_heatmap_eye_tracking <- function(data) {
  ggplot2::ggplot(data, ggplot2::aes(x = Condição, y = EyeTracking_Fixations, fill = EyeTracking_Fixations)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient(low = "blue", high = "red") +
    ggplot2::labs(
      title = "Mapa de Calor de Fixações do Eye Tracking",
      x = "Condição",
      y = "Fixações"
    ) +
    ggplot2::theme_minimal()
}