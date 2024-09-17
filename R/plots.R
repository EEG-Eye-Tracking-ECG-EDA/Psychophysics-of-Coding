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