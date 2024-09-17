# Garantir que os pacotes estão instalados
ensure_packages <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
    library(pkg, character.only = TRUE)
  }
}

# Pacotes necessários
required_packages <- c("dplyr", "readr", "ez", "lavaan", "ggplot2", "pracma", "signal")
ensure_packages(required_packages)

# Função principal
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

  # Executar ANOVA com medidas repetidas
  anova_results <- ez::ezANOVA(
    data = dataset,
    dv = .(EEG_alpha),  # Variável dependente
    wid = .(Participante),
    within = .(Condição),
    return_aov = TRUE
  )

  # Visualizar séries temporais
  plot_time_series <- function(data) {
    ggplot2::ggplot(data, ggplot2::aes(x = Condição, y = EEG_alpha, color = Participante)) +
      ggplot2::geom_line() +
      ggplot2::labs(title = "EEG Alpha ao longo das condições", x = "Condição", y = "EEG Alpha")
  }

  # Resultados
  return(list(
    anova = anova_results,
    plot = plot_time_series(dataset)
  ))
}