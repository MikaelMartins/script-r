# ---------------------------------------------------------------
# Análise Descritiva da Base Iris
# Autor: Mikael Aurio Martins de Paula da Silva
# Curso: Engenharia de Computação
# ---------------------------------------------------------------

# Pacotes
library(tidyverse)

# Base de Dados
data("iris")

# Estatísticas Descritivas Globais
summary(iris)

# Estatísticas por Espécie
iris %>%
  group_by(Species) %>%
  summarise(across(where(is.numeric), list(
    média = mean,
    mediana = median,
    sd = sd,
    min = min,
    max = max
  ), .names = "{.col}_{.fn}"))

# ---------------------------------------------------------------
# Gráficos Cores
# ---------------------------------------------------------------
palette_terra <- c(
  "setosa" = "#355E3B",     # Verde musgo
  "versicolor" = "#2F4858", # Azul petróleo
  "virginica" = "#A47551"   # Marrom areia
)

plot_box <- function(var){
  ggplot(iris, aes(x = Species, y = .data[[var]], fill = Species)) +
    geom_boxplot(alpha = 0.85, color = "black", linewidth = 0.6) +
    scale_fill_manual(values = palette_terra) +
    labs(title = paste(var, "por Espécie"), x = "Espécie", y = var) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      legend.position = "none"
    )
}

# Visualização dos gráficos na tela
plot_box("Sepal.Length")
plot_box("Sepal.Width")
plot_box("Petal.Length")
plot_box("Petal.Width")

# ---------------------------------------------------------------
# Salvar todos os gráficos em um único PDF
# ---------------------------------------------------------------
pdf("Boxplots_Data_Iris.pdf", width = 8, height = 10)

gridExtra::grid.arrange(
  plot_box("Sepal.Length"),
  plot_box("Sepal.Width"),
  plot_box("Petal.Length"),
  plot_box("Petal.Width"),
  ncol = 2
)

dev.off()
