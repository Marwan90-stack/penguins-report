library(tidyverse)
library(gt)

data(penguins)
my_font = "Open Sans"
my_colors = c("#f9cdad", "#c8c8a9", "#83af9b")
my_colors2 = c("#0686f0", "#28abdf", "#c3c1c1")
my_colors3 = c("#033649", "#036564", "#cdb380")

bar_chart <- function(
  df = NULL,
  x = NULL,
  y = NULL,
  fill = NULL,
  label = NULL,
  y_label = NULL
) {
  df %>%
    # filter(!is.na(sex)) %>%
    # group_by(sex, island) %>%
    # summarise(m_body_mass = mean(body_mass, na.rm = TRUE), .groups = "drop") %>%
    ggplot(aes(x = x, y = y, fill = fill)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.90) +
    geom_text(
      aes(label = label),
      color = "white",
      hjust = 0.5,
      vjust = 1.5,
      position = position_dodge(width = 0.90)
    ) +
    theme_minimal() +
    scale_fill_manual(values = my_colors3) +
    scale_y_continuous(
      expand = c(0, Inf),
      labels = scales::number_format(scale = 1 / 1000, suffix = "")
    ) +
    scale_x_discrete(labels = function(x) str_to_title(x), expand = c(0, 0)) +
    labs(
      x = "",
      y = y_label,
      fill = ""
    ) +
    theme(
      legend.position = 'top',
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      legend.key.size = unit(10, "pt"),
      legend.key.justification = "left",
      legend.justification = "left",
      text = element_text(family = my_font)
    )
}
