library(tidyverse)
library(readxl)
##The data in the file “adjusted_quarterly_gdp.xlsx” were computed using Stata. The estimation commands were as follows:
#mixed lntrue_gdp i.quarter $all_controls || provinceid: || citycode: if year==XXX, reml
#margins quarter
#mixed ln_gdp i.quarter $all_controls || provinceid: || citycode: if year==XXX, reml
#margins quarter
#Note that “XXX” should be replaced with the specific year.
file_path <- "adjusted_quarterly_gdp.xlsx"
preds_data <- read_excel(file_path)

ribbon_data <- preds_data %>%
  mutate(year_quarter = year + (quarter - 1) / 4) %>%
  pivot_wider(names_from = gdp_type, values_from = predicted_gdp)

# ggplot2
adjusted_plot_with_shading <- ggplot(ribbon_data, aes(x = year_quarter)) +
  
  # geom_ribbon

  geom_ribbon(aes(ymin = `True GDP`, ymax = `Reported GDP`), fill = "grey70", alpha = 0.5) +
  
  # 在阴影之上绘制两条趋势线
  geom_line(aes(y = `Reported GDP`, color = "Reported GDP"), linewidth = 1.1) +
  geom_line(aes(y = `True GDP`, color = "True GDP"), linewidth = 1.1) +
  

  labs(
    title = "Adjusted Quarterly Trends of Reported vs. True GDP (2013-2020)",
    subtitle = "Predictions from Year-by-Year Multilevel Models with Controls",
    x = "Year",
    y = "Adjusted Average Log GDP"
  ) +
  scale_color_manual(values = c("Reported GDP" = "firebrick", "True GDP" = "steelblue")) +
  scale_x_continuous(breaks = seq(2013, 2019, by = 1)) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.grid.minor.x = element_blank()
  )


print(adjusted_plot_with_shading)


ggsave("Adjusted_GDP_Trends_with_Shading.png", adjusted_plot_with_shading, width = 12, height = 7, dpi = 300)