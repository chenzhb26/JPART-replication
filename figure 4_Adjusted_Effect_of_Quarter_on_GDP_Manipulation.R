library(tidyverse)
library(haven)
# --------------------------------------------------------------------------
# Extract the coefficients and standard errors for Model 3 from Table 4. The first quarter is the baseline category, which is excluded from estimation (coefficient normalized to 0).
regression_results <- tibble(
  quarter = factor(1:4),
  coefficient = c(0,  -0.062, -0.088, 0.019),
  std_error = c(0, 0.011, 0.013, 0.004)
) %>%

  mutate(
    ci_lower = coefficient - 1.96 * std_error,
    ci_upper = coefficient + 1.96 * std_error
  )

# 打印数据框，检查一下
print(regression_results)


plot_B_from_regression <- ggplot(regression_results, aes(x = quarter, y = coefficient)) +
  
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, linewidth = 1, color = "darkblue") +
 
  geom_point(size = 5, color = "darkblue") +
 
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  
  labs(
    title = "Adjusted Effect of Quarter on GDP Manipulation",
    subtitle = "Coefficients from Mixed-Effects Model with Controls",
    x = "Quarter",
    y = "Coefficient (Difference relative to Q1)"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "grey30")
  )


print(plot_B_from_regression)


ggsave("figure 4.png", plot_B_from_regression, width = 8, height = 6, dpi = 300)