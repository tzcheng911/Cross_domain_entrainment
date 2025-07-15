# Load necessary library
library(ggplot2)

# Create a data frame with the CI data
df <- data.frame(
  Experiment = c("EXP8a", "EXP8b", "EXP8c", "EXP9a", "EXP9b", "EXP10a", "EXP10b"),
  CI_lower = c(-0.2938755, -0.4348490, -0.4231266, -0.25790733, -0.4472609, -0.32457094, -0.3454729),
  CI_upper = c(0.006871951, -0.20259526, -0.17363975, -0.052076388, -0.22164672, -0.08680422, -0.10971487)
)

# Calculate the estimate (mean) and error
df$Estimate <- (df$CI_lower + df$CI_upper) / 2
df$Error <- (df$CI_upper - df$CI_lower) / 2

# Ensure the Experiment variable is a factor with reversed levels
df$Experiment <- factor(df$Experiment, levels = rev(df$Experiment))

# Save the plot as a PDF
pdf("confidence_intervals_plot.pdf", width = 8, height = 5)

ggplot(df, aes(y = Experiment, x = Estimate)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbarh(aes(xmin = Estimate - Error, xmax = Estimate + Error), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  labs(title = "Confidence Intervals for Experiments",
       x = "Estimate (with 95% CI)",
       y = "Experiment") +
  theme_minimal()

dev.off()
