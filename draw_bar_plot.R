install.packages("tidyverse")
install.packages("ggsignif")
install.packages("ggbeeswarm")
install.packages("extrafont")

# Attach packages
library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(ggsignif)
library(ggbeeswarm)

# Read data
# "C:\Users\vrept\OneDrive\Desktop\how-to-ggplot\test_data_1.xlsx"
# data_file <- "C:/Users/CEEL-PC-005/Desktop/Joon/how-to-ggplot/test_data_1.xlsx" 
base_folder <- "C:/Users/vrept/OneDrive/Desktop/how-to-ggplot/"
data_file <- paste(base_folder, "test_data_1.xlsx", sep = "") 
data <- read_excel(data_file, sheet = "Sheet1", range = "A1:B10")

data$Condition <- factor(data$Condition, levels = c("Ctrl", "A drug", "B drug"))
data_summary <- data %>% group_by(Condition) %>% summarise(avg = mean(Gene_expression), std = sd(Gene_expression))
data_summary

# t test
p_val <- t.test(
  data[data$Condition == "Ctrl", "Gene_expression"],
  data[data$Condition == "A drug", "Gene_expression"]
)$p.value

if (p_val <= 1e-4) {
  signif_stars = "****"
} else if (p_val <= 1e-3){
  signif_stars = "***"
} else if (p_val <= 1e-2) {
  signif_stars = "**"
} else if (p_val <= 0.05) {
  signif_stars = "*"
} else {
  signif_stars = "n.s."
}

bar_plot <- ggplot(data_summary, aes(x=Condition, y=avg, fill=Condition)) + 
  geom_bar(stat = "identity", color = "black") +
  geom_jitter(data=data, aes(x=Condition, y=Gene_expression), width = 0.2) +
  geom_errorbar(aes(ymin=avg-std, ymax=avg+std),
                width = 0.2) +
  geom_signif(
    annotation = formatC(signif_stars),
    y_position = 2.25,
    xmin = 1, xmax = 2,
    textsize = 8/.pt
  ) +
  xlab("Treatment") +
  ylab("Relative Gene Expression") + 
  scale_y_continuous(limits = c(0,2.5), expand = c(0,0)) +
  scale_fill_manual(name = "Treatment",
                    values = c("blue", "red", "orange")) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 7),
    axis.title.y = element_text(size = 7),
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 6),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 6),
    axis.line = element_line("black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    plot.background = element_rect("white")
  ) +
  guides(fill = guide_legend(override.aes = list(shape = NA)))
bar_plot

figure_file <- paste(base_folder, "bar_plot.tiff", sep = "")
ggsave(figure_file, plot = bar_plot, dpi = 600, width = 7, height = 8, units = "cm")

