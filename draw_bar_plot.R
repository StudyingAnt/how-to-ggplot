# Attach packages
library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)

# Read data
data_file <- "C:/Users/CEEL-PC-005/Desktop/Joon/how-to-ggplot/test_data_1.xlsx" 
data <- read_excel(data_file, sheet = "Sheet1", range = "A1:B10")

data$Condition <- factor(data$Condition, levels = c("Ctrl", "A drug", "B drug"))
data_summary <- data %>% group_by(Condition) %>% summarise(avg = mean(Gene_expression), std = sd(Gene_expression))
data_summary
bar_plot <- ggplot(data_summary, aes(x=Condition, y=avg, fill=Condition)) + 
  geom_bar(stat = "identity", color = "black") +
  geom_jitter(data=data, aes(x=Condition, y=Gene_expression), width = 0.2) +
  geom_errorbar(aes(ymin=avg-std, ymax=avg+std),
                width = 0.2) +
  xlab("Treatment") +
  ylab("Relative Gene Expression") + 
  scale_y_continuous(limits = c(0,2), expand = c(0,0)) +
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

figure_file <- "C:/Users/CEEL-PC-005/Desktop/Joon/how-to-ggplot/bar_plot.png"
ggsave(figure_file, plot = bar_plot, dpi = 600, width = 7, height = 8, units = "cm")

