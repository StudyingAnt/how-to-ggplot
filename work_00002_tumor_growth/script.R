# Attach packages
library(readxl)
library(ggplot2)
library(dplyr)
# install.packages("ggpubr")
library(ggpubr)
library(grid)
library(pBrackets)

# read data
base_folder <- "C:/Users/vrept/OneDrive/Desktop/how-to-ggplot/work_00002_tumor_growth/"
data_file <- paste(base_folder, "43018_2023_668_MOESM5_ESM.xlsx", sep = "")
data <- read_excel(data_file, sheet = "Figure 3", range = "B114:V121")

group <- unname(unlist(data[1,as.vector(!is.na(data[1,]))]))
n_replicates <- c(10, 10)

days <- c()
samples <- c()
tumor_volumes <- c()
for (i in 2:7) {
  days <- c(days, rep(unname(unlist(data[i,1])), 20))
  samples <- c(samples, rep(group[1], 10), rep(group[2], 10))
  tumor_volumes <- c(tumor_volumes, 
                     unname(unlist(data[i,2:11])), 
                     unname(unlist(data[i,12:21])))
}

a = c(0.05, 0.01, 1e-3, 1e-4)
m = 3
sidak = 1-(1-a)^(1/m)
sidak

Sidak <- function(vecP)
  #
  # This function corrects a vector of probabilities for multiple testing
  # using the Bonferroni (1935) and Sidak (1967) corrections.
  #
  # References: Bonferroni (1935), Sidak (1967), Wright (1992).
  #
  # Bonferroni, C. E. 1935. Il calcolo delle assicurazioni su gruppi di teste. 
  # Pp. 13-60 in: Studi in onore del Professore Salvatore Ortu Carboni. Roma.
  #
  # Sidak, Z. 1967. Rectangular confidence regions for the means of multivariate 
  # normal distributions. Journal of the American Statistical Association 62:626-633.
#
# Wright, S. P. 1992. Adjusted P-values for simultaneous inference. 
# Biometrics 48: 1005-1013. 
#
#                  Pierre Legendre, May 2007
{
  k = length(vecP)
  vecPB = 0
  vecPS = 0
  for(i in 1:k) {
    bonf = vecP[i]*k
    if(bonf > 1) bonf=1
    vecPB = c(vecPB, bonf)
    vecPS = c(vecPS, (1-(1-vecP[i])^k))
  }
  #
  return(list(OriginalP=vecP, BonfP=vecPB[-1], SidakP=vecPS[-1]))
}


data_formated <- data.frame(
  Day = days,
  Sample = samples,
  Tumor_volume = as.numeric(tumor_volumes)
)
data_formated

anova_test <- summary(aov(Tumor_volume ~ Day + Sample, data=data_formated))

p_vals <- as.vector(na.omit(anova_test[[1]]$`Pr(>F)`))
p_vals
Sidak(p_vals)

anova_test <- anova(lm(Tumor_volume ~ Day * Sample, data_formated))

data_formated <- data_formated %>% 
  add_row(Sample = "DCP", Day = 0, Tumor_volume = 0) %>% 
  add_row(Sample = "DCP-IL-12/FLT3L", Day = 0, Tumor_volume = 0)


data_plot <- data_formated %>% group_by(Sample, Day) %>% summarise(avg = mean(Tumor_volume), sem = stderror(Tumor_volume))

stderror <- function(x) sd(x)/sqrt(length(x))

max_point <- data_plot %>% summarise(max = max(avg))
max_points <- unlist(max_point$max)

signif_bracket <- data.frame(
  X = c(18.5,19,19,18.5),
  Y = rep(c(as.numeric(max_points[1]), as.numeric(max_points[2])), each = 2)
)

ggtext_size <- function(base_size, ratio = 1) {
  ratio * base_size / ggplot2::.pt
}

growth_plot <- ggplot(data_plot, aes(x=Day, y=avg)) + 
  geom_line(aes(x=Day, y=avg, color=Sample)) +
  geom_errorbar(aes(ymin=avg, ymax=avg+sem, color=Sample),
                width = 0.4) +
  geom_point(aes(x=Day, y=avg, shape=Sample, color=Sample), size=2) +
  geom_path(data=signif_bracket, aes(x=X, y=Y)) +
  annotate('text', x = 19.5, y = (as.numeric(max_points[1]) + as.numeric(max_points[2]))/2,  
           label = '<0.0001', 
           size = ggtext_size(6), 
           angle='90') +
  xlab("Days post tumor injection") +ylab(bquote("Tumor volume "(mm^3))) +
  scale_x_continuous(breaks = c(0,5,10,15,20), limits = c(0, 20), expand = c(0,0.055)) +
  scale_y_continuous(breaks = c(0,200,400,600,800), limits = c(0,800), expand = c(0,4)) +
  scale_shape_manual(values = c(16, 15)) +
  scale_color_manual(values = c("black", "red")) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 6),
    axis.title.y = element_text(size = 6),
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 6),
    axis.ticks = element_line(color="black"),
    legend.text = element_text(size = 6),
    legend.title = element_blank(),
    legend.position = c(0.2,0.9),
    legend.key.height = unit(3, 'mm'),
    legend.spacing.y = unit(1, 'mm'),
    axis.line = element_line("black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect("white")
  ) 

figure_file <- paste(base_folder, "plot_final.tiff", sep = "")
ggsave(figure_file, plot = growth_plot, dpi = 600, width = 8, height = 5, units = "cm")
