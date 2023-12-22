# 패키지 부착 Attach packages
library(readxl)
library(ggplot2)
library(dplyr)

# 데이터 읽기 read data
base_folder <- "C:/Users/vrept/OneDrive/Desktop/how-to-ggplot/work_00002_tumor_growth/"
data_file <- paste(base_folder, "43018_2023_668_MOESM5_ESM.xlsx", sep = "")
data <- read_excel(data_file, sheet = "Figure 3", range = "B114:V121")
data

# 데이터 포맷 변경
group <- unname(unlist(data[1,as.vector(!is.na(data[1,]))]))

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

data_reformat <- data.frame(
  Day = days,
  Sample = samples,
  Tumor_volume = as.numeric(tumor_volumes)
)
data_reformat

# 통계 분석 Statistical analysis
# 논문토대로 Based on the paper
# two-way ANOVA with Sidak correction

# 시닥 교정을 위한 함수 Function for Sidak correction
Sidak <- function(vecP) {
  k = length(vecP)
  vecPB = 0
  vecPS = 0
  for(i in 1:k) {
    bonf = vecP[i]*k
    if(bonf > 1) bonf=1
    vecPB = c(vecPB, bonf)
    vecPS = c(vecPS, (1-(1-vecP[i])^k))
  }
  return(list(OriginalP=vecP, BonfP=vecPB[-1], SidakP=vecPS[-1]))
}

anova_test <- summary(aov(Tumor_volume ~ Day + Sample, data=data_reformat))
p_vals <- as.vector(na.omit(anova_test[[1]]$`Pr(>F)`))
Sidak(p_vals)

# 데이터에 영점 추가 Add time point zero into data
data_reformat <- data_reformat %>% 
  add_row(Sample = "DCP", Day = 0, Tumor_volume = 0) %>% 
  add_row(Sample = "DCP-IL-12/FLT3L", Day = 0, Tumor_volume = 0)

# 표준오차 함수 Standard error fuction
stderror <- function(x) sd(x)/sqrt(length(x))

# 평균, 표준오차 계산 Compute mean and standard error
data_plot <- data_reformat %>% 
  group_by(Sample, Day) %>% 
  summarise(avg = mean(Tumor_volume), sem = stderror(Tumor_volume))

# 그래프 그리기 Draw plot
# 기본 선 그래프 Basic line graph
ggplot(data_plot, aes(x=Day, y=avg)) + 
  geom_line(aes(x=Day, y=avg, color=Sample))

# 에러바 추가 Add error bar
ggplot(data_plot, aes(x=Day, y=avg)) + 
  geom_line(aes(x=Day, y=avg, color=Sample)) +
  geom_errorbar(aes(ymin=avg, ymax=avg+sem, color=Sample),
                width = 0.4)

# 관측점 추가 Add data points
ggplot(data_plot, aes(x=Day, y=avg)) + 
  geom_line(aes(x=Day, y=avg, color=Sample)) +
  geom_errorbar(aes(ymin=avg, ymax=avg+sem, color=Sample),
                width = 0.4) +
  geom_point(aes(x=Day, y=avg, shape=Sample, color=Sample), size=2)

# 축 제목 변경 Modify axis title
ggplot(data_plot, aes(x=Day, y=avg)) + 
  geom_line(aes(x=Day, y=avg, color=Sample)) +
  geom_errorbar(aes(ymin=avg, ymax=avg+sem, color=Sample),
                width = 0.4) +
  geom_point(aes(x=Day, y=avg, shape=Sample, color=Sample), size=2)+
  xlab("Days post tumor injection") + 
  ylab(bquote("Tumor volume "(mm^3))) 

# 축 한계 고정 Fix axis limits
ggplot(data_plot, aes(x=Day, y=avg)) + 
  geom_line(aes(x=Day, y=avg, color=Sample)) +
  geom_errorbar(aes(ymin=avg, ymax=avg+sem, color=Sample),
                width = 0.4) +
  geom_point(aes(x=Day, y=avg, shape=Sample, color=Sample), size=2)+
  xlab("Days post tumor injection") + 
  ylab(bquote("Tumor volume "(mm^3))) +
  scale_x_continuous(limits = c(0, 20)) +
  scale_y_continuous(limits = c(0,800)) 

# 축 눈금 위치 설정 Set axis tick points
ggplot(data_plot, aes(x=Day, y=avg)) + 
  geom_line(aes(x=Day, y=avg, color=Sample)) +
  geom_errorbar(aes(ymin=avg, ymax=avg+sem, color=Sample),
                width = 0.4) +
  geom_point(aes(x=Day, y=avg, shape=Sample, color=Sample), size=2)+
  xlab("Days post tumor injection") + 
  ylab(bquote("Tumor volume "(mm^3))) +
  scale_x_continuous(breaks = c(0,5,10,15,20), 
                     limits = c(0, 20)) +
  scale_y_continuous(breaks = c(0,200,400,600,800), 
                     limits = c(0,800)) 

# 축 확장 변경 Modify axix expand
ggplot(data_plot, aes(x=Day, y=avg)) + 
  geom_line(aes(x=Day, y=avg, color=Sample)) +
  geom_errorbar(aes(ymin=avg, ymax=avg+sem, color=Sample),
                width = 0.4) +
  geom_point(aes(x=Day, y=avg, shape=Sample, color=Sample), size=2)+
  xlab("Days post tumor injection") + 
  ylab(bquote("Tumor volume "(mm^3))) +
  scale_x_continuous(breaks = c(0,5,10,15,20), 
                     limits = c(0, 20), 
                     expand = c(0,1)) +
  scale_y_continuous(breaks = c(0,200,400,600,800), 
                     limits = c(0,800), 
                     expand = c(0,10)) 

# 점 모양 변경 Change point shape
ggplot(data_plot, aes(x=Day, y=avg)) + 
  geom_line(aes(x=Day, y=avg, color=Sample)) +
  geom_errorbar(aes(ymin=avg, ymax=avg+sem, color=Sample),
                width = 0.4) +
  geom_point(aes(x=Day, y=avg, shape=Sample, color=Sample), size=2)+
  xlab("Days post tumor injection") + 
  ylab(bquote("Tumor volume "(mm^3))) +
  scale_x_continuous(breaks = c(0,5,10,15,20), 
                     limits = c(0, 20), 
                     expand = c(0,1)) +
  scale_y_continuous(breaks = c(0,200,400,600,800), 
                     limits = c(0,800), 
                     expand = c(0,10)) +
  scale_shape_manual(values = c(16, 15)) 

# 색 변경 Change color
ggplot(data_plot, aes(x=Day, y=avg)) + 
  geom_line(aes(x=Day, y=avg, color=Sample)) +
  geom_errorbar(aes(ymin=avg, ymax=avg+sem, color=Sample),
                width = 0.4) +
  geom_point(aes(x=Day, y=avg, shape=Sample, color=Sample), size=2)+
  xlab("Days post tumor injection") + 
  ylab(bquote("Tumor volume "(mm^3))) +
  scale_x_continuous(breaks = c(0,5,10,15,20), 
                     limits = c(0, 20), 
                     expand = c(0,1)) +
  scale_y_continuous(breaks = c(0,200,400,600,800), 
                     limits = c(0,800), 
                     expand = c(0,10)) +
  scale_shape_manual(values = c(16, 15)) +
  scale_color_manual(values = c("black", "red"))

# 테마 디테일 수정 Modify theme details
ggplot(data_plot, aes(x=Day, y=avg)) + 
  geom_line(aes(x=Day, y=avg, color=Sample)) +
  geom_errorbar(aes(ymin=avg, ymax=avg+sem, color=Sample),
                width = 0.4) +
  geom_point(aes(x=Day, y=avg, shape=Sample, color=Sample), size=2)+
  xlab("Days post tumor injection") + 
  ylab(bquote("Tumor volume "(mm^3))) +
  scale_x_continuous(breaks = c(0,5,10,15,20), 
                     limits = c(0, 20), 
                     expand = c(0,1)) +
  scale_y_continuous(breaks = c(0,200,400,600,800), 
                     limits = c(0,800), 
                     expand = c(0,10)) +
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


# 유의미도 꺽쇠 만들기 Make bracket for significance
max_point <- data_plot %>% summarise(max = max(avg))
max_points <- unlist(max_point$max)
signif_bracket <- data.frame(
  X = c(18.5,19,19,18.5),
  Y = rep(c(as.numeric(max_points[1]), as.numeric(max_points[2])), each = 2)
)
y_annot <- (as.numeric(max_points[1])+as.numeric(max_points[2]))/2

# 주석 폰트 크기기 보정 Annotation font size correction
ggtext_size <- function(base_size, ratio = 1) {
  ratio * base_size / ggplot2::.pt
}

# 그래프 그리기 Draw plot
ggplot(data_plot, aes(x=Day, y=avg)) + 
  geom_line(aes(x=Day, y=avg, color=Sample)) +
  geom_errorbar(aes(ymin=avg, ymax=avg+sem, color=Sample),
                width = 0.4) +
  geom_point(aes(x=Day, y=avg, shape=Sample, color=Sample), size=2) +
  geom_path(data=signif_bracket, aes(x=X, y=Y)) +
  annotate('text', x = 19.5, y = y_annot,  
           label = '<0.0001', 
           size = ggtext_size(6), 
           angle='90') +
  xlab("Days post tumor injection") + 
  ylab(bquote("Tumor volume "(mm^3))) +
  scale_x_continuous(breaks = c(0,5,10,15,20), 
                     limits = c(0, 20), 
                     expand = c(0,0.055)) +
  scale_y_continuous(breaks = c(0,200,400,600,800), 
                     limits = c(0,800), 
                     expand = c(0,4)) +
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

# 그래프 변수로 저장 Save graph into variable
growth_plot <- ggplot(data_plot, aes(x=Day, y=avg)) + 
  geom_line(aes(x=Day, y=avg, color=Sample)) +
  geom_errorbar(aes(ymin=avg, ymax=avg+sem, color=Sample),
                width = 0.4) +
  geom_point(aes(x=Day, y=avg, shape=Sample, color=Sample), size=2) +
  geom_path(data=signif_bracket, aes(x=X, y=Y)) +
  annotate('text', x = 19.5, y = y_annot,  
           label = '<0.0001', 
           size = ggtext_size(6), 
           angle='90') +
  xlab("Days post tumor injection") + 
  ylab(bquote("Tumor volume "(mm^3))) +
  scale_x_continuous(breaks = c(0,5,10,15,20), 
                     limits = c(0, 20), 
                     expand = c(0,0.055)) +
  scale_y_continuous(breaks = c(0,200,400,600,800), 
                     limits = c(0,800), 
                     expand = c(0,4)) +
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

# 그래프 TIFF 저장 Save graph to TIFF file
figure_file <- paste(base_folder, "plot_final.tiff", sep = "")
ggsave(figure_file, plot = growth_plot, 
       dpi = 600, width = 8, height = 5, units = "cm")
