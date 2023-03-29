# install.packages("ggplot2")
# install.packages("tidyverse")
# install.packages("ggsignif")
# install.packages("RVAideMemoire")
# install.packages("lawstat")
# install.packages("ARTool")
# install.packages("effsize")

library(ARTool)
library(lawstat)
library(ggplot2) 
library(tidyverse)
library(ggsignif)
library(RVAideMemoire)
library("easystats")
library(psych)
library(effsize)

###Setting of the Graph###
Setting <- list(theme_classic(),
                theme(plot.margin = unit(c(1, 1, 1, 1), "lines")),
                theme(axis.title.y = element_text(size = 30, vjust = 1)),
                
                theme(axis.text.y = element_text(size = 30, colour = "black")),
                theme(axis.text.x = element_text(size = 15, colour = "white", vjust = 0.5)),
                theme(axis.line = element_line(colour = "black")),
                scale_fill_manual(values = c("#f39800", "#595857")),
                theme(legend.position = "none"))

Setting2 <- list(theme_classic(),
                theme(plot.margin = unit(c(1, 1, 1, 1), "lines")),
                theme(axis.title.x = element_text(size = 15, vjust = -0.5, color = "white")),
                theme(axis.title.y = element_text(size = 15, vjust = 2, color = "white")),
                theme(axis.text.x = element_text(size = 10, colour = "white", vjust = 0.5)),
                theme(axis.text.y = element_text(size = 10, colour = "white")),
                theme(axis.line = element_line(colour = "white")),
                theme(axis.ticks = element_line(colour = "white")),
                scale_fill_brewer(palette = "Accent"), 
                theme(legend.position = "none"))

Setting3 <- list(theme_classic(),
                theme(plot.margin = unit(c(1, 1, 1, 1), "lines")),
                theme(axis.title.x = element_text(size = 15, vjust = -0.5, color = "black")),
                theme(axis.title.y = element_text(size = 15, vjust = 2, color = "black")),
                theme(axis.text.x = element_text(size = 15, colour = "black", vjust = 0.5)),
                theme(axis.text.y = element_text(size = 15, colour = "black")),
                theme(axis.line = element_line(colour = "black")),
                scale_fill_brewer(palette = "Accent"), 
                theme(legend.position = "none"))


#read to CSV
df_atr <- readr::read_csv("../laser_opt_piezo/created_csv/atr_laser2.csv")
df_etoh <- readr::read_csv("../laser_opt_piezo/created_csv/etoh_laser2.csv")
head(df_atr)
head(df_etoh)

#name dataframe
z = c("../figure_R/", "../figure_R/", "../figure_R/", "../figure_R/", "../figure_R/", "../figure_R/", "../figure_R/", "../figure_R/", "../figure_R/", "../figure_R/", "../figure_R/", "../figure_R/")
a = c("cop_laser_atr_", "cop_laser_atr_", "cop_laser_atr_", "cop_laser_atr_", "cop_laser_atr_", "cop_laser_atr_", "cop_laser_atr_", "cop_laser_atr_", "cop_laser_atr_", "cop_laser_atr_", "cop_laser_atr_", "cop_laser_atr_")
b = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
c = c(".png", ".png", ".png", ".png", ".png", ".png", ".png", ".png", ".png", ".png", ".png", ".png")
d = c("scale", "scale", "scale", "scale", "scale", "scale", "scale", "scale", "scale", "scale", "scale", "scale")
df1 = data.frame(z, a, b, c) %>%
  unite(col = "name", c(z, a, b, c))
df2 = data.frame(z, a, b, d, c) %>%
  unite(col = "name", c(z, a, b, d, c))
head(df1)

#draw graph
n = 1
for (i in 1:12) {
  df = df_atr%>%
    filter(Class == n)   
  head(df)
  gp = ggplot() +
    geom_line(data = df, aes(x = frame, y = length, group = Class))+
    scale_y_continuous(limits = c(0, 11), expand = c(0, 0), breaks=seq(0, 11, 1)) +
    scale_x_continuous(limits = c(0, 1200), expand = c(0, 0), breaks=seq(0, 1200, 30)) +
    # geom_rect(aes(xmin = 300, xmax = 1200, ymin = -Inf, ymax = Inf), fill = "#99cc33", alpha = 0.3) +
    Setting2
  print(df1[n,])
  ggsave(df1[n,], gp, width = 11, height = 4, dpi = 300)
  n = n + 1
}

n = 1
for (i in 1:12) {
  df = df_atr%>%
    filter(Class == n)   
  head(df)
  gp = ggplot() +
    geom_line(data = df, aes(x = frame, y = length, group = Class))+
    scale_y_continuous(limits = c(0, 11), expand = c(0, 0), breaks=seq(0, 11, 1)) +
    scale_x_continuous(limits = c(0, 1200), expand = c(0, 0), breaks=seq(0, 1200, 30)) +
    geom_rect(aes(xmin = 300, xmax = 1200, ymin = -Inf, ymax = Inf), fill = "#99cc33", alpha = 0.3) +
    Setting3
  print(df2[n,])
  ggsave(df2[n,], gp, width = 11, height = 4, dpi = 300)
  n = n + 1
}

#name dataframe
z2 = c("../figure_R/", "../figure_R/", "../figure_R/", "../figure_R/", "../figure_R/", "../figure_R/", "../figure_R/", "../figure_R/", "../figure_R/")
a2 = c("cop_laser_etoh_", "cop_laser_etoh_", "cop_laser_etoh_", "cop_laser_etoh_", "cop_laser_etoh_", "cop_laser_etoh_", "cop_laser_etoh_", "cop_laser_etoh_", "cop_laser_etoh_")
b2 = c(1, 2, 3, 4, 5, 6, 7, 8, 9)
c2 = c(".png", ".png", ".png", ".png", ".png", ".png", ".png", ".png", ".png")
d2 = c("scale", "scale", "scale", "scale", "scale", "scale", "scale", "scale", "scale")
df3 = data.frame(z2, a2, b2, c2) %>%
  unite(col = "name", c(z2, a2, b2, c2))
df4 = data.frame(z2, a2, b2, d2, c2) %>%
  unite(col = "name", c(z2, a2, b2, d2, c2))
head(df3)

n = 1
for (i in 1:9) {
  df = df_etoh%>%
    filter(Class == n)   
  head(df)
  gp = ggplot() +
    geom_line(data = df, aes(x = frame, y = length, group = Class))+
    scale_y_continuous(limits = c(0, 11), expand = c(0, 0), breaks=seq(0, 11, 1)) +
    scale_x_continuous(limits = c(0, 1200), expand = c(0, 0), breaks=seq(0, 1200, 30)) +
    # geom_rect(aes(xmin = 300, xmax = 1200, ymin = -Inf, ymax = Inf), fill = "#99cc33", alpha = 0.3) +
    Setting2
  print(df3[n,])
  ggsave(df3[n,], gp, width = 11, height = 4, dpi = 300)
  n = n + 1
}

n = 1
for (i in 1:9) {
  df = df_etoh%>%
    filter(Class == n)   
  head(df)
  gp = ggplot() +
    geom_line(data = df, aes(x = frame, y = length, group = Class))+
    scale_y_continuous(limits = c(0, 11), expand = c(0, 0), breaks=seq(0, 11, 1)) +
    scale_x_continuous(limits = c(0, 1200), expand = c(0, 0), breaks=seq(0, 1200, 30)) +
    geom_rect(aes(xmin = 300, xmax = 1200, ymin = -Inf, ymax = Inf), fill = "#99cc33", alpha = 0.3) +
    Setting3
  print(df4[n,])
  ggsave(df4[n,], gp, width = 11, height = 4, dpi = 300)
  n = n + 1
}


#read to CSV 30 sec
df_atr_30sec <- readr::read_csv("../laser_opt_piezo/created_csv/atr_laser_30sec.csv")
df_etoh_30sec <- readr::read_csv("../laser_opt_piezo/created_csv/etoh_laser_30sec.csv")
head(df_atr_30sec)
head(df_etoh_30sec)
df_30sec = rbind(df_atr_30sec, df_etoh_30sec)
df_30sec

describe(df_atr_30sec)
describe(df_etoh_30sec)

gp5 = ggplot(data = df_30sec, aes(x = Experiment, y = len_sec))+
  geom_boxplot(aes(fill = Experiment), outlier.shape = NA)+
  geom_jitter(size = 4, width =0.1) +
  
  
  #atr-etoh
  geom_signif(
    y_position = 85,
    xmin = 1.0,
    xmax = 2.0, 
    annotation = "p=0.026\nd=0.711",
    tip_length = 0.01, 
    size = 0.7,
    textsize = 9) + 
  
  
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20), expand = c(0, 0)) +
  theme_classic() +
  labs(y = "The average displacement (px/sec)") +
  Setting +
  theme(axis.title.x = element_blank())

gp5
ggsave("../figure_R/laser_cop_length_sec.png", gp5, width = 5, height = 7, dpi = 300)

sw_test_results = df_30sec %>% 
  group_by(Experiment) %>% 
  summarise(statistic = shapiro.test(len_sec)$statistic, 
            p.value = shapiro.test(len_sec)$p.value,
            .groups     = "drop") 
head(sw_test_results)

var.test(data = df_30sec, len_sec ~ Experiment)


library(lawstat)
report(brunner.munzel.test(df_atr_30sec$len_sec, df_etoh_30sec$len_sec))
a = brunner.munzel.test(df_atr_30sec$len_sec, df_etoh_30sec$len_sec)
a
cohen.d(df_atr_30sec$len_sec, df_etoh_30sec$len_sec, hedges.correction = FALSE)

#read to CSV b10 sec
df_atr_b10sec <- readr::read_csv("../laser_opt_piezo/created_csv/atr_laser_b10sec.csv")
df_etoh_b10sec <- readr::read_csv("../laser_opt_piezo/created_csv/etoh_laser_b10sec.csv")
head(df_atr_b10sec)
head(df_etoh_b10sec)
df_b10sec = rbind(df_atr_b10sec, df_etoh_b10sec)
df_b10sec

gp6 = ggplot(data = df_b10sec, aes(x = Experiment, y = len_sec))+
  geom_boxplot(aes(fill = Experiment), outlier.shape = NA)+
  geom_jitter(size = 1, width =0.1) +

  #atr-etoh
  geom_signif(
  y_position = 0.7,
  xmin = 1.0,
  xmax = 2.0,
  annotation = "p=0.804\nd=-0.010",
  tip_length = 0.03,
  size = 0.3,
  textsize = 5) + 
  
  
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25), expand = c(0, 0)) +
  theme_classic() +
  labs(y = "The average displacement (px/sec)") +
  Setting +
  theme(axis.title.x = element_blank())
gp6

sw_test_results = df_b10sec %>%  
  group_by(Experiment) %>% 
  summarise(statistic = shapiro.test(len_sec)$statistic, 
            p.value = shapiro.test(len_sec)$p.value, 
            .groups     = "drop") 
head(sw_test_results)

var.test(data = df_b10sec, len_sec ~ Experiment)

library(lawstat)
report(brunner.munzel.test(df_atr_b10sec$len_sec, df_etoh_b10sec$len_sec))
brunner.munzel.test(df_atr_b10sec$len_sec, df_etoh_b10sec$len_sec)

cohen.d(df_atr_b10sec$len_sec, df_etoh_b10sec$len_sec, hedges.correction = FALSE)