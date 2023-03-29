# install.packages("ggplot2")
# install.packages("tidyverse")
# install.packages("ggsignif")
# install.packages("RVAideMemoire")
# install.packages("lawstat")
# install.packages("ARTool")

library(ARTool)
library(lawstat)
library(ggplot2) 
library(tidyverse)
library(ggsignif)
library(RVAideMemoire)
library(psych)

###Setting of the Graph###

Setting <- list(theme(plot.margin = unit(c(1, 1, 1, 1), "lines")),
                theme_classic(),
                theme(axis.title.y = element_text(size = 20, vjust = 1)),
                labs(title = "TITLE"), 
                theme(plot.title = element_text(size = 20, colour = "white")),
                theme(axis.text.y = element_text(size = 20, colour = "black")),
                scale_fill_manual(values = c("#f39800", "#aaaaaa")),
                scale_color_manual(values = c("#000000", "#000000")),
                theme(axis.text.x = element_text(size = 15, colour = "white", angle = 45, hjust = 1)),
                theme(axis.title.x = element_blank()),
                theme(legend.position = "none"))



library(ggplot2)
library(ggsignif)

a = c(8, 0)
b = c(2, 8)
c = c(80, 0)
d = c("ATR", "Control")
df1 = data.frame(a, b, c, d)
df1

df_a1 = data.frame(a, b)

gp1 = ggplot(df1, aes(x = d, y = c)) + 
  geom_col(aes(fill = d), colour="black") +
  
  #atr-control
  geom_signif(
    y_position = 90,
    xmin = 1.0, 
    xmax = 2.0, 
    annotation = "p=0.001",
    tip_length = 0.03, 
    size = 0.7, 
    textsize = 6) + 
  
  theme_classic() + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), breaks = seq(0, 100, 25)) +
  labs(y = "Disruption Rate") +
  Setting +
  theme(axis.title.x = element_blank())
gp1

testResult <- fisher.test(df_a1)
testResult


a2 = c(5, 0)
b2 = c(7, 9)
c2 = c(500/12, 0)
d2 = c("ATR", "Control")
df2 = data.frame(a2, b2, c2, d2)
df2

df_a2 = data.frame(a2, b2)

gp2 = ggplot(df2, aes(x = d2, y = c2)) + 
  geom_col(aes(fill = d2), colour="black") +
  
  #atr-control
  geom_signif(
    y_position = 85,
    xmin = 1.0,
    xmax = 2.0, 
    annotation = "p=0.045",
    tip_length = 0.03,
    size = 0.7,
    textsize = 6) + 
  
  theme_classic() + 
  
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), breaks = seq(0, 100, 25)) +
  labs(y = "Disruption Rate") +
  Setting +
  theme(axis.title.x = element_blank())

gp2

testResult <- fisher.test(df_a2)
testResult

ggsave("../figure_R/cp_otp_LED.png", gp1, width = 3, height = 5.5, dpi = 300)
ggsave("../figure_R/cop_opt_laser.png", gp2, width = 3, height = 5.5, dpi = 300)