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
                theme(axis.title.y = element_text(size = 25, vjust = 2)),
                labs(title = "TITLE"), 
                theme(plot.title = element_text(size = 25, colour = "white")),
                theme(axis.text.y = element_text(size = 25, colour = "black")),
                theme(axis.line = element_line(colour = "black")),
                scale_fill_manual(values = c("#f39800", "#aaaaaa")),
                theme(axis.text.x = element_text(size = 25, colour = "black", angle = 45, hjust = 1)),
                theme(axis.title.x = element_blank()),
                theme(legend.position = "none"))

#label
lab_homo = expression(paste(italic({Piezo ^ {"KO"}})))
lab_csh = "Canton-S"

#read to CSV
df_csh <- readr::read_csv("genital_touch_count_csh.csv")
df_piezoKO <- readr::read_csv("genital_touch_count_homo.csv")
df_pi = rbind(df_csh, df_piezoKO)
head(df_csh)
head(df_piezoKO)
df_pi <- transform(df_pi, genotype = factor(genotype, levels = c("homo", "csh")))



describe(df_piezoKO)
describe(df_csh)

gp5 = ggplot(data = df_pi, aes(x = genotype, y = count_min))+
  geom_boxplot(aes(fill = genotype), outlier.shape = NA)+
  geom_jitter(size = 4, width =0.1) +
  
  
  #atr-etoh
  geom_signif(
    y_position = 3.2,
    xmin = 1.0,
    xmax = 2.0, 
    annotation = "p=0.762?nd=0.096",
    tip_length = 0.05, 
    size = 0.7,
    textsize = 9) + 
  
  
  scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, 1), expand = c(0, 0)) +
  theme_classic() +
  labs(y = "Genital Touch (/min)") +
  scale_x_discrete(labels = c('homo' = lab_homo,'csh' = lab_csh)) +
  Setting +
  theme(axis.title.x = element_blank())

gp5
ggsave("fly_genital_touch_count.png", gp5, width = 5, height = 7, dpi = 300)

sw_test_results = df_pi %>% 
  group_by(genotype) %>% 
  summarise(statistic = shapiro.test(count_min)$statistic, 
            p.value = shapiro.test(count_min)$p.value,
            .groups     = "drop") 
head(sw_test_results)

var.test(data = df_pi, count_min?genotype)


library(lawstat)
report(t.test(df_piezoKO$count_min, df_csh$count_min))
a = t.test(df_piezoKO$count_min, df_csh$count_min)
a

power.t.test(power=0.8, delta=0.1, type="paired")


cohen.d(df_piezoKO$count_min, df_csh$count_min, hedges.correction = FALSE)

