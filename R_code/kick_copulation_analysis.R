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
df_csh <- readr::read_csv("female_kick_copulation_csh.csv")
df_piezoKO <- readr::read_csv("female_kick_copulation_homo.csv")
df_pi = rbind(df_csh, df_piezoKO)
head(df_csh)
head(df_piezoKO)
df_pi <- transform(df_pi, genotype = factor(genotype, levels = c("homo", "csh")))



describe(df_piezoKO)
describe(df_csh)

gp5 = ggplot(data = df_pi, aes(x = genotype, y = ratio_of_kick))+
  geom_boxplot(aes(fill = genotype), outlier.shape = NA)+
  geom_jitter(size = 4, width =0.1) +
  
  
  #atr-etoh
  geom_signif(
    y_position = 0.83,
    xmin = 1.0,
    xmax = 2.0, 
    annotation = "p=0.071?nd=-0.662",
    tip_length = 0.03, 
    size = 0.7,
    textsize = 9) + 
  
  
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), expand = c(0, 0)) +
  theme_classic() +
  labs(y = "Female Kick Ratio") +
  scale_x_discrete(labels = c('homo' = lab_homo,'csh' = lab_csh)) +
  Setting +
  theme(axis.title.x = element_blank())

gp5
ggsave("female_kick_count.png", gp5, width = 5, height = 7, dpi = 300)

sw_test_results = df_pi %>% 
  group_by(genotype) %>% 
  summarise(statistic = shapiro.test(ratio_of_kick)$statistic, 
            p.value = shapiro.test(ratio_of_kick)$p.value,
            .groups     = "drop") 
head(sw_test_results)

var.test(data = df_pi, ratio_of_kick?genotype)


library(lawstat)
report(t.test(df_piezoKO$ratio_of_kick, df_csh$ratio_of_kick,var.equal=T,paired=F))
a = t.test(df_piezoKO$ratio_of_kick, df_csh$ratio_of_kick,var.equal=T,paired=F)
a

power.t.test(power=0.8, delta=0.1, type="paired")


cohen.d(df_piezoKO$ratio_of_kick, df_csh$ratio_of_kick, hedges.correction = FALSE)

