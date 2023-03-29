# install.packages("ggplot2")
# install.packages("tidyverse")
# install.packages("ggsignif")
# install.packages("RVAideMemoire")
# install.packages("lawstat")
# install.packages("ARTool")
# install.packages("emmeans")
# install.packages("rcompanion")
# install.packages("psych")

library(ARTool)
library(lawstat)
library(ggplot2) 
library(tidyverse)
library(ggsignif)
library(RVAideMemoire)
library(emmeans)
library(rcompanion)
library(psych)
library(car)

###Setting of the Graph###
#label
lab_homo = expression(paste(italic({piezo ^ {"KO"}})))
print(lab_homo)
lab_hetero = expression(paste(italic({piezo ^ KO/"+"})))
lab_csh = "Canton-S"

Setting <- list(theme(plot.margin = unit(c(1, 1, 1, 1), "lines")),
                theme_classic(),
                scale_fill_manual(values = c("#f39800", "#aaaaaa", "#aaaaaa")),
                scale_color_manual(values = c("#000000", "#000000", "#000000")),
                theme(axis.title.y = element_text(size = 20, vjust = 2)),
                labs(title = "TITLE"), 
                theme(plot.title = element_text(size = 20, colour = "white")),
                theme(axis.text.y = element_text(size = 20, colour = "black")),
                scale_x_discrete(labels = c('homo' = lab_homo, 'hetero' = lab_hetero, 'csh' = lab_csh)),
                theme(axis.text.x = element_text(size = 20, colour = "black", angle = 45, hjust = 1)),
                theme(axis.title.x = element_blank()),
                theme(legend.position = "none"))

#read to CSV
df_cop <- readr::read_csv("csv/df_angle.csv")
head(df_cop)
#Changing label weights
df_cop <- transform(df_cop, genotype = factor(genotype, levels = c("homo", "hetero", "csh")))


###copulatin angle ratio###
gp2 = ggplot(data = df_cop) +
  stat_qq(aes(sample = ratio, color = genotype)) + 
  facet_wrap( ~ genotype, scales = "free")
gp2

sw_test_results = df_cop %>%  
  group_by(genotype) %>% 
  summarise(statistic = shapiro.test(ratio)$statistic, 
            p.value = shapiro.test(ratio)$p.value,
            .groups     = "drop")
head(sw_test_results)

bartlett.test(data = df_cop, ratio~genotype)

df_homo_ana = df_cop[df_cop$genotype=="homo",]
df_hetero_ana = df_cop[df_cop$genotype=="hetero",]
df_csh_ana = df_cop[df_cop$genotype=="csh",]

describe(df_homo_ana)
describe(df_hetero_ana)
describe(df_csh_ana)


#ART one-way ANOVA
df_1 = df_cop %>%                    
  select(genotype, ratio)        
head(df_1) 

model = art(ratio ~ genotype, data = df_1)
summary(model)
result = anova(model)
print(result, verbose = TRUE)
model.lm = artlm(model, "genotype")
marginal = emmeans(model.lm, ~ genotype)
pairs(marginal, adjust = "BH")
Sum = as.data.frame(marginal)
Sum
sum_art = summary(pairs(marginal, adjust = "BH"))
sum_art$d = sum_art$estimate /sigmaHat(model.lm)
sum_art
result$part.eta.sq = with(result,`Sum Sq`/(`Sum Sq` +`Sum Sq.res`))
result
summary(marginal)

gp = ggplot(data = df_cop, aes(x = genotype, y = ratio)) +
  geom_boxplot(aes(fill = genotype), outlier.shape = NA) +
  geom_jitter(size = 2, width =0.1) +
  
  
  #homo-csh
  geom_signif(
    y_position = 0.85,
    xmin = 1.1,
    xmax = 2.9,
    annotation = "p=0.022\nd=0.398",
    tip_length = 0.03,
    size = 0.5,
    textsize = 6) + 
  
  #hetero-homo
  geom_signif(
    y_position = 0.70,
    xmin = 1.1, 
    xmax = 1.9, 
    annotation = "p=0.022\nd=0.448",
    tip_length = 0.03,
    size = 0.5,
    textsize = 6) + 
  
  #hetero-csh
  geom_signif(
    y_position = 0.70,
    xmin = 2.1,
    xmax = 2.9,
    annotation = "p=0.763\nd=-0.050",
    tip_length = 0.03,
    size = 0.5,
    textsize = 6) + 
  
  
  #geom_dotplot(aes(x = genotype, y = ratio, fill = genotype), binaxis = "y", stackdir = "center", binwidth = 0.01) +
  #changing the x orders
  scale_x_discrete(labels = c('piezoKO' = lab_homo, 'piezoKOhetero' = lab_hetero, 'CS-H' = lab_csh)) +
  #change the color pallete
  scale_fill_brewer(palette = "Accent") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25), expand = c(0, 0)) +
  theme_classic() +
  labs(y = "Tilted Copulation Ratio") +
  Setting +
  theme(axis.title.x = element_blank())
gp

ggsave("../figure_R/cop_ratio_graph.png", gp, width = 5, height = 6, dpi = 300)



###variance###

gp4 = ggplot(data = df_cop) +
  stat_qq(aes(sample = bunsan, color = genotype)) + 
  facet_wrap( ~ genotype, scales = "free")
gp4

sw_test_results = df_cop %>% 
  group_by(genotype) %>% 
  summarise(statistic = shapiro.test(bunsan)$statistic, 
            p.value = shapiro.test(bunsan)$p.value, 
            .groups     = "drop") 
head(sw_test_results)

bartlett.test(data = df_cop, bunsan~genotype)

#ART one-way ANOVA
df_2 = df_cop %>%                    
  select(genotype, bunsan)        
head(df_2) 
library(emmeans)
model = art(bunsan ~ genotype, data = df_2)
summary(model)
result = anova(model)
result
model.lm = artlm(model, "genotype")
marginal = emmeans(model.lm, ~ genotype)
pairs(marginal, adjust = "BH")
Sum = as.data.frame(marginal)
Sum
sum_art = summary(pairs(marginal, adjust = "BH"))
sum_art$d = sum_art$estimate /sigmaHat(model.lm)
sum_art

result$part.eta.sq = with(result,`Sum Sq`/(`Sum Sq` +`Sum Sq.res`))
result


gp3 = ggplot(data = df_cop, aes(x = genotype, y = bunsan)) +
  geom_boxplot(aes(fill = genotype), outlier.shape = NA) +
  geom_jitter(size = 2, width =0.1) +
  
  
  #homo-csh
  geom_signif(
    y_position = 434,
    xmin = 1.1, 
    xmax = 2.9,
    annotation = "p=0.045\nd=0.354",
    tip_length = 0.03,
    size = 0.5,
    textsize = 6) +
  
  #hetero-homo
  geom_signif(
    y_position = 358,
    xmin = 1.1, 
    xmax = 1.9,
    annotation = "p=0.009\nd=0.505",
    tip_length = 0.03,
    size = 0.5,
    textsize = 6) +
  
  #hetero-homo
  geom_signif(
    y_position = 358,
    xmin = 2.1, 
    xmax = 2.9, 
    annotation = "p=0.3677\nd=-0.151",
    tip_length = 0.03,
    size = 0.5,
    textsize = 6) +
  
  scale_y_continuous(limits = c(0, 500), breaks = seq(0, 500, 100), expand = c(0, 0)) +
  theme_classic() +
  labs(y = "Variance") +
  Setting 
gp3

ggsave("../figure_R/cop_variance_graph.png", gp3, width = 5, height = 6, dpi = 300)


###DLC_time###

gp2 = ggplot(data = df_cop) +
  stat_qq(aes(sample = cop_mtime, color = genotype)) + 
  facet_wrap( ~ genotype, scales = "free")
gp2

sw_test_results = df_cop %>%
  group_by(genotype) %>%  
  summarise(statistic = shapiro.test(cop_mtime)$statistic, 
            p.value = shapiro.test(cop_mtime)$p.value,
            .groups     = "drop")
head(sw_test_results)

bartlett.test(data = df_cop, cop_mtime~genotype)


#ART one-way ANOVA
df_3 = df_cop %>%                    
  select(genotype, cop_mtime)        
head(df_3) 
library(emmeans)
model = art(cop_mtime ~ genotype, data = df_3)
summary(model)
result = anova(model)
result
model.lm = artlm(model, "genotype")
marginal = emmeans(model.lm, ~ genotype)
pairs(marginal, adjust = "BH")
Sum = as.data.frame(marginal)
sum_art = summary(pairs(marginal, adjust = "BH"))
sum_art$d = sum_art$estimate /sigmaHat(model.lm)
sum_art

result$part.eta.sq = with(result,`Sum Sq`/(`Sum Sq` +`Sum Sq.res`))
result



gp = ggplot(data = df_cop, aes(x = genotype, y = cop_mtime)) +
  geom_boxplot(aes(fill = genotype), outlier.shape = NA) +
  geom_jitter(size = 2, width =0.1) +
  
  
  #homo-csh
  geom_signif(
    y_position = 43.2,
    xmin = 1.1,
    xmax = 2.9,
    annotation = "p=0.0005\nd=-0.641",
    tip_length = 0.03,
    size = 0.3,
    textsize = 6) + 
  
  #hetero-homo
  geom_signif(
    y_position = 35,
    xmin = 1.1,
    xmax = 1.9,
    annotation = "p=0.277\nd=0.184",
    tip_length = 0.03,
    size = 0.3,
    textsize = 6) + 
  
  #hetero-csh
  geom_signif(
    y_position = 35,
    xmin = 2.1, 
    xmax = 2.9,
    annotation = "p=0.008\nd=-0.457",
    tip_length = 0.03,
    size = 0.3,
    textsize = 6) +
  
  
  # scale_y_continuous(limits = c(0, 43)) +
  scale_y_continuous( limits = c( 0, 50 ), breaks = seq(0, 50, 10) , expand = c(0, 0)) +
  theme_classic() +
  labs(y = "DLC duration (min)") +
  Setting

gp

ggsave("../figure_R/cop_DLC_time_graph.png", gp, width = 4, height = 6, dpi = 300)

