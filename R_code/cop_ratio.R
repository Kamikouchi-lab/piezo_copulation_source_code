# install.packages("ggplot2")
# install.packages("tidyverse")
# install.packages("ggsignif")
# install.packages("RVAideMemoire")
# install.packages("lawstat")
# install.packages("ARTool")
# install.packages("rcompanion")

library(ARTool)
library(lawstat)
library(ggplot2) 
library(tidyverse)
library(ggsignif)
library(RVAideMemoire)
library(psych)

###Setting of the Graph###
#label
lab_homo = expression(paste(italic({piezo ^ {"KO"}})))
print(lab_homo)
lab_hetero = expression(paste(italic({piezo ^ KO/"+"})))
lab_csh = "Canton-S"

Setting <- list(theme(plot.margin = unit(c(1, 1, 1, 1), "lines")),
                theme_classic(),
                theme(axis.title.y = element_text(size = 20, vjust = 2)),
                labs(title = "TITLE"), 
                theme(plot.title = element_text(size = 20, colour = "white")),
                theme(axis.text.y = element_text(size = 20, colour = "black")),
                scale_fill_manual(values = c("#f39800", "#aaaaaa", "#aaaaaa")),
                scale_color_manual(values = c("#000000", "#000000", "#000000")),
                scale_x_discrete(labels = c('homo' = lab_homo, 'hetero' = lab_hetero, 'csh' = lab_csh)),
                theme(axis.text.x = element_text(size = 20, colour = "black", angle = 45, hjust = 1)),
                theme(axis.title.x = element_blank()),
                theme(legend.position = "none"))





###read CSV###
df_piezo_cop <- readr::read_csv("csv/piezo_copulation_info.csv")
head(df_piezo_cop)
#Changing label weights
df_piezo_cop <- transform(df_piezo_cop, genotype = factor(genotype, levels = c("homo", "hetero", "csh")))

####Calculation of copulation rate ####

#Count copulation
result_1 = df_piezo_cop %>%
  filter(cop_or_not == 1) %>%
  group_by(genotype) %>%
  count(genotype)
#Count　not copulation
result_0 = df_piezo_cop %>%
  filter(cop_or_not == 0) %>%
  group_by(genotype) %>%
  count(genotype)

#Calculate copulation rate
result = left_join(result_1, result_0, by = "genotype") %>%
  mutate(cop_rat = n.x / (n.x + n.y) * 100)

#Convert data frame to matrix
fish_result <- result %>%
  ungroup() %>%
  select(n.x, n.y) %>%
  as.matrix() %>%
  t()
print(fish_result)

#Fisher's test
fisher.test(fish_result)
fisher.multcomp(fish_result, p.method = "BH" )
gp = ggplot(data =result, aes(x = genotype, y = cop_rat)) +
  geom_col(aes(fill = genotype), colour="black") +

  #homo-csh
  geom_signif(
    y_position = 110,
    xmin = 1.1,
    xmax = 2.9,
    annotation = "p=0.554",
    tip_length = 0.1,
    size = 0.5,
    textsize = 6) +

  #hetero-homo
  geom_signif(
    y_position = 95,
    xmin = 1.1, 
    xmax = 1.9,
    annotation = "p=0.491",
    tip_length = 0.1,
    size = 0.5,
    textsize = 6) +
  
  #hetero-csh
  geom_signif(
    y_position = 95,
    xmin = 2.1,
    xmax = 2.9,
    annotation = "p=0.201",
    tip_length = 0.1,
    size = 0.5,
    textsize = 6) +
  
  scale_y_continuous(expand = c(0, 0), breaks=seq(0, 100,length = 5),limits = c(0, 120)) +
  theme_classic() +
  labs(y = "Copulation Rate (%)") +
  Setting +
  theme(axis.title.x = element_blank())
gp

ggsave("../figure_R/copulation_rate_graph.png", gp, width = 4, height = 6, dpi = 300)


###copulation duration###

#Calculate duration
df_cop_dur <- df_piezo_cop %>%
  mutate(duration = copulation_duration_min + (copulation_duration_sec/60)) %>%
  subset(!(is.na(df_piezo_cop$copulation_duration_min)))
head(df_cop_dur)

df_dur_homo_ana = df_cop_dur[df_cop_dur$genotype=="homo",]
df_dur_hetero_ana = df_cop_dur[df_cop_dur$genotype=="hetero",]
df_dur_csh_ana = df_cop_dur[df_cop_dur$genotype=="csh",]

describe(df_dur_homo_ana)
describe(df_dur_hetero_ana)
describe(df_dur_csh_ana)



gp2 = ggplot(data = df_cop_dur) +
  stat_qq(aes(sample = duration, color = genotype)) + 
  facet_wrap( ~ genotype, scales = "free")
gp2

sw_test_results = df_cop_dur %>%
  group_by(genotype) %>%  
  summarise(statistic = shapiro.test(duration)$statistic, 
            p.value = shapiro.test(duration)$p.value,
            .groups     = "drop")
head(sw_test_results)

bartlett.test(data = df_cop_dur, duration~genotype)



#ART 1-way-ANOVA
#https://rcompanion.org/handbook/F_16.html
df_1 = df_cop_dur %>%      
  select(genotype, duration) 
head(df_1) 
library(emmeans)
model = art(duration ~ genotype, data = df_1)
summary(model)
result　= anova(model)
model.lm = artlm(model, "genotype")
marginal = emmeans(model.lm, ~ genotype)
pairs(marginal, adjust = "holm")
Sum = as.data.frame(marginal)

sum_art = summary(pairs(marginal, adjust = "BH"))
sum_art$d = sum_art$estimate /sigmaHat(model.lm)
sum_art

result$part.eta.sq = with(result,`Sum Sq`/(`Sum Sq` +`Sum Sq.res`))
result



gp = ggplot(data = df_cop_dur, aes(x = genotype, y = duration)) +
  geom_boxplot(aes(fill = genotype), outlier.shape = NA) +
  geom_jitter(size = 1.5, width =0.1) +
  
  
  #homo-csh
  geom_signif(
    y_position = 65,
    xmin = 1.1, 
    xmax = 2.9,
    annotation = "p=0.013\nd=-0.407",
    tip_length = 0.03,
    size = 0.5,
    textsize = 6) + 
  
  #hetero-homo
  geom_signif(
    y_position = 50,
    xmin = 1.1,
    xmax = 1.9,
    annotation = "p=0.089\nd=0.268",
    tip_length = 0.03,
    size = 0.5,
    textsize = 6) +
  
  #hetero-csh
  geom_signif(
    y_position = 50,
    xmin = 2.1,
    xmax = 2.9,
    annotation = "p=0.0001\nd=-0.675",
    tip_length = 0.03, 
    size = 0.5,
    textsize = 6) + 
  
  
  #geom_dotplot(aes(x = genotype, y = ratio, fill = genotype), binaxis = "y", stackdir = "center", binwidth = 0.01) +
  #changing the x orders
  scale_x_discrete(labels = c('piezoKO' = lab_homo, 'piezoKOhetero' = lab_hetero, 'CS-H' = lab_csh)) +
  #change the color pallete
  scale_fill_brewer(palette = "Accent") +
  scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, 20), expand = c(0, 0)) +
  labs(y = "Copulation Duration (min)") +
  Setting +
  theme(axis.title.x = element_blank())
gp

ggsave("../figure_R/cop_duration_graph.png", gp, width = 4, height = 6, dpi = 300)



###No of pupaes###
df_pupae <- df_piezo_cop %>%
  subset(!(is.na(df_piezo_cop$no_pupae)))
head(df_pupae)

df_pu_homo_ana = df_pupae[df_pupae$genotype=="homo",]
df_pu_hetero_ana = df_pupae[df_pupae$genotype=="hetero",]
df_pu_csh_ana = df_pupae[df_pupae$genotype=="csh",]

describe(df_pu_homo_ana)
describe(df_pu_hetero_ana)
describe(df_pu_csh_ana)

gp2 = ggplot(data = df_pupae) +
  stat_qq(aes(sample = no_pupae, color = genotype)) + 
  facet_wrap( ~ genotype, scales = "free")
gp2

sw_test_results = df_pupae %>%
  group_by(genotype) %>%
  summarise(statistic = shapiro.test(no_pupae)$statistic, 
            p.value = shapiro.test(no_pupae)$p.value,
            .groups     = "drop") 
head(sw_test_results)

bartlett.test(data = df_pupae, no_pupae~genotype)


df_homo_ana = df_pupae[df_pupae$genotype=="homo",]
df_hetero_ana = df_pupae[df_pupae$genotype=="hetero",]
df_csh_ana = df_pupae[df_pupae$genotype=="csh",]


#ART one-way ANOVA
df_1 = df_pupae %>% 
  select(genotype, no_pupae)  
head(df_1) 
library(emmeans)
model = art(no_pupae ~ genotype, data = df_1)
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




gp = ggplot(data = df_pupae, aes(x = genotype, y = no_pupae)) +
  geom_boxplot(aes(fill = genotype), outlier.shape = NA) +
  geom_jitter(aes(color = genotype), size = 2, width =0.1) +
  
  
  #homo-csh
  geom_signif(
    y_position = 210,
    xmin = 1.1, 
    xmax = 2.9,
    annotation = "p=0.077\nd=-0.446",
    tip_length = 0.03,
    size = 0.5,
    textsize = 6) +
  
  #hetero-homo
  geom_signif(
    y_position = 160,
    xmin = 1.1,
    xmax = 1.9,
    annotation = "p=0.028\nd=-0.617",
    tip_length = 0.03,
    size = 0.5,
    textsize = 6) +
  
  #hetero-csh
  geom_signif(
    y_position = 160,
    xmin = 2.1,
    xmax = 2.9,
    annotation = "p=0.458\nd=0.171",
    tip_length = 0.03,
    size = 0.5,
    textsize = 6) +
  
  
  scale_y_continuous(limits = c(0, 250), breaks = seq(0, 250, 50), expand = c(0, 0)) +
  theme_classic() +
  labs(y = "No. of Pupae") +
  Setting +
  theme(axis.title.x = element_blank())

gp

ggsave("../figure_R/cop_no_pupae_graph.png", gp, width = 4, height = 6, dpi = 300)


