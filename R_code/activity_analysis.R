# install.packages("rethomics")
# install.packages("behavr")
# install.packages("devtools")
# install.packages(c('ggetho', 'damr'))
# install.packages("zeitgebr")
# install.packages("sleepr")
# install.packages("ggpubr")


library(devtools)
library(behavr)
library(ggplot2)
library(ggetho)
library(damr)
library(tidyverse)
library(zeitgebr)
library(rstatix)
library(ggpubr)
library(survival)
library(sleepr)
library(lawstat)
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

cohens.d <- function(x){
  ans <- 2*x/sqrt(1-x^2)
  return(ans)
}
getwd()

#read to CSV
DATA_DIR <- "/Users/tankororin/Desktop/piezoKO_DAM_analysis/analysis_data" #change directory for your enveronment
list.files(DATA_DIR, pattern= "*.txt|*.csv")
setwd(DATA_DIR)

metadata <- fread("metadata.csv")
metadata <- link_dam_metadata(metadata, result_dir = DATA_DIR)
metadata

dt <- load_dam(metadata)
dt_activity <- dt[, moving := activity > 0]
dt_activity

dt_curated <- curate_dead_animals(dt_activity)
summary(dt_curated)
setdiff(dt[, id, meta=T], dt_curated[, id, meta=T])

lifespan_dt <- dt_curated[, .(lifespan = max(t)), by=id]
valid_ids <- lifespan_dt[lifespan > days(3), id]
dt_curated <- dt_curated[id %in% valid_ids]
#summary(dt_curated)
#write.csv(dt_curated, "data_curated.csv")

a = days(1)/24
start_t = a * 1 + days(0)
end_t = a * 11 + days(0)
#Trimming
dt_curated_1L <- dt_curated[t %between% c(start_t, end_t)] #getting data from 0h to 12h (L time)

summary_dt <- rejoin(dt_curated_1L[, by=id])
summary_dt

count_dt <- count(group_by(summary_dt, genotype, region_id, moving))
count_dt <- count_dt[count_dt$moving=="TRUE",]
count_dt

count_dt <- transform(count_dt, genotype = factor(genotype, levels = c("piezo_KO", "CS-H")))

df_piezoKO <- count_dt[count_dt$genotype=="piezo_KO",]
df_csh <- count_dt[count_dt$genotype=="CS-H",]

describe(df_piezoKO)
describe(df_csh)

gp = ggplot(count_dt, aes(x = genotype, y = n))+
  geom_boxplot(aes(fill = genotype), outlier.shape = NA)+
  geom_jitter(size = 4, width =0.1) +
  
  #atr-etoh
  geom_signif(
    y_position = 180,
    xmin = 1.0,
    xmax = 2.0, 
    annotation = "p<0.001?nd=1.537",
    tip_length = 0.01, 
    size = 0.5,
    textsize = 9) + 
  
  
  scale_y_continuous(limits = c(0, 230), breaks = seq(0, 230, 50), expand = c(0, 0)) +
  theme_classic() +
  labs(y = "Locomotor Activity") +
  scale_x_discrete(labels = c('piezo_KO' = lab_homo,'CS-H' = lab_csh)) +
  Setting +
  theme(axis.title.x = element_blank())

gp
ggsave("locomotion_activity.png", gp, width = 5, height = 7, dpi = 300)

sw_test_results = count_dt %>% 
  group_by(genotype) %>% 
  summarise(statistic = shapiro.test(n)$statistic, 
            p.value = shapiro.test(n)$p.value,
            .groups     = "drop") 
head(sw_test_results)

var.test(data = count_dt, n?genotype)


library(lawstat)
report(t.test(df_piezoKO$n, df_csh$n))
a = t.test(df_piezoKO$n, df_csh$n)
a

cohen.d(df_piezoKO$n, df_csh$n, hedges.correction = FALSE)
