#install.packages("ggsignif")

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
                theme(axis.title.y = element_text(size = 15, vjust = 2)),
                
                theme(axis.text.y = element_text(size = 15, colour = "black")),
                scale_fill_manual(values = c("#f39800", "#aaaaaa", "#aaaaaa")),
                scale_color_manual(values = c("#000000", "#000000", "#000000")),
                scale_x_discrete(labels = c('homo' = lab_homo, 'hetero' = lab_hetero, 'csh' = lab_csh)),
                theme(plot.title = element_text(size = 20, hjust = 0.5)),
                theme(axis.text.x = element_text(size = 15, colour = "black", angle = 45, hjust = 1)),
                theme(axis.title.x = element_blank()),
                theme(legend.position = "none"))



draw_graph <- function(filename, b, c, d, e, f, g, deg, csv_path, f_path){
  #read to CSV
  print(paste(deg, " start analysis", sep=""))
  
  df_cop_angle <- readr::read_csv(paste(csv_path, filename,".csv", sep=""))
  head(df_cop_angle)
  df_cop_angle <- transform(df_cop_angle, genotype = factor(genotype, levels = c("homo", "hetero", "csh")))
  
  
  df_homo_ana = df_cop_angle[df_cop_angle$genotype=="homo",]
  df_hetero_ana = df_cop_angle[df_cop_angle$genotype=="hetero",]
  df_csh_ana = df_cop_angle[df_cop_angle$genotype=="csh",]
  
  print(describe(df_homo_ana))
  print(describe(df_hetero_ana))
  print(describe(df_csh_ana))
  
  ###copulatin angle ratio###
  gp = ggplot(data = df_cop_angle) +
    stat_qq(aes(sample = ratio, color = genotype)) + 
    facet_wrap( ~ genotype, scales = "free")
  gp
  
  sw_test_results = df_cop_angle %>% 
    group_by(genotype) %>% 
    summarise(statistic = shapiro.test(ratio)$statistic, 
              p.value = shapiro.test(ratio)$p.value, 
              .groups     = "drop") 
  print(sw_test_results)
  
  print(bartlett.test(data = df_cop_angle, ratio~genotype))
  #ART one-way ANOVA
  model = art(ratio ~ genotype, data = df_cop_angle)
  summary(model)
  result = anova(model)
  print(result, verbose = TRUE)
  model.lm = artlm(model, "genotype")
  marginal = emmeans(model.lm, ~ genotype)
  pairs(marginal, adjust = "BH")
  Sum = as.data.frame(marginal)
  print(Sum)
  sum_art = summary(pairs(marginal, adjust = "BH"))
  sum_art$d = sum_art$estimate /sigmaHat(model.lm)
  print(sum_art)
  result$part.eta.sq = with(result,`Sum Sq`/(`Sum Sq` +`Sum Sq.res`))
  print(result)
  summary(marginal)
  
  #drawing coplation angle graph
  degree = paste(deg, "degree")
  gp = ggplot(data = df_cop_angle, aes(x = genotype, y = ratio)) +
    geom_boxplot(aes(fill = genotype), outlier.shape = NA) +
    geom_jitter(size = 1, width =0.1, aes(colour = genotype)) +
    
    
    #homo-hetero
    geom_signif(
      y_position = c,
      xmin = 1.1, 
      xmax = 1.9, 
      annotation = d,
      tip_length = 0.03,
      size = 0.3,
      textsize = 4) + 
    
    #homo-csh
    geom_signif(
      y_position = b,
      xmin = 1.1,
      xmax = 2.9,
      annotation = e,
      tip_length = 0.03,
      size = 0.3,
      textsize = 4) +
    
    #hetero-csh
    geom_signif(
      y_position = c,
      xmin = 2.1,
      xmax = 2.9,
      annotation = f,
      tip_length = 0.03,
      size = 0.3,
      textsize = 4) +
    
    
    scale_y_continuous(limits = c(-0.01, g), breaks = seq(0, 1, 0.5), expand = c(0, 0)) +
    
    labs(title = degree, y = "Tilted Copulation Ratio") +
    Setting
  
  gp
  
  ggsave(paste(f_path, filename, ".png", sep=""), gp, width = 3, height = 4, dpi = 300)
  
}



draw_graph("df_angle_20", 1.2, 0.9, "p=0.017\nd=0.440", "p=0.017\nd=0.414", "p=0.879\nd=-0.026", 1.45, 20, "../csv_total/20_30/", "../figure_R/20-30/")
draw_graph("df_angle_21", 1.2, 0.9, "p=0.014\nd=0.452", "p=0.014\nd=0.423", "p=0.862\nd=-0.029", 1.45, 21, "../csv_total/20_30/", "../figure_R/20-30/")
draw_graph("df_angle_22", 1.2, 0.9, "p=0.017\nd=0.446", "p=0.017\nd=0.414", "p=0.849\nd=-0.031", 1.45, 22, "../csv_total/20_30/", "../figure_R/20-30/")
draw_graph("df_angle_23", 1.2, 0.9, "p=0.016\nd=0.450", "p=0.016\nd=0.417", "p=0.843\nd=-0.033", 1.45, 23, "../csv_total/20_30/", "../figure_R/20-30/")
draw_graph("df_angle_24", 1.2, 0.9, "p=0.018\nd=0.448", "p=0.018\nd=0.410", "p=0.818\nd=-0.039", 1.45, 24, "../csv_total/20_30/", "../figure_R/20-30/")
draw_graph("df_angle_25", 1.15, 0.85, "p=0.022\nd=0.448", "p=0.022\nd=0.398", "p=0.763\nd=-0.050", 1.45, 25, "../csv_total/20_30/", "../figure_R/20-30/")
draw_graph("df_angle_26", 1.15, 0.85, "p=0.025\nd=0.450", "p=0.026\nd=0.389", "p=0.716\nd=-0.061", 1.45, 26, "../csv_total/20_30/", "../figure_R/20-30/")
draw_graph("df_angle_27", 1.15, 0.85, "p=0.021\nd=0.459", "p=0.025\nd=0.389", "p=0.676\nd=-0.070", 1.45, 27, "../csv_total/20_30/", "../figure_R/20-30/")
draw_graph("df_angle_28", 1.1, 0.8, "p=0.023\nd=0.455", "p=0.024\nd=0.392", "p=0.709\nd=-0.063", 1.45, 28, "../csv_total/20_30/", "../figure_R/20-30/")
draw_graph("df_angle_29", 1.1, 0.8, "p=0.019\nd=0.467", "p=0.024\nd=0.393", "p=0.662\nd=-0.073", 1.45, 29, "../csv_total/20_30/", "../figure_R/20-30/")
draw_graph("df_angle_30", 1.1, 0.8, "p=0.014\nd=0.484", "p=0.021\nd=0.401", "p=0.619\nd=-0.083", 1.45, 30, "../csv_total/20_30/", "../figure_R/20-30/")

