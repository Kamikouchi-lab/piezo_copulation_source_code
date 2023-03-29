# install.packages("ggplot2")
# install.packages("ggsignif")
# install.packages("RVAideMemoire")
library(ggplot2)
library("ggsignif")
library(RVAideMemoire)

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


mx <- matrix(c(11, 6, 8, 19, 21, 19), nrow = 3,
             dimnames = list(genotype = c("M", "T", "C"),
                             phenotype = c("maru", "batsu")))
mx1 <- matrix(c(11, 19, 6, 21, 8, 19), nrow = 2)
mx
mx1

testResult1<- fisher.multcomp(mx1, p.method= "BH")
testResult1
testResult2 <- fisher.test(df_a1)
testResult2



a = c(16, 15, 16)
b = c(11, 11, 13)
c = c(16*100/27, 15*100/26, 16*100/29)
d = c("homo", "hetero", "csh")
df1 = data.frame(a, b, c, d)
df1

df1 <- transform(df1, d = factor(d, levels = c("homo", "hetero", "csh")))

df_a1 = data.frame(a, b)

gp1 = ggplot(df1, aes(x = d, y = c), color = "black") + 
  geom_bar(aes(fill = d), color = "black", stat = "identity") +
  
  #homo-csh
  geom_signif(
    y_position = 85,
    xmin = 1.1, 
    xmax = 2.8, 
    annotation = "p=0.779",
    tip_length = 0.5, 
    size = 0.5, 
    textsize = 5) + 
  
  #homo-hetero
  geom_signif(
    y_position = 70,
    xmin = 1.1, 
    xmax = 1.9, 
    annotation = "p=0.779",
    tip_length = 0.5,
    size = 0.5, 
    textsize = 5) + 
  
  #hetero-csh
  geom_signif(
    y_position = 70,
    xmin = 2.1, 
    xmax = 2.9, 
    annotation = "p=0.779",
    tip_length = 0.5,
    size = 0.5, 
    textsize = 5) + 

  theme_classic() + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), breaks = seq(0, 100, 25)) +
  labs(y = "Disturbed Rate (%)") +
  Setting
gp1



ggsave("../figure_R/cop_disrupted.png", gp1, width = 4, height = 5, dpi = 500)
