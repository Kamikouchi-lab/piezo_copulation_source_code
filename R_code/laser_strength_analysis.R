library(ggplot2)
library(tidyverse)
library(lawstat)
library(psych)

Setting <- list(theme_classic(),
                 theme(plot.margin = unit(c(1, 1, 1, 1), "lines")),
                 theme(axis.title.x = element_text(size = 15, vjust = -0.5, color = "black")),
                 theme(axis.title.y = element_text(size = 15, vjust = 2, color = "black")),
                 theme(axis.text.x = element_text(size = 15, colour = "black", vjust = 0.5)),
                 theme(axis.text.y = element_text(size = 15, colour = "black")),
                 theme(axis.line = element_line(colour = "black")),
                 scale_fill_brewer(palette = "Accent"), 
                 labs(title = "TITLE"), 
                 theme(plot.title = element_text(size = 20, colour = "white")),
                 theme(legend.position = "none"))

df_max <- readr::read_csv("data/df_max.csv")
print(df_max)
describe(df_max)

laser_rad = 1/exp(2)

gp1 = ggplot(data = df_max, aes(x = width, y = relative_intensity)) + 
  geom_line() +
  geom_line(aes(y= laser_rad), color = "red", lwd =0.5) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.5), expand = c(0, 0)) +
  labs(x = "Length", y = "Relative intensity") +
  Setting
gp1

ggsave("laser_strength.png", gp1, width = 5, height = 3, dpi = 300)
