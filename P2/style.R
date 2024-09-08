library(ggplot2) # visualizations

my_theme <- theme(panel.border = element_rect(fill = NA, colour = "grey10"),
                  panel.background = element_blank(),
                  panel.grid.major = element_line(colour = "grey85"),
                  panel.grid.minor = element_line(colour = "grey85"),
                  panel.grid.major.x = element_line(colour = "grey85"),
                  axis.text = element_text(size = 13, face = "bold"),
                  axis.title = element_text(size = 15, face = "bold"),
                  plot.title = element_text(size = 16, face = "bold"),
                  strip.text = element_text(size = 16, face = "bold"),
                  strip.background = element_rect(colour = "black"),
                  legend.text = element_text(size = 15),
                  legend.title = element_blank(),
                  legend.background = element_blank(),
                  legend.key = element_rect(fill = "white"),
                  legend.position = "bottom")
