# Study: Impacts of the invasive round goby (Neogobius melanostomus) on benthic invertebrate macrofauna - a case study from the Baltic Sea
#
# Code authored by: Nicholas Moran, The Centre for Ocean Life- DTU Aqua, Technical University of Denmark
#                   Mikael van Deurs, DTU AQUA, Technical University of Denmark
# Date: March 2021



#### 3. Visualisations of Before-After Effects ####

Sys.setenv(LANG = "en")

library(dplyr); library(rlang); library(tidyverse); library(ggplot2); library(labeling)


load("./models/GULD.brms.origtaxa.negbinom.RData")
load("./models/STBT.brms.origtaxa.negbinom.RData")
load("./models/GULD.brms.fulltaxa.negbinom.RData")
load("./models/STBT.brms.fulltaxa.negbinom.RData")


#Figure 1a. Guldborgsund
working <- as.data.frame(coef(GULD.brms.origtaxa.negbinom, probs = c(0.05, 0.95)))

data_GULDfig <- NULL
data_GULDfig$estimate <- working$Artsgruppering.Estimate.BAbAfter
data_GULDfig$lower <- working$Artsgruppering.Q5.BAbAfter
data_GULDfig$upper <- working$Artsgruppering.Q95.BAbAfter
data_GULDfig$positions <- c( 3, 8, 5, 4, 7, 1, 6, 2)
data_GULDfig$color <- c( "darkorchid4", "deeppink4", "darkmagenta", "darkmagenta", "deeppink4", "darkorchid1", "darkmagenta", "darkorchid1")
data_GULDfig$labels <- rownames(working)
data_GULDfig <- as.data.frame(data_GULDfig)

GULDfig <- ggplot(data_GULDfig, aes(x = estimate, y = positions)) +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.x = element_text(size = 8.5, colour = "black"), 
        panel.background = element_rect(fill = "white"),
        axis.title.x  = element_text(size=13, vjust = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) +
  geom_segment(x = data_GULDfig$lower, y = data_GULDfig$pos, xend = data_GULDfig$upper, yend = data_GULDfig$pos, size = 1, colour = data_GULDfig$color) + 
  geom_point(shape = 19, size = 2, colour = data_GULDfig$color) +
  geom_vline(xintercept = 0, linetype = 2, colour = "black", size = 0.5) +
  geom_text(aes(label=labels, fontface = 4), hjust = "left", x =-8, vjust=-1.5, size = 2.5) +
  labs(x = "Before-after impact effect",
     y = "") +
  xlim(-7.5, 15) +
  ylim(0.5, 8.5)
GULDfig

ggsave("GULDfig.jpg", width = 18, height = 8, units = "cm", GULDfig, dpi = 600)





#Figure 1b. Stege Bugt
working <- as.data.frame(coef(STBT.brms.origtaxa.negbinom, probs = c(0.05, 0.95)))
