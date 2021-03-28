# Study: Impacts of the invasive round goby (Neogobius melanostomus) on benthic invertebrate macrofauna - a case study from the Baltic Sea
#
# Code authored by: Nicholas Moran, The Centre for Ocean Life- DTU Aqua, Technical University of Denmark
#                   Mikael van Deurs, DTU AQUA, Technical University of Denmark
# Date: March 2021



#### 3. Visualisations of Before-After Effects ####

Sys.setenv(LANG = "en")

library(dplyr); library(rlang); library(tidyverse); library(ggplot2); 
library(labeling); library(data.table); library(RColorBrewer); library(brms)


#loading models
load("./models/GULD.brms.fulltaxa.negbinom.RData")
load("./models/STBT.brms.fulltaxa.negbinom.RData")


## Figure 1a. Guldborgsund (FULL TAXA) ----
working <- as.data.frame(ranef(GULD.brms.fulltaxa.negbinom, probs = c(0.05, 0.95)))

#Building dataframe
data_GULDfig <- NULL
data_GULDfig$estimate <- working$TaxaGroup.Estimate.BAbAfter
data_GULDfig$lower <- working$TaxaGroup.Q5.BAbAfter
data_GULDfig$upper <- working$TaxaGroup.Q95.BAbAfter
data_GULDfig$positions <- c(14.5, 1.0, 9.0, 23.5, 5.5, 
                            6.5, 13.5, 18.0, 17.0, 16.0, 
                            21.5, 22.5, 4.0, 11.0, 19.0, 
                            8.0, 10.0, 12.0, 20.5, 2.5)
data_GULDfig$color <- c('Group 3', 'Group 8', 'Group 4', 'Group 1', 'Group 5', 
                        'Group 5', 'Group 3', 'Group 2', 'Group 2', 'Group 2', 
                        'Group 1', 'Group 1', 'Group 6', 'Group 4', 'Group 2',  
                        'Group 4', 'Group 4', 'Group 4', 'Group 1', 'Group 7')
data_GULDfig$labels <- rownames(working)


#Calculating before and after count per sq m to annotate graph
data_GULDgrouped <- read.csv("data_GULDgrouped.csv", strip.white=TRUE)

data_GULDgrouped$Count_psqm <- data_GULDgrouped$Count/0.0143 #as each core samples 0.0143m2 of the seabed
data_GULDgrouped.Before <- as.data.frame(setDT(subset(data_GULDgrouped, BA == 'aBefore'))[ , list(Before.Count_psqm = mean(Count_psqm), sdBefore.Count_psqm = sd(Count_psqm)), by = .(TaxaGroup)])
names(data_GULDgrouped.Before)[names(data_GULDgrouped.Before) == "TaxaGroup"] <- "labels"
data_GULDgrouped.After <- as.data.frame(setDT(subset(data_GULDgrouped, BA == 'bAfter'))[ , list(After.Count_psqm = mean(Count_psqm), sdAfter.Count_psqm = sd(Count_psqm)), by = .(TaxaGroup)])
names(data_GULDgrouped.After)[names(data_GULDgrouped.After) == "TaxaGroup"] <- "labels"

data_GULDfig <- merge(data_GULDfig, data_GULDgrouped.Before, by = 'labels', all.x = TRUE) 
data_GULDfig <- merge(data_GULDfig, data_GULDgrouped.After, by = 'labels', all.x = TRUE) 

data_GULDfig$Before.Count_psqm <- round(data_GULDfig$Before.Count_psqm, digits = 0)
data_GULDfig$sdBefore.Count_psqm <- round(data_GULDfig$sdBefore.Count_psqm, digits = 0)
data_GULDfig <- transform(data_GULDfig, Before = paste(Before.Count_psqm, sdBefore.Count_psqm, sep = " ± "))

data_GULDfig$After.Count_psqm <- round(data_GULDfig$After.Count_psqm, digits = 0)
data_GULDfig$sdAfter.Count_psqm <- round(data_GULDfig$sdAfter.Count_psqm, digits = 0)
data_GULDfig <- transform(data_GULDfig, After = paste(After.Count_psqm, sdAfter.Count_psqm, sep = " ± "))
data_GULDfig <- transform(data_GULDfig, BeforeAfter = paste('(', Before, sep = ""))
data_GULDfig <- transform(data_GULDfig, BeforeAfter = paste(BeforeAfter, After, sep = " → "))
data_GULDfig <- transform(data_GULDfig, BeforeAfter = paste(BeforeAfter, ')', sep = ""))

#adding annotation for taxa found in gut content
data_GULDfig$labels <- if_else(data_GULDfig$labels == 'Amphipoda', 'Amphipoda*', data_GULDfig$labels <- data_GULDfig$labels)
data_GULDfig$labels <- if_else(data_GULDfig$labels == 'Isopoda', 'Isopoda*', data_GULDfig$labels <- data_GULDfig$labels)
data_GULDfig$labels <- if_else(data_GULDfig$labels == 'Littorinimorpha (small)', 'Littorinimorpha (small)*', data_GULDfig$labels <- data_GULDfig$labels)
data_GULDfig$labels <- if_else(data_GULDfig$labels == 'Littorinimorpha (large)', 'Littorinimorpha (large)*', data_GULDfig$labels <- data_GULDfig$labels)
data_GULDfig$labels <- if_else(data_GULDfig$labels == 'Myidae', 'Myidae*', data_GULDfig$labels <- data_GULDfig$labels)
data_GULDfig$labels <- if_else(data_GULDfig$labels == 'Cardiidae', 'Cardiidae*', data_GULDfig$labels <- data_GULDfig$labels)
data_GULDfig$labels <- if_else(data_GULDfig$labels == 'Neritidae', 'Neritidae*', data_GULDfig$labels <- data_GULDfig$labels)
data_GULDfig <- as.data.frame(data_GULDfig)


#Plotting
GULDfig <- ggplot(data_GULDfig, aes(x = estimate, y = positions)) +
scale_x_continuous(limits = c(-10, 6), expand = c(0, 0), breaks=c(-5.0,-2.5,0.0,2.5,5.0)) +
scale_y_continuous(limits = c(0.25, 25.75), expand = c(0, 0), breaks=NULL) +
theme(legend.position = "none",
      axis.text.y = element_blank(), 
      axis.ticks.y = element_blank(), 
      axis.text.x = element_text(size = 5, colour = "black"), 
      axis.line.x = element_line(colour = "black", size = 0.5),
      panel.background = element_rect(fill = "white"),
      axis.title.x  = element_text(size=8, vjust = 0.1),
      panel.border = element_rect(colour = "black", fill=NA, size = 1)) +
geom_segment(aes(color = color), x = data_GULDfig$lower, y = data_GULDfig$pos, xend = data_GULDfig$upper, yend = data_GULDfig$pos, size = 0.5) + 
geom_point(aes(color = color), shape = 19, size = 1) +
scale_color_brewer(palette = "Dark2") +
geom_vline(xintercept = 0, linetype = 2, colour = "black", size = 0.5) +
geom_hline(yintercept = c(2, 3.5, 5.0, 7.5, 13.0, 15.5, 20.0), linetype = 3, colour = "black", size = 0.25) +
geom_text(aes(label=labels, fontface = 3), hjust = "left", x =-9.5, vjust=-1.8, size = 1.8) +
geom_text(aes(label=BeforeAfter, fontface = 1), hjust = "left", x =-9.5, vjust=0.3, size = 1.8) +
labs(x = "Before-after impact effect (Guldborgsund)",
     y = "") 
GULDfig

ggsave("./visualisations/GULDFULLfig.jpg", width = 8, height = 16, units = "cm", GULDfig, dpi = 600)



#Figure 1b. Stege Bugt (FULL TAXA) ----
working <- as.data.frame(ranef(STBT.brms.fulltaxa.negbinom, probs = c(0.05, 0.95), groups = 'TaxaGroup'))

#Building dataframe
data_STBTfig <- NULL
data_STBTfig$estimate <- working$TaxaGroup.Estimate.BAbAfter
data_STBTfig$lower <- working$TaxaGroup.Q5.BAbAfter
data_STBTfig$upper <- working$TaxaGroup.Q95.BAbAfter
data_STBTfig$positions <- c(14.5, 1.0, 9.0, 23.5, 5.5, 
                            6.5, 13.5, 18.0, 17.0, 16.0, 
                            21.5, 22.5, 4.0, 11.0, 19.0, 
                            10.0, 12.0, 20.5, 2.5)
data_STBTfig$color <- c('Group 3', 'Group 8', 'Group 4', 'Group 1', 'Group 5', 
                        'Group 5', 'Group 3', 'Group 2', 'Group 2', 'Group 2', 
                        'Group 1', 'Group 1', 'Group 6', 'Group 4', 'Group 2',  
                        'Group 4', 'Group 4', 'Group 1', 'Group 7')
data_STBTfig$labels <- rownames(working)


#Calculating before and after count per sq m to annotate graph
data_STBTgrouped <- read.csv("data_STBTgrouped.csv", strip.white=TRUE)

data_STBTgrouped$Count_psqm <- data_STBTgrouped$Count/0.0143 #as each core samples 0.0143m2 of the seabed
data_STBTgrouped.Before <- as.data.frame(setDT(subset(data_STBTgrouped, BA == 'aBefore'))[ , list(Before.Count_psqm = mean(Count_psqm), sdBefore.Count_psqm = sd(Count_psqm)), by = .(TaxaGroup)])
names(data_STBTgrouped.Before)[names(data_STBTgrouped.Before) == "TaxaGroup"] <- "labels"
data_STBTgrouped.After <- as.data.frame(setDT(subset(data_STBTgrouped, BA == 'bAfter'))[ , list(After.Count_psqm = mean(Count_psqm), sdAfter.Count_psqm = sd(Count_psqm)), by = .(TaxaGroup)])
names(data_STBTgrouped.After)[names(data_STBTgrouped.After) == "TaxaGroup"] <- "labels"

data_STBTfig <- merge(data_STBTfig, data_STBTgrouped.Before, by = 'labels', all.x = TRUE) 
data_STBTfig <- merge(data_STBTfig, data_STBTgrouped.After, by = 'labels', all.x = TRUE) 

data_STBTfig$Before.Count_psqm <- round(data_STBTfig$Before.Count_psqm, digits = 0)
data_STBTfig$sdBefore.Count_psqm <- round(data_STBTfig$sdBefore.Count_psqm, digits = 0)
data_STBTfig <- transform(data_STBTfig, Before = paste(Before.Count_psqm, sdBefore.Count_psqm, sep = " ± "))

data_STBTfig$After.Count_psqm <- round(data_STBTfig$After.Count_psqm, digits = 0)
data_STBTfig$sdAfter.Count_psqm <- round(data_STBTfig$sdAfter.Count_psqm, digits = 0)
data_STBTfig <- transform(data_STBTfig, After = paste(After.Count_psqm, sdAfter.Count_psqm, sep = " ± "))
data_STBTfig <- transform(data_STBTfig, BeforeAfter = paste('(', Before, sep = ""))
data_STBTfig <- transform(data_STBTfig, BeforeAfter = paste(BeforeAfter, After, sep = " → "))
data_STBTfig <- transform(data_STBTfig, BeforeAfter = paste(BeforeAfter, ')', sep = ""))

data_STBTfig <- as.data.frame(data_STBTfig)


#Plotting
STBTfig <- ggplot(data_STBTfig, aes(x = estimate, y = positions)) +
  scale_x_continuous(limits = c(-10, 6), expand = c(0, 0), breaks=c(-5.0,-2.5,0.0,2.5,5.0)) +
  scale_y_continuous(limits = c(0.25, 25.75), expand = c(0, 0), breaks=NULL) +
  theme(legend.position = "none",
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.x = element_text(size = 5, colour = "black"), 
        axis.line.x = element_line(colour = "black", size = 0.5),
        panel.background = element_rect(fill = "white"),
        axis.title.x  = element_text(size=8, vjust = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) +
  geom_segment(aes(color = color), x = data_STBTfig$lower, y = data_STBTfig$pos, xend = data_STBTfig$upper, yend = data_STBTfig$pos, size = 0.5) + 
  geom_point(aes(color = color), shape = 19, size = 1) +
  scale_color_brewer(palette = "Dark2") +
  geom_vline(xintercept = 0, linetype = 2, colour = "black", size = 0.5) +
  geom_hline(yintercept = c(2, 3.5, 5.0, 7.5, 13.0, 15.5, 20.0), linetype = 3, colour = "black", size = 0.25) +
  geom_text(aes(label=labels, fontface = 3), hjust = "left", x =-9.5, vjust=-1.8, size = 1.8) +
  geom_text(aes(label=BeforeAfter, fontface = 1), hjust = "left", x =-9.5, vjust=0.3, size = 1.8) +
  labs(x = "Before-after impact effect (Stege Bugt)",
       y = "") 
STBTfig

ggsave("./visualisations/STBTFULLfig.jpg", width = 8, height = 16, units = "cm", STBTfig, dpi = 600)


