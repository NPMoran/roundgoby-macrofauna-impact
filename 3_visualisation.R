# Study: Impacts of the invasive round goby (Neogobius melanostomus) on benthic invertebrate macrofauna - a case study from the Baltic Sea
#
# Code authored by: Nicholas Moran, The Centre for Ocean Life- DTU Aqua, Technical University of Denmark
#                   Mikael van Deurs, DTU AQUA, Technical University of Denmark
# Date: March 2021



#### 3. Visualisations of Before-After Effects ####

Sys.setenv(LANG = "en")

library(dplyr); library(rlang); library(tidyverse); library(ggplot2); library(labeling); library(data.table)


#loading models
load("./models/GULD.brms.origtaxa.negbinom.RData")
load("./models/STBT.brms.origtaxa.negbinom.RData")
load("./models/GULD.brms.fulltaxa.negbinom.RData")
load("./models/STBT.brms.fulltaxa.negbinom.RData")


#Figure 1a. Guldborgsund
working <- as.data.frame(ranef(GULD.brms.origtaxa.negbinom, probs = c(0.05, 0.95)))

#building dataframe
data_GULDfig <- NULL
data_GULDfig$estimate <- working$Artsgruppering.Estimate.BAbAfter
data_GULDfig$lower <- working$Artsgruppering.Q5.BAbAfter
data_GULDfig$upper <- working$Artsgruppering.Q95.BAbAfter
data_GULDfig$positions <- c( 3, 8, 5, 4, 7, 1, 6, 2)
data_GULDfig$color <- c( "royalblue2", "deeppink4", "violetred3", "violetred3", "deeppink4", "palegreen3", "violetred3", "palegreen3")
data_GULDfig$labels <- rownames(working)
data_GULDfig <- as.data.frame(data_GULDfig)

#calculating before and after count per sq m to annotate graph
data_GULDgrouped <- read.csv("data_GULDgrouped.csv", strip.white=TRUE)
data_STBTgrouped <- read.csv("data_STBTgrouped.csv", strip.white=TRUE)

data_GULDgrouped$Count_psqm <- data_GULDgrouped$Count/0.0143 #as each core samples 0.0143m2 of the seabed
data_GULDgrouped.Before <- as.data.frame(setDT(subset(data_GULDgrouped, BA == 'aBefore'))[ , list(Before.Count_psqm = mean(Count_psqm), sdBefore.Count_psqm = sd(Count_psqm)), by = .(Artsgruppering)])
names(data_GULDgrouped.Before)[names(data_GULDgrouped.Before) == "Artsgruppering"] <- "labels"
data_GULDgrouped.After <- as.data.frame(setDT(subset(data_GULDgrouped, BA == 'bAfter'))[ , list(After.Count_psqm = mean(Count_psqm), sdAfter.Count_psqm = sd(Count_psqm)), by = .(Artsgruppering)])
names(data_GULDgrouped.After)[names(data_GULDgrouped.After) == "Artsgruppering"] <- "labels"

data_GULDfig <- merge(data_GULDfig, data_GULDgrouped.Before, by = 'labels', all.x = TRUE) 
data_GULDfig <- merge(data_GULDfig, data_GULDgrouped.After, by = 'labels', all.x = TRUE) 

data_GULDfig$Before.Count_psqm <- round(data_GULDfig$Before.Count_psqm, digits = 0)
data_GULDfig$sdBefore.Count_psqm <- round(data_GULDfig$sdBefore.Count_psqm, digits = 0)
data_GULDfig <- transform(data_GULDfig, Before = paste(Before.Count_psqm, sdBefore.Count_psqm, sep = " ± "))

data_GULDfig$After.Count_psqm <- round(data_GULDfig$After.Count_psqm, digits = 0)
data_GULDfig$sdAfter.Count_psqm <- round(data_GULDfig$sdAfter.Count_psqm, digits = 0)
data_GULDfig <- transform(data_GULDfig, After = paste(After.Count_psqm, sdAfter.Count_psqm, sep = " ± "))


#plotting
GULDfig <- ggplot(data_GULDfig, aes(x = estimate, y = positions)) +
  scale_x_continuous(limits = c(-7.5, 15), expand = c(0, 0), breaks=c(-7.5,-5.0,-2.5,0.0,2.5,5.0,7.5)) +
  scale_y_continuous(limits = c(0.5, 9), expand = c(0, 0), breaks=NULL) +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.x = element_text(size = 5, colour = "black"), 
        axis.line.x = element_line(colour = "black", size = 0.5),
        panel.background = element_rect(fill = "white"),
        axis.title.x  = element_text(size=8, vjust = 0.1),
        panel.border = element_rect(colour = "white", fill=NA, size = 1)) +
  geom_segment(x = data_GULDfig$lower, y = data_GULDfig$pos, xend = data_GULDfig$upper, yend = data_GULDfig$pos, size = 0.5, colour = data_GULDfig$color) + 
  geom_point(shape = 19, size = 1, colour = data_GULDfig$color) +
  geom_vline(xintercept = 0, linetype = 2, colour = "black", size = 0.5) +
  geom_text(aes(label=labels, fontface = 2), hjust = "left", x =-7.2, vjust=-2, size = 1.8, colour = data_GULDfig$color) +
  geom_text(aes(label=Before, fontface = 1), hjust = "left", x =7, vjust=0, size = 1.8) +
  geom_text(aes(label=After, fontface = 1), hjust = "left", x =11, vjust=0, size = 1.8) +
  labs(x = "Before-after impact effect (GULD)",
     y = "") 
GULDfig


ggsave("./visualisations/GULDfig.jpg", width = 8, height = 10, units = "cm", GULDfig, dpi = 600)



#Figure 1b. STBT
working <- as.data.frame(ranef(STBT.brms.origtaxa.negbinom, probs = c(0.05, 0.95)))

#building dataframe
data_STBTfig <- NULL
data_STBTfig$estimate <- working$Artsgruppering.Estimate.BAbAfter
data_STBTfig$lower <- working$Artsgruppering.Q5.BAbAfter
data_STBTfig$upper <- working$Artsgruppering.Q95.BAbAfter
data_STBTfig$positions <- c( 3, 8, 5, 4, 7, 1, 6, 2)
data_STBTfig$color <- c( "royalblue2", "deeppink4", "violetred3", "violetred3", "deeppink4", "palegreen3", "violetred3", "palegreen3")
data_STBTfig$labels <- rownames(working)
data_STBTfig <- as.data.frame(data_STBTfig)

#calculating before and after count per sq m to annotate graph
data_STBTgrouped <- read.csv("data_STBTgrouped.csv", strip.white=TRUE)
data_STBTgrouped <- read.csv("data_STBTgrouped.csv", strip.white=TRUE)

data_STBTgrouped$Count_psqm <- data_STBTgrouped$Count/0.0143 #as each core samples 0.0143m2 of the seabed
data_STBTgrouped.Before <- as.data.frame(setDT(subset(data_STBTgrouped, BA == 'aBefore'))[ , list(Before.Count_psqm = mean(Count_psqm), sdBefore.Count_psqm = sd(Count_psqm)), by = .(Artsgruppering)])
names(data_STBTgrouped.Before)[names(data_STBTgrouped.Before) == "Artsgruppering"] <- "labels"
data_STBTgrouped.After <- as.data.frame(setDT(subset(data_STBTgrouped, BA == 'bAfter'))[ , list(After.Count_psqm = mean(Count_psqm), sdAfter.Count_psqm = sd(Count_psqm)), by = .(Artsgruppering)])
names(data_STBTgrouped.After)[names(data_STBTgrouped.After) == "Artsgruppering"] <- "labels"

data_STBTfig <- merge(data_STBTfig, data_STBTgrouped.Before, by = 'labels', all.x = TRUE) 
data_STBTfig <- merge(data_STBTfig, data_STBTgrouped.After, by = 'labels', all.x = TRUE) 

data_STBTfig$Before.Count_psqm <- round(data_STBTfig$Before.Count_psqm, digits = 0)
data_STBTfig$sdBefore.Count_psqm <- round(data_STBTfig$sdBefore.Count_psqm, digits = 0)
data_STBTfig <- transform(data_STBTfig, Before = paste(Before.Count_psqm, sdBefore.Count_psqm, sep = " ± "))

data_STBTfig$After.Count_psqm <- round(data_STBTfig$After.Count_psqm, digits = 0)
data_STBTfig$sdAfter.Count_psqm <- round(data_STBTfig$sdAfter.Count_psqm, digits = 0)
data_STBTfig <- transform(data_STBTfig, After = paste(After.Count_psqm, sdAfter.Count_psqm, sep = " ± "))


#plotting
STBTfig <- ggplot(data_STBTfig, aes(x = estimate, y = positions)) +
  scale_x_continuous(limits = c(-7.5, 15), expand = c(0, 0), breaks=c(-7.5,-5.0,-2.5,0.0,2.5,5.0,7.5)) +
  scale_y_continuous(limits = c(0.5, 9), expand = c(0, 0), breaks=NULL) +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.x = element_text(size = 5, colour = "black"), 
        axis.line.x = element_line(colour = "black", size = 0.5),
        panel.background = element_rect(fill = "white"),
        axis.title.x  = element_text(size=8, vjust = 0.1),
        panel.border = element_rect(colour = "white", fill=NA, size = 1)) +
  geom_segment(x = data_STBTfig$lower, y = data_STBTfig$pos, xend = data_STBTfig$upper, yend = data_STBTfig$pos, size = 0.5, colour = data_STBTfig$color) + 
  geom_point(shape = 19, size = 1, colour = data_STBTfig$color) +
  geom_vline(xintercept = 0, linetype = 2, colour = "black", size = 0.5) +
  geom_text(aes(label=labels, fontface = 2), hjust = "left", x =-7.2, vjust=-2, size = 1.8, colour = data_STBTfig$color) +
  geom_text(aes(label=Before, fontface = 1), hjust = "left", x =7, vjust=0, size = 1.8) +
  geom_text(aes(label=After, fontface = 1), hjust = "left", x =11, vjust=0, size = 1.8) +
  labs(x = "Before-after impact effect (STBT)",
       y = "") 
STBTfig


ggsave("./visualisations/STBTfig.jpg", width = 8, height = 10, units = "cm", STBTfig, dpi = 600)



