# Study: Impacts of the invasive round goby (Neogobius melanostomus) on benthic invertebrate macrofauna - a case study from the Baltic Sea
#
# Code authored by: Nicholas Moran, The Centre for Ocean Life- DTU Aqua, Technical University of Denmark
#                   Mikael van Deurs, DTU AQUA, Technical University of Denmark
# Date: March 2021



#### 6. Sensitivity Analysis -
# NOTE: Sensitivity analysis run unsing Guldborgsund dataset, as our main Results (gut content analysis) are based on this site

# Part 1: rerunning analysis zero inflated neg binomial distribution ####

Sys.setenv(LANG = "en")
library(dplyr); library(brms); library(performance)
library(ggplot2); library(labeling); library(RColorBrewer)

#importing datasets
data_GULDgrouped <- read.csv("data_GULDgrouped.csv", strip.white=TRUE)
data_STBTgrouped <- read.csv("data_STBTgrouped.csv", strip.white=TRUE)

data_GULDgrouped$Year <- as.factor(data_GULDgrouped$Year)
data_STBTgrouped$Year <- as.factor(data_STBTgrouped$Year)


#preliminary model specifications
adapt_delta_value <- 0.9999
max_treedepth_value <- 20
iterations <- 2000
burnin <- 1000
thinning <- 2


GULD.brms.fulltaxa.zerinfnb <- brm(Count ~
                                     1 + BA + (1 + BA|TaxaGroup) + (1|SampleID) + (1|Year), data=data_GULDgrouped, 
                                   family = zero_inflated_negbinomial(),
                                   control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                   chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(GULD.brms.fulltaxa.zerinfnb)
summary(GULD.brms.fulltaxa.zerinfnb)
ranef(GULD.brms.fulltaxa.zerinfnb)
fixef(GULD.brms.fulltaxa.zerinfnb)
r2_bayes(GULD.brms.fulltaxa.zerinfnb, ci = 0.95)

save(GULD.brms.fulltaxa.zerinfnb, file = "./models/sensitivity/GULD.brms.fulltaxa.zerinfnb.RData")



# Part 2: Including sampling month as an additional random effect ####
# Sampling in 2015 was conducted approx 2 months earlier than in previous seasons, therefore, checking if similar responses are detected excluding 2015

#First, testing for taxa-specific responses are influenced by excluding 2015
data_GULDgrouped2015 <- subset(data_GULDgrouped, Year != 2015)

GULD.brms.fulltaxa.negbinom2015 <- brm(Count ~
                                       1 + BA + (1 + BA|TaxaGroup) + (1|SampleID) + (1|Year), data=data_GULDgrouped2015, 
                                     family = negbinomial(),
                                     control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                     chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(GULD.brms.fulltaxa.negbinom2015)
summary(GULD.brms.fulltaxa.negbinom2015)
ranef(GULD.brms.fulltaxa.negbinom2015)
fixef(GULD.brms.fulltaxa.negbinom2015)
r2_bayes(GULD.brms.fulltaxa.negbinom2015, ci = 0.95)

save(GULD.brms.fulltaxa.negbinom2015, file = "./models/sensitivity/GULD.brms.fulltaxa.negbinom2015.RData")


#Second, testing if gut content precense-absence hypothesis testing model is influenced by excluding 2015
data_GULDgrouped$Presence <- if_else(data_GULDgrouped$TaxaGroup == 'Littorinimorpha (small)', 'Present', 'Absent')
data_GULDgrouped$Presence <- if_else(data_GULDgrouped$TaxaGroup == 'Littorinimorpha (large)', 'Present', data_GULDgrouped$Presence <- data_GULDgrouped$Presence)
data_GULDgrouped$Presence <- if_else(data_GULDgrouped$TaxaGroup == 'Neritidae', 'Present', data_GULDgrouped$Presence <- data_GULDgrouped$Presence)
data_GULDgrouped$Presence <- if_else(data_GULDgrouped$TaxaGroup == 'Cardiidae', 'Present', data_GULDgrouped$Presence <- data_GULDgrouped$Presence)
data_GULDgrouped$Presence <- if_else(data_GULDgrouped$TaxaGroup == 'Isopoda', 'Present', data_GULDgrouped$Presence <- data_GULDgrouped$Presence)
data_GULDgrouped$Presence <- if_else(data_GULDgrouped$TaxaGroup == 'Amphipoda', 'Present', data_GULDgrouped$Presence <- data_GULDgrouped$Presence)
data_GULDgrouped$Presence <- if_else(data_GULDgrouped$TaxaGroup == 'Myidae', 'Present', data_GULDgrouped$Presence <- data_GULDgrouped$Presence)

data_GULDgrouped2015 <- subset(data_GULDgrouped, Year != 2015)

GULD.brms.guttest.negbinom_PresenceTEST2015 <- brm(Count ~
                                                 1 + BA*Presence + (BA|TaxaGroup) + (1|SampleID) + (1|Year), data=data_GULDgrouped2015, 
                                               family = negbinomial(),
                                               control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                               chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(GULD.brms.guttest.negbinom_PresenceTEST2015)
summary(GULD.brms.guttest.negbinom_PresenceTEST2015)
fixef(GULD.brms.guttest.negbinom_PresenceTEST2015)
r2_bayes(GULD.brms.guttest.negbinom_PresenceTEST2015, ci = 0.95)

save(GULD.brms.guttest.negbinom_PresenceTEST2015, file = "./models/sensitivity/GULD.brms.guttest.negbinom_PresenceTEST2015.RData")


# Plotting comparison (Figure S2)
load("./models/GULD.brms.fulltaxa.negbinom.RData")
load("./models/sensitivity/GULD.brms.fulltaxa.zerinfnb.RData")
load("./models/sensitivity/GULD.brms.fulltaxa.negbinom2015.RData")

working1 <- as.data.frame(ranef(GULD.brms.fulltaxa.negbinom, groups = 'TaxaGroup'))
working2 <- as.data.frame(ranef(GULD.brms.fulltaxa.zerinfnb, groups = 'TaxaGroup'))
working3 <- as.data.frame(ranef(GULD.brms.fulltaxa.negbinomA5, groups = 'TaxaGroup'))

#Building dataframe
data_SUPPfigA <- NULL
data_SUPPfigA$estimate1 <- working1$TaxaGroup.Estimate.BAbAfter
data_SUPPfigA$lower1 <- working1$TaxaGroup.Q2.5.BAbAfter
data_SUPPfigA$upper1 <- working1$TaxaGroup.Q97.5.BAbAfter
data_SUPPfigA$estimate2 <- working2$TaxaGroup.Estimate.BAbAfter
data_SUPPfigA$lower2 <- working2$TaxaGroup.Q2.5.BAbAfter
data_SUPPfigA$upper2 <- working2$TaxaGroup.Q97.5.BAbAfter
data_SUPPfigA$estimate3 <- working3$TaxaGroup.Estimate.BAbAfter
data_SUPPfigA$lower3 <- working3$TaxaGroup.Q2.5.BAbAfter
data_SUPPfigA$upper3 <- working3$TaxaGroup.Q97.5.BAbAfter

data_SUPPfigA$positions <- c(14.5, 1.0, 9.0, 23.5, 5.5, 
                            6.5, 13.5, 18.0, 17.0, 16.0, 
                            21.5, 22.5, 4.0, 11.0, 19.0, 
                            8.0, 10.0, 12.0, 20.5, 2.5)
data_SUPPfigA$position1 <- data_SUPPfigA$positions + 0.45
data_SUPPfigA$position2 <- data_SUPPfigA$positions + 0.2
data_SUPPfigA$position3 <- data_SUPPfigA$positions - 0.05

data_SUPPfigA$labels <- rownames(working1)
data_SUPPfigA <- as.data.frame(data_SUPPfigA)


SUPPfigA <- ggplot(data_SUPPfigA, aes(x = estimate1, y = positions)) +
  scale_x_continuous(limits = c(-11, 7), expand = c(0, 0), breaks=c(-5.0,-2.5,0.0,2.5,5.0)) +
  scale_y_continuous(limits = c(0.25, 25.5), expand = c(0, 0), breaks=NULL) +
  theme(legend.position = "none",
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.x = element_text(size = 5, colour = "black"), 
        axis.line.x = element_line(colour = "black", size = 0.5),
        panel.background = element_rect(fill = "white"),
        axis.title.x  = element_text(size=8, vjust = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) +
  geom_segment(x = data_SUPPfigA$lower1, y = data_SUPPfigA$position1, xend = data_SUPPfigA$upper1, yend = data_SUPPfigA$position1, size = 0.4) + 
  geom_point(x = data_SUPPfigA$estimate1, y = data_SUPPfigA$position1, shape = 19, size = 0.8) +
  geom_segment(colour = "dodgerblue2", x = data_SUPPfigA$lower2, y = data_SUPPfigA$position2, xend = data_SUPPfigA$upper2, yend = data_SUPPfigA$position2, size = 0.4) + 
  geom_point(colour = "dodgerblue2", x = data_SUPPfigA$estimate2, y = data_SUPPfigA$position2, shape = 19, size = 0.8) +
  geom_segment(colour = "firebrick2", x = data_SUPPfigA$lower3, y = data_SUPPfigA$position3, xend = data_SUPPfigA$upper3, yend = data_SUPPfigA$position3, size = 0.4) + 
  geom_point(colour = "firebrick2", x = data_SUPPfigA$estimate3, y = data_SUPPfigA$position3, shape = 19, size = 0.8) +
  geom_vline(xintercept = 0, linetype = 2, colour = "black", size = 0.5) +
  geom_hline(yintercept = c(2, 3.5, 5.0, 7.5, 13.0, 15.5, 20.0), linetype = 3, colour = "black", size = 0.25) +
  geom_text(aes(label=labels, fontface = 3), hjust = "left", x =-10.5, vjust=-1.2, size = 1.8) +
  labs(x = "Before-after impact effect (Guldborgsund)",
       y = "") 
SUPPfigA

ggsave("./visualisations/SUPPfig.jpg", width = 8, height = 16, units = "cm", SUPPfigA, dpi = 600)

