# Study: Impacts of the invasive round goby (Neogobius melanostomus) on benthic invertebrate macrofauna - a case study from the Baltic Sea
#
# Code authored by: Nicholas Moran, The Centre for Ocean Life- DTU Aqua, Technical University of Denmark
#                   Mikael van Deurs, DTU AQUA, Technical University of Denmark
# Date: March 2021



#### 6. Sensitivity Analysis -
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

save(GULD.brms.fulltaxa.zerinfnb, file = "./models/A1_sensitivity/GULD.brms.fulltaxa.zerinfnb.RData")


STBT.brms.fulltaxa.zerinfnb <- brm(Count ~
                                     1 + BA + (1 + BA|TaxaGroup) + (1|SampleID) + (1|Year), data=data_STBTgrouped, 
                                   family = zero_inflated_negbinomial(),
                                   control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                   chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(STBT.brms.fulltaxa.zerinfnb)
summary(STBT.brms.fulltaxa.zerinfnb)
ranef(STBT.brms.fulltaxa.zerinfnb)
fixef(STBT.brms.fulltaxa.zerinfnb)
r2_bayes(STBT.brms.fulltaxa.zerinfnb, ci = 0.95)

save(STBT.brms.fulltaxa.zerinfnb, file = "./models/A1_sensitivity/STBT.brms.fulltaxa.zerinfnb.RData")


# Part 2: Including sampling month as an additional random effect ####
# Sampling in 2015 was conducted approx 2 months earlier than in previous seasons,
# Therefore, checking if similar responses are detected excluding 2015, or by including sampling month as a factor

#Including sampling month as a random effect
data_GULDgrouped$Month <- if_else(data_GULDgrouped$Year == '2015', 'March', 'May')
data_STBTgrouped$Month <- if_else(data_STBTgrouped$Year == '2015', 'March', 'May')

GULD.brms.fulltaxa.negbinomA2 <- brm(Count ~
                                       1 + BA + (1 + BA|TaxaGroup) + (1|SampleID) + (1|Month) + (1|Year) , data=data_GULDgrouped, 
                                     family = negbinomial(),
                                     control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                     chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(GULD.brms.fulltaxa.negbinomA2)
summary(GULD.brms.fulltaxa.negbinomA2)
ranef(GULD.brms.fulltaxa.negbinomA2)
fixef(GULD.brms.fulltaxa.negbinomA2)
r2_bayes(GULD.brms.fulltaxa.negbinomA2, ci = 0.95)

save(GULD.brms.fulltaxa.negbinomA2, file = "./models/A2_sensitivity/GULD.brms.fulltaxa.negbinomA2.RData")


STBT.brms.fulltaxa.negbinomA2 <- brm(Count ~
                                       1 + BA + (1 + BA|TaxaGroup) + (1|SampleID) + (1|Month) + (1|Year), data=data_STBTgrouped, 
                                     family = negbinomial(),
                                     control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                     chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(STBT.brms.fulltaxa.negbinomA2)
summary(STBT.brms.fulltaxa.negbinomA2)
ranef(STBT.brms.fulltaxa.negbinomA2)
fixef(STBT.brms.fulltaxa.negbinomA2)
r2_bayes(STBT.brms.fulltaxa.negbinomA2, ci = 0.95)

save(STBT.brms.fulltaxa.negbinomA2, file = "./models/A2_sensitivity/STBT.brms.fulltaxa.negbinomA2.RData")


#Using Month as a fixed effect
GULD.brms.fulltaxa.negbinomA3 <- brm(Count ~
                                       1 + BA + Month + (1 + BA + Month|TaxaGroup) + (1|SampleID) + (1|Year) , data=data_GULDgrouped, 
                                     family = negbinomial(),
                                     control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                     chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(GULD.brms.fulltaxa.negbinomA3)
summary(GULD.brms.fulltaxa.negbinomA3)
ranef(GULD.brms.fulltaxa.negbinomA3)
fixef(GULD.brms.fulltaxa.negbinomA3)
r2_bayes(GULD.brms.fulltaxa.negbinomA3, ci = 0.95)

save(GULD.brms.fulltaxa.negbinomA3, file = "./models/A2_sensitivity/GULD.brms.fulltaxa.negbinomA3.RData")


STBT.brms.fulltaxa.negbinomA3 <- brm(Count ~
                                       1 + BA + Month + (1 + BA + Month|TaxaGroup) + (1|SampleID) + (1|Year), data=data_STBTgrouped, 
                                     family = negbinomial(),
                                     control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                     chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(STBT.brms.fulltaxa.negbinomA3)
summary(STBT.brms.fulltaxa.negbinomA3)
ranef(STBT.brms.fulltaxa.negbinomA3)
fixef(STBT.brms.fulltaxa.negbinomA3)
r2_bayes(STBT.brms.fulltaxa.negbinomA3, ci = 0.95)

save(STBT.brms.fulltaxa.negbinomA3, file = "./models/A2_sensitivity/STBT.brms.fulltaxa.negbinomA3.RData")



#Alternate way of doing the above, using a taxa-specific month random effect.
data_GULDgrouped$Month2 <- paste(data_GULDgrouped$Month, data_GULDgrouped$TaxaGroup, sep = '')
data_STBTgrouped$Month2 <- paste(data_STBTgrouped$Month, data_STBTgrouped$TaxaGroup, sep = '')


GULD.brms.fulltaxa.negbinomA4 <- brm(Count ~
                                       1 + BA + (1 + BA|TaxaGroup) + (1|SampleID) + (1|Year) + (1|Month2), data=data_GULDgrouped, 
                                     family = negbinomial(),
                                     control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                     chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(GULD.brms.fulltaxa.negbinomA4)
summary(GULD.brms.fulltaxa.negbinomA4)
ranef(GULD.brms.fulltaxa.negbinomA4)
fixef(GULD.brms.fulltaxa.negbinomA4)
r2_bayes(GULD.brms.fulltaxa.negbinomA4, ci = 0.95)

save(GULD.brms.fulltaxa.negbinomA4, file = "./models/A2_sensitivity/GULD.brms.fulltaxa.negbinomA4.RData")


STBT.brms.fulltaxa.negbinomA4 <- brm(Count ~
                                       1 + BA + Month + (1 + BA + Month|TaxaGroup) + (1|SampleID) + (1|Year) + (1|Month2), data=data_STBTgrouped, 
                                     family = negbinomial(),
                                     control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                     chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(STBT.brms.fulltaxa.negbinomA4)
summary(STBT.brms.fulltaxa.negbinomA4)
ranef(STBT.brms.fulltaxa.negbinomA4)
fixef(STBT.brms.fulltaxa.negbinomA4)
r2_bayes(STBT.brms.fulltaxa.negbinomA4, ci = 0.95)

save(STBT.brms.fulltaxa.negbinomA4, file = "./models/A2_sensitivity/STBT.brms.fulltaxa.negbinomA4.RData")




# Plotting comparison (Figure S2)
load("./models/GULD.brms.fulltaxa.negbinom.RData")
load("./models/STBT.brms.fulltaxa.negbinom.RData")
load("./models/A1_sensitivity/GULD.brms.fulltaxa.zerinfnb.RData")
load("./models/A1_sensitivity/STBT.brms.fulltaxa.zerinfnb.RData")
load("./models/A2_sensitivity/GULD.brms.fulltaxa.negbinomA2.RData")
load("./models/A2_sensitivity/STBT.brms.fulltaxa.negbinomA2.RData")


working1 <- as.data.frame(ranef(GULD.brms.fulltaxa.negbinom, groups = 'TaxaGroup'))
working2 <- as.data.frame(ranef(GULD.brms.fulltaxa.zerinfnb, groups = 'TaxaGroup'))
working3 <- as.data.frame(ranef(GULD.brms.fulltaxa.negbinomA4, groups = 'TaxaGroup'))

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

ggsave("./visualisations/SUPPfigA.jpg", width = 8, height = 16, units = "cm", SUPPfigA, dpi = 600)




working1 <- as.data.frame(ranef(STBT.brms.fulltaxa.negbinom, groups = 'TaxaGroup'))
working2 <- as.data.frame(ranef(STBT.brms.fulltaxa.zerinfnb, groups = 'TaxaGroup'))
working3 <- as.data.frame(ranef(STBT.brms.fulltaxa.negbinomA4, groups = 'TaxaGroup'))

#Building dataframe
data_SUPPfigB <- NULL
data_SUPPfigB$estimate1 <- working1$TaxaGroup.Estimate.BAbAfter
data_SUPPfigB$lower1 <- working1$TaxaGroup.Q2.5.BAbAfter
data_SUPPfigB$upper1 <- working1$TaxaGroup.Q97.5.BAbAfter
data_SUPPfigB$estimate2 <- working2$TaxaGroup.Estimate.BAbAfter
data_SUPPfigB$lower2 <- working2$TaxaGroup.Q2.5.BAbAfter
data_SUPPfigB$upper2 <- working2$TaxaGroup.Q97.5.BAbAfter
data_SUPPfigB$estimate3 <- working3$TaxaGroup.Estimate.BAbAfter
data_SUPPfigB$lower3 <- working3$TaxaGroup.Q2.5.BAbAfter
data_SUPPfigB$upper3 <- working3$TaxaGroup.Q97.5.BAbAfter

data_SUPPfigB$positions <- c(14.5, 1.0, 9.0, 23.5, 5.5, 
                            6.5, 13.5, 18.0, 17.0, 16.0, 
                            21.5, 22.5, 4.0, 11.0, 19.0, 
                            10.0, 12.0, 20.5, 2.5)
data_SUPPfigB$position1 <- data_SUPPfigB$positions + 0.45
data_SUPPfigB$position2 <- data_SUPPfigB$positions + 0.2
data_SUPPfigB$position3 <- data_SUPPfigB$positions - 0.05

data_SUPPfigB$labels <- rownames(working1)
data_SUPPfigB <- as.data.frame(data_SUPPfigB)


SUPPfigB <- ggplot(data_SUPPfigB, aes(x = estimate1, y = positions)) +
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
  geom_segment(x = data_SUPPfigB$lower1, y = data_SUPPfigB$position1, xend = data_SUPPfigB$upper1, yend = data_SUPPfigB$position1, size = 0.4) + 
  geom_point(x = data_SUPPfigB$estimate1, y = data_SUPPfigB$position1, shape = 19, size = 0.8) +
  geom_segment(colour = "dodgerblue2", x = data_SUPPfigB$lower2, y = data_SUPPfigB$position2, xend = data_SUPPfigB$upper2, yend = data_SUPPfigB$position2, size = 0.4) + 
  geom_point(colour = "dodgerblue2", x = data_SUPPfigB$estimate2, y = data_SUPPfigB$position2, shape = 19, size = 0.8) +
  geom_segment(colour = "firebrick2", x = data_SUPPfigB$lower3, y = data_SUPPfigB$position3, xend = data_SUPPfigB$upper3, yend = data_SUPPfigB$position3, size = 0.4) + 
  geom_point(colour = "firebrick2", x = data_SUPPfigB$estimate3, y = data_SUPPfigB$position3, shape = 19, size = 0.8) +
  geom_vline(xintercept = 0, linetype = 2, colour = "black", size = 0.5) +
  geom_hline(yintercept = c(2, 3.5, 5.0, 7.5, 13.0, 15.5, 20.0), linetype = 3, colour = "black", size = 0.25) +
  geom_text(aes(label=labels, fontface = 3), hjust = "left", x =-10.5, vjust=-1.2, size = 1.8) +
  labs(x = "Before-after impact effect (Stege Bugt)",
       y = "") 
SUPPfigB

ggsave("./visualisations/SUPPfigB.jpg", width = 8, height = 16, units = "cm", SUPPfigB, dpi = 600)
