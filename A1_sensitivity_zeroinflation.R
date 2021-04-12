# Study: Impacts of the invasive round goby (Neogobius melanostomus) on benthic invertebrate macrofauna - a case study from the Baltic Sea
#
# Code authored by: Nicholas Moran, The Centre for Ocean Life- DTU Aqua, Technical University of Denmark
#                   Mikael van Deurs, DTU AQUA, Technical University of Denmark
# Date: March 2021



#### A1. Sensitivity Analysis -rerunning analysis zero inflated neg binomial distribution ####

Sys.setenv(LANG = "en")
library(dplyr); library(brms); library(performance)

#importing datasets
data_GULDgrouped <- read.csv("data_GULDgrouped.csv", strip.white=TRUE)
data_STBTgrouped <- read.csv("data_STBTgrouped.csv", strip.white=TRUE)

data_GULDgrouped$Year <- as.factor(data_GULDgrouped$Year)
data_STBTgrouped$Year <- as.factor(data_STBTgrouped$Year)


##BRMS full site models ----
#preliminary model specifications
adapt_delta_value <- 0.9999
max_treedepth_value <- 20
iterations <- 1000
burnin <- 200
thinning <- 2


#One model per site, using updated 20 taxonomic groups
# - run using negative binomial model, and zero-inflated negative binomial model
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



#Presence-Absence in gut content only ----
data_GULDgrouped <- read.csv("data_GULDgrouped.csv", strip.white=TRUE)

data_GULDgrouped$Presence <- if_else(data_GULDgrouped$TaxaGroup == 'Littorinimorpha (small)', 'Present', 'Absent')
data_GULDgrouped$Presence <- if_else(data_GULDgrouped$TaxaGroup == 'Littorinimorpha (large)', 'Present', data_GULDgrouped$Presence <- data_GULDgrouped$Presence)
data_GULDgrouped$Presence <- if_else(data_GULDgrouped$TaxaGroup == 'Neritidae', 'Present', data_GULDgrouped$Presence <- data_GULDgrouped$Presence)
data_GULDgrouped$Presence <- if_else(data_GULDgrouped$TaxaGroup == 'Cardiidae', 'Present', data_GULDgrouped$Presence <- data_GULDgrouped$Presence)
data_GULDgrouped$Presence <- if_else(data_GULDgrouped$TaxaGroup == 'Isopoda', 'Present', data_GULDgrouped$Presence <- data_GULDgrouped$Presence)
data_GULDgrouped$Presence <- if_else(data_GULDgrouped$TaxaGroup == 'Amphipoda', 'Present', data_GULDgrouped$Presence <- data_GULDgrouped$Presence)
data_GULDgrouped$Presence <- if_else(data_GULDgrouped$TaxaGroup == 'Myidae', 'Present', data_GULDgrouped$Presence <- data_GULDgrouped$Presence)


data=data_GULDgrouped_Present <- subset(data_GULDgrouped, Presence == 'Present')

GULD.brms.guttest.zerinfnb_Present <- brm(Count ~
                                            1 + BA + (BA|TaxaGroup) + (1|SampleID) + (1|Year), data=data_GULDgrouped_Present, 
                                          family = zero_inflated_negbinomial(),
                                          control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                          chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(GULD.brms.guttest.zerinfnb_Present)
summary(GULD.brms.guttest.zerinfnb_Present)
fixef(GULD.brms.guttest.zerinfnb_Present)
r2_bayes(GULD.brms.guttest.zerinfnb_Present, ci = 0.95)

save(GULD.brms.guttest.zerinfnb_Present, file = "./models/A1_sensitivity/GULD.brms.guttest.zerinfnb_Present.RData")


data=data_GULDgrouped_Absent <- subset(data_GULDgrouped, Presence == 'Absent')

GULD.brms.guttest.zerinfnb_Absent <- brm(Count ~
                                           1 + BA + (BA|TaxaGroup) + (1|SampleID) +(1|Year), data=data_GULDgrouped_Absent, 
                                         family = zero_inflated_negbinomial(),
                                         control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                         chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(GULD.brms.guttest.zerinfnb_Absent)
summary(GULD.brms.guttest.zerinfnb_Absent)
fixef(GULD.brms.guttest.zerinfnb_Absent)
r2_bayes(GULD.brms.guttest.zerinfnb_Absent, ci = 0.95)

save(GULD.brms.guttest.zerinfnb_Absent, file = "./models/A1_sensitivity/GULD.brms.guttest.zerinfnb_Absent.RData")


#Presence-Absence hypothesis testing ----
GULD.brms.guttest.zerinfnb_PresenceTEST <- brm(Count ~
                                                 1 + BA*Presence + (BA|TaxaGroup) + (1|SampleID) + (1|Year), data=data_GULDgrouped, 
                                               family = zero_inflated_negbinomial(),
                                               control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                               chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(GULD.brms.guttest.zerinfnb_PresenceTEST)
summary(GULD.brms.guttest.zerinfnb_PresenceTEST)
fixef(GULD.brms.guttest.zerinfnb_PresenceTEST)
r2_bayes(GULD.brms.guttest.zerinfnb_PresenceTEST, ci = 0.95)

save(GULD.brms.guttest.zerinfnb_PresenceTEST, file = "./models/A1_sensitivity/GULD.brms.guttest.zerinfnb_PresenceTEST.RData")