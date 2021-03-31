# Study: Impacts of the invasive round goby (Neogobius melanostomus) on benthic invertebrate macrofauna - a case study from the Baltic Sea
#
# Code authored by: Nicholas Moran, The Centre for Ocean Life- DTU Aqua, Technical University of Denmark
#                   Mikael van Deurs, DTU AQUA, Technical University of Denmark
# Date: March 2021



#### A1. Sensitivity Analysis -rerunning analysis alternate distributions ####

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
iterations <- 2000
burnin <- 500
thinning <- 2

## Zero-inflated negative binomial model ----
GULD.brms.fulltaxa.zerinfnb <- brm(Count ~
                                     1 + BA + (1 + BA|TaxaGroup) + (1|Year), data=data_GULDgrouped, 
                                   family = zero_inflated_negbinomial(),
                                   control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                   chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(GULD.brms.fulltaxa.zerinfnb)
summary(GULD.brms.fulltaxa.zerinfnb)
ranef(GULD.brms.fulltaxa.zerinfnb, probs = c(0.05, 0.95))
fixef(GULD.brms.fulltaxa.zerinfnb, probs = c(0.05, 0.95))
r2_bayes(GULD.brms.fulltaxa.zerinfnb, ci = 0.95)

save(GULD.brms.fulltaxa.zerinfnb, file = "./models/A1_sensitivity/GULD.brms.fulltaxa.zerinfnb.RData")


STBT.brms.fulltaxa.zerinfnb <- brm(Count ~
                                     1 + BA + (1 + BA|TaxaGroup) + (1|Year), data=data_STBTgrouped, 
                                   family = zero_inflated_negbinomial(),
                                   control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                   chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(STBT.brms.fulltaxa.zerinfnb)
summary(STBT.brms.fulltaxa.zerinfnb)
ranef(STBT.brms.fulltaxa.zerinfnb, probs = c(0.05, 0.95))
fixef(STBT.brms.fulltaxa.zerinfnb, probs = c(0.05, 0.95))
r2_bayes(STBT.brms.fulltaxa.zerinfnb, ci = 0.95)

save(STBT.brms.fulltaxa.zerinfnb, file = "./models/A1_sensitivity/STBT.brms.fulltaxa.zerinfnb.RData")


data_GULDgrouped$GutPresence <- if_else(data_GULDgrouped$TaxaGroup == 'Littorinimorpha (small)', 'Preferred', 'NonPreferred')
data_GULDgrouped$GutPresence <- if_else(data_GULDgrouped$TaxaGroup == 'Littorinimorpha (large)', 'Preferred', data_GULDgrouped$GutPresence <- data_GULDgrouped$GutPresence)
data_GULDgrouped$GutPresence <- if_else(data_GULDgrouped$TaxaGroup == 'Neritidae', 'Preferred', data_GULDgrouped$GutPresence <- data_GULDgrouped$GutPresence)
data_GULDgrouped$GutPresence <- if_else(data_GULDgrouped$TaxaGroup == 'Cardiidae', 'Preferred', data_GULDgrouped$GutPresence <- data_GULDgrouped$GutPresence)
data_GULDgrouped$GutPresence <- if_else(data_GULDgrouped$TaxaGroup == 'Isopoda', 'Preferred', data_GULDgrouped$GutPresence <- data_GULDgrouped$GutPresence)
data=data_GULDgrouped_Preferred <- subset(data_GULDgrouped, GutPresence == 'Preferred')

GULD.brms.guttest.zerinfnb_Preferred <- brm(Count ~
                                              1 + BA + (BA|TaxaGroup) + (1|Year), data=data_GULDgrouped_Preferred, 
                                            family = zero_inflated_negbinomial(),
                                            control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                            chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(GULD.brms.guttest.zerinfnb_Preferred)
summary(GULD.brms.guttest.zerinfnb_Preferred)
fixef(GULD.brms.guttest.zerinfnb_Preferred, probs = c(0.05, 0.95))
r2_bayes(GULD.brms.guttest.zerinfnb_Preferred, ci = 0.95)

save(GULD.brms.guttest.zerinfnb_Preferred, file = "./models/A1_sensitivity/GULD.brms.guttest.zerinfnb_Preferred.RData")


data=data_GULDgrouped2_NonPreferred <- subset(data_GULDgrouped, GutPresence == 'NonPreferred')

GULD.brms.guttest.zerinfnb_NonPreferred <- brm(Count ~
                                                 1 + BA + (BA|TaxaGroup) + (1|Year), data=data_GULDgrouped2_NonPreferred, 
                                               family = zero_inflated_negbinomial(),
                                               control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                               chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(GULD.brms.guttest.zerinfnb_NonPreferred)
summary(GULD.brms.guttest.zerinfnb_NonPreferred)
fixef(GULD.brms.guttest.zerinfnb_NonPreferred, probs = c(0.05, 0.95))
r2_bayes(GULD.brms.guttest.zerinfnb_NonPreferred, ci = 0.95)

save(GULD.brms.guttest.zerinfnb_NonPreferred, file = "./models/A1_sensitivity/GULD.brms.guttest.zerinfnb_NonPreferred.RData")


## Zero-Inflated Poisson Distribution
GULD.brms.fulltaxa.zerinfps <- brm(Count ~
                                     1 + BA + (1 + BA|TaxaGroup) + (1|Year), data=data_GULDgrouped, 
                                   family = zero_inflated_poisson(),
                                   control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                   chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(GULD.brms.fulltaxa.zerinfps)
summary(GULD.brms.fulltaxa.zerinfps)
ranef(GULD.brms.fulltaxa.zerinfps, probs = c(0.05, 0.95))
fixef(GULD.brms.fulltaxa.zerinfps, probs = c(0.05, 0.95))
r2_bayes(GULD.brms.fulltaxa.zerinfps, ci = 0.95)

save(GULD.brms.fulltaxa.zerinfps, file = "./models/GULD.brms.fulltaxa.zerinfps.RData")


STBT.brms.fulltaxa.zerinfps <- brm(Count ~
                                     1 + BA + (1 + BA|TaxaGroup) + (1|Year), data=data_STBTgrouped, 
                                   family = zero_inflated_poisson(),
                                   control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                   chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(STBT.brms.fulltaxa.zerinfps)
summary(STBT.brms.fulltaxa.zerinfps)
ranef(STBT.brms.fulltaxa.zerinfps, probs = c(0.05, 0.95))
fixef(STBT.brms.fulltaxa.zerinfps, probs = c(0.05, 0.95))
r2_bayes(STBT.brms.fulltaxa.zerinfps, ci = 0.95)

save(STBT.brms.fulltaxa.zerinfps, file = "./models/STBT.brms.fulltaxa.zerinfps.RData")


GULD.brms.guttest.zerinfps_Preferred <- brm(Count ~
                                              1 + BA + (BA|TaxaGroup) + (1|Year), data=data_GULDgrouped_Preferred, 
                                            family = zero_inflated_poisson(),
                                            control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                            chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(GULD.brms.guttest.zerinfps_Preferred)
summary(GULD.brms.guttest.zerinfps_Preferred)
fixef(GULD.brms.guttest.zerinfps_Preferred, probs = c(0.05, 0.95))
r2_bayes(GULD.brms.guttest.zerinfps_Preferred, ci = 0.95)

save(GULD.brms.guttest.zerinfps_Preferred, file = "./models/A1_sensitivity/GULD.brms.guttest.zerinfps_Preferred.RData")


GULD.brms.guttest.zerinfps_NonPreferred <- brm(Count ~
                                                 1 + BA + (BA|TaxaGroup) + (1|Year), data=data_GULDgrouped2_NonPreferred, 
                                               family = zero_inflated_poisson(),
                                               control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                               chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(GULD.brms.guttest.zerinfps_NonPreferred)
summary(GULD.brms.guttest.zerinfps_NonPreferred)
fixef(GULD.brms.guttest.zerinfps_NonPreferred, probs = c(0.05, 0.95))
r2_bayes(GULD.brms.guttest.zerinfps_NonPreferred, ci = 0.95)
?r2_bayes
save(GULD.brms.guttest.zerinfps_NonPreferred, file = "./models/A1_sensitivity/GULD.brms.guttest.zerinfps_NonPreferred.RData")
bayes

