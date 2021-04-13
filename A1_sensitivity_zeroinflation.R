# Study: Impacts of the invasive round goby (Neogobius melanostomus) on benthic invertebrate macrofauna - a case study from the Baltic Sea
#
# Code authored by: Nicholas Moran, The Centre for Ocean Life- DTU Aqua, Technical University of Denmark
#                   Mikael van Deurs, DTU AQUA, Technical University of Denmark
# Date: March 2021



#### A1. Sensitivity Analysis -rerunning analysis zero inflated neg binomial distribution ####

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
iterations <- 6000
burnin <- 2000
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


#plotting comparison (Figure S2)
load("./models/GULD.brms.fulltaxa.negbinom.RData")
load("./models/STBT.brms.fulltaxa.negbinom.RData")
load("./A1_sensitivity/models/GULD.brms.fulltaxa.zerinfnb.RData")
load("./A1_sensitivity/models/STBT.brms.fulltaxa.zerinfnb.RData")


