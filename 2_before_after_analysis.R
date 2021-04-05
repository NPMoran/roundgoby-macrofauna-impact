# Study: Impacts of the invasive round goby (Neogobius melanostomus) on benthic invertebrate macrofauna - a case study from the Baltic Sea
#
# Code authored by: Nicholas Moran, The Centre for Ocean Life- DTU Aqua, Technical University of Denmark
#                   Mikael van Deurs, DTU AQUA, Technical University of Denmark
# Date: March 2021



#### 2. Before-after taxa abundance analysis ####

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
iterations <- 6000
burnin <- 3000
thinning <- 2


#One model per site, using updated 20 taxonomic groups
# - run using negative binomial model, and zero-inflated negative binomial model
GULD.brms.fulltaxa.negbinom <- brm(Count ~
                                     1 + BA + (1 + BA|TaxaGroup) + (1|SampleID) + (1|Year), data=data_GULDgrouped, 
                                   family = negbinomial(),
                                   control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                   chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(GULD.brms.fulltaxa.negbinom)
summary(GULD.brms.fulltaxa.negbinom)
ranef(GULD.brms.fulltaxa.negbinom)
fixef(GULD.brms.fulltaxa.negbinom)
r2_bayes(GULD.brms.fulltaxa.negbinom, ci = 0.95)

save(GULD.brms.fulltaxa.negbinom, file = "./models/GULD.brms.fulltaxa.negbinom.RData")


STBT.brms.fulltaxa.negbinom <- brm(Count ~
                                     1 + BA + (1 + BA|TaxaGroup) + (1|SampleID) + (1|Year), data=data_STBTgrouped, 
                                   family = negbinomial(),
                                   control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                   chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(STBT.brms.fulltaxa.negbinom)
summary(STBT.brms.fulltaxa.negbinom)
ranef(STBT.brms.fulltaxa.negbinom)
fixef(STBT.brms.fulltaxa.negbinom)
r2_bayes(STBT.brms.fulltaxa.negbinom, ci = 0.95)

save(STBT.brms.fulltaxa.negbinom, file = "./models/STBT.brms.fulltaxa.negbinom.RData")

