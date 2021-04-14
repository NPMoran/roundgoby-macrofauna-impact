# Study: Impacts of the invasive round goby (Neogobius melanostomus) on benthic invertebrate macrofauna - a case study from the Baltic Sea
#
# Code authored by: Nicholas Moran, The Centre for Ocean Life- DTU Aqua, Technical University of Denmark
#                   Mikael van Deurs, DTU AQUA, Technical University of Denmark
# Date: March 2021


#### 2. Before-after taxa abundance analysis  ####
# Sampling in 2015 was conducted approx 2 months earlier than in previous seasons,
# Therefore, checking if similar responses are detected excluding 2015, or by including sampling month as a factor

Sys.setenv(LANG = "en")
library(dplyr); library(brms); library(performance)

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
VarCorr(GULD.brms.fulltaxa.negbinomA2)
ranef(GULD.brms.fulltaxa.negbinomA2)
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

save(STBT.brms.fulltaxa.negbinomA2, file = "./models/A2_sensitivity/STBT.brms.fulltaxa.negbinomA2b.RData")
