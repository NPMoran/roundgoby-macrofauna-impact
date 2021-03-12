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

data_GULDgrouped2 <- read.csv("data_GULDgrouped2.csv", strip.white=TRUE)
data_STBTgrouped2 <- read.csv("data_STBTgrouped2.csv", strip.white=TRUE)

data_GULDgrouped2$Year <- as.factor(data_GULDgrouped2$Year)
data_STBTgrouped2$Year <- as.factor(data_STBTgrouped2$Year)


##BRMS full site models ----
#preliminary model specifications
adapt_delta_value <- 0.9999
max_treedepth_value <- 20
iterations <- 6000
burnin <- 3000
thinning <- 2

##One model per site, using original 8 taxonomic groups
# - run using negative binomial model, and zero-inflated negative binomial model
GULD.brms.origtaxa.negbinom <- brm(Count ~
                                     1 + BA + (1 + BA|Artsgruppering) + (1|Year), data=data_GULDgrouped, 
                                   family = negbinomial(),
                                   control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                   chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(GULD.brms.origtaxa.negbinom)
summary(GULD.brms.origtaxa.negbinom)
coef(GULD.brms.origtaxa.negbinom, probs = c(0.05, 0.95))
r2_bayes(GULD.brms.origtaxa.negbinom)


STBT.brms.origtaxa.negbinom <- brm(Count ~
                                     1 + BA + (1 + BA|Artsgruppering) + (1|Year), data=data_STBTgrouped, 
                                   family = negbinomial(),
                                   control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                   chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(STBT.brms.origtaxa.negbinom)
summary(STBT.brms.origtaxa.negbinom)
coef(STBT.brms.origtaxa.negbinom, probs = c(0.05, 0.95))
r2_bayes(STBT.brms.origtaxa.negbinom)


save(GULD.brms.origtaxa.negbinom, file = "./models/GULD.brms.origtaxa.negbinom.RData")
save(STBT.brms.origtaxa.negbinom, file = "./models/STBT.brms.origtaxa.negbinom.RData")



##Single model for both sites, using original 8 taxonomic groups
# - run using negative binomial model
data_GULDgrouped$Site <- "GULD"
data_STBTgrouped$Site <- "STBT"
data_DUALgrouped <- rbind(data_GULDgrouped, data_STBTgrouped)

DUAL.brms.origtaxa.negbinom <- brm(Count ~
                                     1 + BA + (1 + BA|Artsgruppering) + (1|Year) + (1|Site), data=data_DUALgrouped, 
                                   family = negbinomial(),
                                   control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                   chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(DUAL.brms.origtaxa.negbinom)
summary(DUAL.brms.origtaxa.negbinom)
coef(DUAL.brms.origtaxa.negbinom, probs = c(0.05, 0.95))
r2_bayes(DUAL.brms.origtaxa.negbinom)


save(DUAL.brms.origtaxa.negbinom, file = "./models/DUAL.brms.origtaxa.negbinom.RData")



#One model per site, using updated 20 taxonomic groups
# - run using negative binomial model, and zero-inflated negative binomial model
GULD.brms.fulltaxa.negbinom <- brm(Count ~
                                     1 + BA + (1 + BA|Artsgruppering) + (1|Year), data=data_GULDgrouped2, 
                                   family = negbinomial(),
                                   control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                   chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(GULD.brms.fulltaxa.negbinom)
summary(GULD.brms.fulltaxa.negbinom)
coef(GULD.brms.fulltaxa.negbinom, probs = c(0.05, 0.95))
r2_bayes(GULD.brms.fulltaxa.negbinom)


STBT.brms.fulltaxa.negbinom <- brm(Count ~
                                     1 + BA + (1 + BA|Artsgruppering) + (1|Year), data=data_STBTgrouped2, 
                                   family = negbinomial(),
                                   control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                   chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(STBT.brms.fulltaxa.negbinom)
summary(STBT.brms.fulltaxa.negbinom)
coef(STBT.brms.fulltaxa.negbinom, probs = c(0.05, 0.95))
r2_bayes(STBT.brms.fulltaxa.negbinom)


save(GULD.brms.fulltaxa.negbinom, file = "./models/GULD.brms.fulltaxa.negbinom.RData")
save(STBT.brms.fulltaxa.negbinom, file = "./models/STBT.brms.fulltaxa.negbinom.RData")



#Single model for both sites, using updated 20 taxonomic groups
# - run using negative binomial model, and zero-inflated negative binomial model
data_GULDgrouped2$Site <- "GULD"
data_STBTgrouped2$Site <- "STBT"
data_DUALgrouped2 <- rbind(data_GULDgrouped2, data_STBTgrouped2)

DUAL.brms.fulltaxa.negbinom <- brm(Count ~
                                     1 + BA + (1 + BA|Artsgruppering) + (1|Year) + (1|Site), data=data_DUALgrouped2, 
                                   family = negbinomial(),
                                   control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                   chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(DUAL.brms.fulltaxa.negbinom)
summary(DUAL.brms.fulltaxa.negbinom)
coef(DUAL.brms.fulltaxa.negbinom, probs = c(0.05, 0.95))
r2_bayes(DUAL.brms.fulltaxa.negbinom)


save(DUAL.brms.fulltaxa.negbinom, file = "./models/DUAL.brms.fulltaxa.negbinom.RData")

