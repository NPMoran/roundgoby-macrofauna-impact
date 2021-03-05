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


GULD.brms.origtaxa.zerinfnb <- brm(Count ~
                                        1 + BA + (1 + BA|Artsgruppering) + (1|Year), data=data_GULDgrouped, 
                                      family = zero_inflated_negbinomial(),
                                      control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                      chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(GULD.brms.origtaxa.zerinfnb)
summary(GULD.brms.origtaxa.zerinfnb)
coef(GULD.brms.origtaxa.zerinfnb, probs = c(0.05, 0.95))
r2_bayes(GULD.brms.origtaxa.zerinfnb)



STBT.brms.origtaxa.negbinom <- brm(Count ~
                                     1 + BA + (1 + BA|Artsgruppering) + (1|Year), data=data_STBTgrouped, 
                                   family = negbinomial(),
                                   control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                   chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(STBT.brms.origtaxa.negbinom)
summary(STBT.brms.origtaxa.negbinom)
coef(STBT.brms.origtaxa.negbinom, probs = c(0.05, 0.95))
r2_bayes(STBT.brms.origtaxa.negbinom)


STBT.brms.origtaxa.zerinfnb <- brm(Count ~
                                     1 + BA + (1 + BA|Artsgruppering) + (1|Year), data=data_STBTgrouped, 
                                   family = zero_inflated_negbinomial(),
                                   control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                   chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(STBT.brms.origtaxa.zerinfnb)
summary(STBT.brms.origtaxa.zerinfnb)
coef(STBT.brms.origtaxa.zerinfnb, probs = c(0.05, 0.95))
r2_bayes(STBT.brms.origtaxa.zerinfnb)


save(GULD.brms.origtaxa.negbinom, file = "./models/GULD.brms.origtaxa.negbinom.RData")
save(GULD.brms.origtaxa.zerinfnb, file = "./models/GULD.brms.origtaxa.zerinfnb.RData")

save(STBT.brms.origtaxa.negbinom, file = "./models/STBT.brms.origtaxa.negbinom.RData")
save(STBT.brms.origtaxa.zerinfnb, file = "./models/STBT.brms.origtaxa.zerinfnb.RData")



##Single model for both sites, using original 8 taxonomic groups
# - run using negative binomial model, and zero-inflated negative binomial model
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


DUAL.brms.origtaxa.zerinfnb <- brm(Count ~
                                     1 + BA + (1 + BA|Artsgruppering) + (1|Year) + (1|Site), data=data_DUALgrouped, 
                                   family = zero_inflated_negbinomial(),
                                   control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                   chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(DUAL.brms.origtaxa.zerinfnb)
summary(DUAL.brms.origtaxa.zerinfnb)
coef(DUAL.brms.origtaxa.zerinfnb, probs = c(0.05, 0.95))
r2_bayes(DUAL.brms.origtaxa.zerinfnb)


save(DUAL.brms.origtaxa.negbinom, file = "./models/DUAL.brms.origtaxa.negbinom.RData")
save(DUAL.brms.origtaxa.zerinfnb, file = "./models/DUAL.brms.origtaxa.zerinfnb.RData")




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


GULD.brms.fulltaxa.zerinfnb <- brm(Count ~
                                     1 + BA + (1 + BA|Artsgruppering) + (1|Year), data=data_GULDgrouped2, 
                                   family = zero_inflated_negbinomial(),
                                   control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                   chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(GULD.brms.fulltaxa.zerinfnb)
summary(GULD.brms.fulltaxa.zerinfnb)
coef(GULD.brms.fulltaxa.zerinfnb, probs = c(0.05, 0.95))
r2_bayes(GULD.brms.fulltaxa.zerinfnb)



STBT.brms.fulltaxa.negbinom <- brm(Count ~
                                     1 + BA + (1 + BA|Artsgruppering) + (1|Year), data=data_STBTgrouped2, 
                                   family = negbinomial(),
                                   control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                   chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(STBT.brms.fulltaxa.negbinom)
summary(STBT.brms.fulltaxa.negbinom)
coef(STBT.brms.fulltaxa.negbinom, probs = c(0.05, 0.95))
r2_bayes(STBT.brms.fulltaxa.negbinom)


STBT.brms.fulltaxa.zerinfnb <- brm(Count ~
                                     1 + BA + (1 + BA|Artsgruppering) + (1|Year), data=data_STBTgrouped2, 
                                   family = zero_inflated_negbinomial(),
                                   control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                   chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(STBT.brms.fulltaxa.zerinfnb)
summary(STBT.brms.fulltaxa.zerinfnb)
coef(STBT.brms.fulltaxa.zerinfnb, probs = c(0.05, 0.95))
r2_bayes(STBT.brms.fulltaxa.zerinfnb)


save(GULD.brms.fulltaxa.negbinom, file = "./models/GULD.brms.fulltaxa.negbinom.RData")
save(GULD.brms.fulltaxa.zerinfnb, file = "./models/GULD.brms.fulltaxa.zerinfnb.RData")

save(STBT.brms.fulltaxa.negbinom, file = "./models/STBT.brms.fulltaxa.negbinom.RData")
save(STBT.brms.fulltaxa.zerinfnb, file = "./models/STBT.brms.fulltaxa.zerinfnb.RData")



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


DUAL.brms.fulltaxa.zerinfnb <- brm(Count ~
                                     1 + BA + (1 + BA|Artsgruppering) + (1|Year) + (1|Site), data=data_DUALgrouped2, 
                                   family = zero_inflated_negbinomial(),
                                   control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                   chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(DUAL.brms.fulltaxa.zerinfnb)
summary(DUAL.brms.fulltaxa.zerinfnb)
coef(DUAL.brms.fulltaxa.zerinfnb, probs = c(0.05, 0.95))
r2_bayes(DUAL.brms.fulltaxa.zerinfnb)


save(DUAL.brms.fulltaxa.negbinom, file = "./models/DUAL.brms.fulltaxa.negbinom.RData")
save(DUAL.brms.fulltaxa.zerinfnb, file = "./models/DUAL.brms.fulltaxa.zerinfnb.RData")

