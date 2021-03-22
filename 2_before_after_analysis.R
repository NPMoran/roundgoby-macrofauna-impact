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
ranef(GULD.brms.origtaxa.negbinom, probs = c(0.05, 0.95))
r2_bayes(GULD.brms.origtaxa.negbinom, ci = 0.95)


STBT.brms.origtaxa.negbinom <- brm(Count ~
                                     1 + BA + (1 + BA|Artsgruppering) + (1|Year), data=data_STBTgrouped, 
                                   family = negbinomial(),
                                   control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                   chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(STBT.brms.origtaxa.negbinom)
summary(STBT.brms.origtaxa.negbinom)
ranef(STBT.brms.origtaxa.negbinom, probs = c(0.05, 0.95))
r2_bayes(STBT.brms.origtaxa.negbinom, ci = 0.95)


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
ranef(DUAL.brms.origtaxa.negbinom, probs = c(0.05, 0.95))
r2_bayes(DUAL.brms.origtaxa.negbinom, ci = 0.95)


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
ranef(GULD.brms.fulltaxa.negbinom, probs = c(0.05, 0.95))
r2_bayes(GULD.brms.fulltaxa.negbinom, ci = 0.95)


STBT.brms.fulltaxa.negbinom <- brm(Count ~
                                     1 + BA + (1 + BA|Artsgruppering) + (1|Year), data=data_STBTgrouped2, 
                                   family = negbinomial(),
                                   control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                   chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(STBT.brms.fulltaxa.negbinom)
summary(STBT.brms.fulltaxa.negbinom)
ranef(STBT.brms.fulltaxa.negbinom, probs = c(0.05, 0.95))
r2_bayes(STBT.brms.fulltaxa.negbinom, ci = 0.95)

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
ranef(DUAL.brms.fulltaxa.negbinom, probs = c(0.05, 0.95))
r2_bayes(DUAL.brms.fulltaxa.negbinom, ci = 0.95)


save(DUAL.brms.fulltaxa.negbinom, file = "./models/DUAL.brms.fulltaxa.negbinom.RData")



##BRMS single taxa models ----
# I. Amphipoda (Order) 
data_GULDgrouped2.taxa01 <- subset(data_GULDgrouped2, Artsgruppering == "Amphipoda")
GULD.brms.taxa01.negbinom <- brm(Count ~
                                     1 + BA + (1|Year), data=data_GULDgrouped2.taxa01, 
                                   family = negbinomial(),
                                   control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                   chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(GULD.brms.taxa01.negbinom)
summary(GULD.brms.taxa01.negbinom)
fixef(GULD.brms.taxa01.negbinom, probs = c(0.05, 0.95))
r2_bayes(GULD.brms.taxa01.negbinom, ci = 0.95, ci = 0.95)

data_STBTgrouped2.taxa01 <- subset(data_STBTgrouped2, Artsgruppering == "Amphipoda")
STBT.brms.taxa01.negbinom <- brm(Count ~
                                   1 + BA + (1|Year), data=data_STBTgrouped2.taxa01, 
                                 family = negbinomial(),
                                 control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                 chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(STBT.brms.taxa01.negbinom)
summary(STBT.brms.taxa01.negbinom)
fixef(STBT.brms.taxa01.negbinom, probs = c(0.05, 0.95))
r2_bayes(STBT.brms.taxa01.negbinom, ci = 0.95)

save(GULD.brms.taxa01.negbinom, file = "./models/GULD.brms.taxa01.negbinom.RData")
save(STBT.brms.taxa01.negbinom, file = "./models/STBT.brms.taxa01.negbinom.RData")


# II. Isopoda (Order)
data_GULDgrouped2.taxa02 <- subset(data_GULDgrouped2, Artsgruppering == "Isopoda")
GULD.brms.taxa02.negbinom <- brm(Count ~
                                   1 + BA + (1|Year), data=data_GULDgrouped2.taxa02, 
                                 family = negbinomial(),
                                 control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                 chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(GULD.brms.origtaxa.negbinom)
summary(GULD.brms.taxa02.negbinom)
fixef(GULD.brms.taxa02.negbinom, probs = c(0.05, 0.95))
r2_bayes(GULD.brms.taxa02.negbinom, ci = 0.95)

data_STBTgrouped2.taxa02 <- subset(data_STBTgrouped2, Artsgruppering == "Isopoda")
STBT.brms.taxa02.negbinom <- brm(Count ~
                                   1 + BA + (1|Year), data=data_STBTgrouped2.taxa02, 
                                 family = negbinomial(),
                                 control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                 chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(STBT.brms.origtaxa.negbinom)
summary(STBT.brms.taxa02.negbinom)
fixef(STBT.brms.taxa02.negbinom, probs = c(0.05, 0.95))
r2_bayes(STBT.brms.taxa02.negbinom, ci = 0.95)

save(GULD.brms.taxa02.negbinom, file = "./models/GULD.brms.taxa02.negbinom.RData")
save(STBT.brms.taxa02.negbinom, file = "./models/STBT.brms.taxa02.negbinom.RData")


# III. Littorinimorpha (small) (Order)
data_GULDgrouped2.taxa03 <- subset(data_GULDgrouped2, Artsgruppering == "Littorinimorpha (small)")
GULD.brms.taxa03.negbinom <- brm(Count ~
                                   1 + BA + (1|Year), data=data_GULDgrouped2.taxa03, 
                                 family = negbinomial(),
                                 control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                 chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(GULD.brms.taxa03.negbinom)
summary(GULD.brms.taxa03.negbinom)
fixef(GULD.brms.taxa03.negbinom, probs = c(0.05, 0.95))
r2_bayes(GULD.brms.taxa03.negbinom, ci = 0.95)

data_STBTgrouped2.taxa03 <- subset(data_STBTgrouped2, Artsgruppering == "Littorinimorpha (small)")
STBT.brms.taxa03.negbinom <- brm(Count ~
                                   1 + BA + (1|Year), data=data_STBTgrouped2.taxa03, 
                                 family = negbinomial(),
                                 control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                 chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(STBT.brms.taxa03.negbinom)
summary(STBT.brms.taxa03.negbinom)
fixef(STBT.brms.taxa03.negbinom, probs = c(0.05, 0.95))
r2_bayes(STBT.brms.taxa03.negbinom, ci = 0.95)

save(GULD.brms.taxa03.negbinom, file = "./models/GULD.brms.taxa03.negbinom.RData")
save(STBT.brms.taxa03.negbinom, file = "./models/STBT.brms.taxa03.negbinom.RData")


# IV. Littorinimorpha (large) (Order)
data_GULDgrouped2.taxa04 <- subset(data_GULDgrouped2, Artsgruppering == "Littorinimorpha (large)")
GULD.brms.taxa04.negbinom <- brm(Count ~
                                   1 + BA + (1|Year), data=data_GULDgrouped2.taxa04, 
                                 family = negbinomial(),
                                 control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                 chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(GULD.brms.taxa04.negbinom)
summary(GULD.brms.taxa04.negbinom)
fixef(GULD.brms.taxa04.negbinom, probs = c(0.05, 0.95))
r2_bayes(GULD.brms.taxa04.negbinom, ci = 0.95)

data_STBTgrouped2.taxa04 <- subset(data_STBTgrouped2, Artsgruppering == "Littorinimorpha (large)")
STBT.brms.taxa04.negbinom <- brm(Count ~
                                   1 + BA + (1|Year), data=data_STBTgrouped2.taxa04, 
                                 family = negbinomial(),
                                 control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                 chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(STBT.brms.taxa04.negbinom)
summary(STBT.brms.taxa04.negbinom)
fixef(STBT.brms.taxa04.negbinom, probs = c(0.05, 0.95))
r2_bayes(STBT.brms.taxa04.negbinom, ci = 0.95)

save(GULD.brms.taxa04.negbinom, file = "./models/GULD.brms.taxa04.negbinom.RData")
save(STBT.brms.taxa04.negbinom, file = "./models/STBT.brms.taxa04.negbinom.RData")


# V. Lymnaeidae (Family)
data_GULDgrouped2.taxa05 <- subset(data_GULDgrouped2, Artsgruppering == "Lymnaeidae")
GULD.brms.taxa05.negbinom <- brm(Count ~
                                   1 + BA + (1|Year), data=data_GULDgrouped2.taxa05, 
                                 family = negbinomial(),
                                 control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                 chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(GULD.brms.taxa05.negbinom)
summary(GULD.brms.taxa05.negbinom)
fixef(GULD.brms.taxa05.negbinom, probs = c(0.05, 0.95))
r2_bayes(GULD.brms.taxa05.negbinom, ci = 0.95)

data_STBTgrouped2.taxa05 <- subset(data_STBTgrouped2, Artsgruppering == "Lymnaeidae")
STBT.brms.taxa05.negbinom <- brm(Count ~
                                   1 + BA + (1|Year), data=data_STBTgrouped2.taxa05, 
                                 family = negbinomial(),
                                 control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                 chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(STBT.brms.taxa05.negbinom)
summary(STBT.brms.taxa05.negbinom)
fixef(STBT.brms.taxa05.negbinom, probs = c(0.05, 0.95))
r2_bayes(STBT.brms.taxa05.negbinom, ci = 0.95)

save(GULD.brms.taxa05.negbinom, file = "./models/GULD.brms.taxa05.negbinom.RData")
save(STBT.brms.taxa05.negbinom, file = "./models/STBT.brms.taxa05.negbinom.RData")


# VI. Neritidae [Family)
data_GULDgrouped2.taxa06 <- subset(data_GULDgrouped2, Artsgruppering == "Neritidae")
GULD.brms.taxa06.negbinom <- brm(Count ~
                                   1 + BA + (1|Year), data=data_GULDgrouped2.taxa06, 
                                 family = negbinomial(),
                                 control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                 chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(GULD.brms.taxa06.negbinom)
summary(GULD.brms.taxa06.negbinom)
fixef(GULD.brms.taxa06.negbinom, probs = c(0.05, 0.95))
r2_bayes(GULD.brms.taxa06.negbinom, ci = 0.95)

data_STBTgrouped2.taxa06 <- subset(data_STBTgrouped2, Artsgruppering == "Neritidae")
STBT.brms.taxa06.negbinom <- brm(Count ~
                                   1 + BA + (1|Year), data=data_STBTgrouped2.taxa06, 
                                 family = zero_inflated_negbinomial(),
                                 control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                 chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(STBT.brms.origtaxa.negbinom)
summary(STBT.brms.taxa06.negbinom)
fixef(STBT.brms.taxa06.negbinom, probs = c(0.05, 0.95))
r2_bayes(STBT.brms.taxa06.negbinom, ci = 0.95)

save(GULD.brms.taxa06.negbinom, file = "./models/GULD.brms.taxa06.negbinom.RData")
save(STBT.brms.taxa06.negbinom, file = "./models/STBT.brms.taxa06.negbinom.RData")


load("./models/GULD.brms.taxa06.negbinom.RData")


# VII. Cardiidae,[Family)
data_GULDgrouped2.taxa07 <- subset(data_GULDgrouped2, Artsgruppering == "Cardiidae")
GULD.brms.taxa07.negbinom <- brm(Count ~
                                   1 + BA + (1|Year), data=data_GULDgrouped2.taxa07, 
                                 family = negbinomial(),
                                 control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                 chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(GULD.brms.taxa07.negbinom)
summary(GULD.brms.taxa07.negbinom)
fixef(GULD.brms.taxa07.negbinom, probs = c(0.05, 0.95))
r2_bayes(GULD.brms.taxa07.negbinom, ci = 0.95)

data_STBTgrouped2.taxa07 <- subset(data_STBTgrouped2, Artsgruppering == "Cardiidae")
STBT.brms.taxa07.negbinom <- brm(Count ~
                                   1 + BA + (1|Year), data=data_STBTgrouped2.taxa07, 
                                 family = negbinomial(),
                                 control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                 chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(STBT.brms.origtaxa.negbinom)
summary(STBT.brms.taxa07.negbinom)
fixef(STBT.brms.taxa07.negbinom, probs = c(0.05, 0.95))
r2_bayes(STBT.brms.taxa07.negbinom, ci = 0.95)

save(GULD.brms.taxa07.negbinom, file = "./models/GULD.brms.taxa07.negbinom.RData")
save(STBT.brms.taxa07.negbinom, file = "./models/STBT.brms.taxa07.negbinom.RData")


# VIII. Mytilidae [Family)
data_GULDgrouped2.taxa08 <- subset(data_GULDgrouped2, Artsgruppering == "Mytilidae")
GULD.brms.taxa08.negbinom <- brm(Count ~
                                   1 + BA + (1|Year), data=data_GULDgrouped2.taxa08, 
                                 family = negbinomial(),
                                 control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                 chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(GULD.brms.taxa08.negbinom)
summary(GULD.brms.taxa08.negbinom)
fixef(GULD.brms.taxa08.negbinom, probs = c(0.05, 0.95))
r2_bayes(GULD.brms.taxa08.negbinom, ci = 0.95)

data_STBTgrouped2.taxa08 <- subset(data_STBTgrouped2, Artsgruppering == "Mytilidae")
STBT.brms.taxa08.negbinom <- brm(Count ~
                                   1 + BA + (1|Year), data=data_STBTgrouped2.taxa08, 
                                 family = negbinomial(),
                                 control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                 chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(STBT.brms.taxa08.negbinom)
summary(STBT.brms.taxa08.negbinom)
fixef(STBT.brms.taxa08.negbinom, probs = c(0.05, 0.95))
r2_bayes(STBT.brms.taxa08.negbinom, ci = 0.95)

save(GULD.brms.taxa08.negbinom, file = "./models/GULD.brms.taxa08.negbinom.RData")
save(STBT.brms.taxa08.negbinom, file = "./models/STBT.brms.taxa08.negbinom.RData")


# IX. Tellinidae [Family)
data_GULDgrouped2.taxa09 <- subset(data_GULDgrouped2, Artsgruppering == "Tellinidae")
GULD.brms.taxa09.negbinom <- brm(Count ~
                                   1 + BA + (1|Year), data=data_GULDgrouped2.taxa09, 
                                 family = negbinomial(),
                                 control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                 chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(GULD.brms.taxa09.negbinom)
summary(GULD.brms.taxa09.negbinom)
fixef(GULD.brms.taxa09.negbinom, probs = c(0.05, 0.95))
r2_bayes(GULD.brms.taxa09.negbinom, ci = 0.95)

data_STBTgrouped2.taxa09 <- subset(data_STBTgrouped2, Artsgruppering == "Tellinidae")
STBT.brms.taxa09.negbinom <- brm(Count ~
                                   1 + BA + (1|Year), data=data_STBTgrouped2.taxa09, 
                                 family = negbinomial(),
                                 control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                 chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(STBT.brms.taxa09.negbinom)
summary(STBT.brms.taxa09.negbinom)
fixef(STBT.brms.taxa09.negbinom, probs = c(0.05, 0.95))
r2_bayes(STBT.brms.taxa09.negbinom, ci = 0.95)

save(GULD.brms.taxa09.negbinom, file = "./models/GULD.brms.taxa09.negbinom.RData")
save(STBT.brms.taxa09.negbinom, file = "./models/STBT.brms.taxa09.negbinom.RData")


# X. Myidae [Family)
data_GULDgrouped2.taxa10 <- subset(data_GULDgrouped2, Artsgruppering == "Myidae")
GULD.brms.taxa10.negbinom <- brm(Count ~
                                   1 + BA + (1|Year), data=data_GULDgrouped2.taxa10, 
                                 family = negbinomial(),
                                 control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                 chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(GULD.brms.taxa10.negbinom)
summary(GULD.brms.taxa10.negbinom)
fixef(GULD.brms.taxa10.negbinom, probs = c(0.05, 0.95))
r2_bayes(GULD.brms.taxa10.negbinom, ci = 0.95)

data_STBTgrouped2.taxa10 <- subset(data_STBTgrouped2, Artsgruppering == "Myidae")
STBT.brms.taxa10.negbinom <- brm(Count ~
                                   1 + BA + (1|Year), data=data_STBTgrouped2.taxa10, 
                                 family = negbinomial(),
                                 control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                 chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(STBT.brms.taxa10.negbinom)
summary(STBT.brms.taxa10.negbinom)
fixef(STBT.brms.taxa10.negbinom, probs = c(0.05, 0.95))
r2_bayes(STBT.brms.taxa10.negbinom, ci = 0.95)

save(GULD.brms.taxa10.negbinom, file = "./models/GULD.brms.taxa10.negbinom.RData")
save(STBT.brms.taxa10.negbinom, file = "./models/STBT.brms.taxa10.negbinom.RData")


# XI. Nereididae [Family)
data_GULDgrouped2.taxa11 <- subset(data_GULDgrouped2, Artsgruppering == "Nereididae")
GULD.brms.taxa11.negbinom <- brm(Count ~
                                   1 + BA + (1|Year), data=data_GULDgrouped2.taxa11, 
                                 family = negbinomial(),
                                 control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                 chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(GULD.brms.taxa11.negbinom)
summary(GULD.brms.taxa11.negbinom)
fixef(GULD.brms.taxa11.negbinom, probs = c(0.05, 0.95))
r2_bayes(GULD.brms.taxa11.negbinom, ci = 0.95)

data_STBTgrouped2.taxa11 <- subset(data_STBTgrouped2, Artsgruppering == "Nereididae")
STBT.brms.taxa11.negbinom <- brm(Count ~
                                   1 + BA + (1|Year), data=data_STBTgrouped2.taxa11, 
                                 family = negbinomial(),
                                 control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                 chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(STBT.brms.taxa11.negbinom)
summary(STBT.brms.taxa11.negbinom)
fixef(STBT.brms.taxa11.negbinom, probs = c(0.05, 0.95))
r2_bayes(STBT.brms.taxa11.negbinom, ci = 0.95)

save(GULD.brms.taxa11.negbinom, file = "./models/GULD.brms.taxa11.negbinom.RData")
save(STBT.brms.taxa11.negbinom, file = "./models/STBT.brms.taxa11.negbinom.RData")


# XII. Spionidae [Family)
data_GULDgrouped2.taxa12 <- subset(data_GULDgrouped2, Artsgruppering == "Spionidae")
GULD.brms.taxa12.negbinom <- brm(Count ~
                                   1 + BA + (1|Year), data=data_GULDgrouped2.taxa12, 
                                 family = negbinomial(),
                                 control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                 chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(GULD.brms.taxa12.negbinom)
summary(GULD.brms.taxa12.negbinom)
fixef(GULD.brms.taxa12.negbinom, probs = c(0.05, 0.95))
r2_bayes(GULD.brms.taxa12.negbinom, ci = 0.95)

data_STBTgrouped2.taxa12 <- subset(data_STBTgrouped2, Artsgruppering == "Spionidae")
STBT.brms.taxa12.negbinom <- brm(Count ~
                                   1 + BA + (1|Year), data=data_STBTgrouped2.taxa12, 
                                 family = negbinomial(),
                                 control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                 chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(STBT.brms.taxa12.negbinom)
summary(STBT.brms.taxa12.negbinom)
fixef(STBT.brms.taxa12.negbinom, probs = c(0.05, 0.95))
r2_bayes(STBT.brms.taxa12.negbinom, ci = 0.95)

save(GULD.brms.taxa12.negbinom, file = "./models/GULD.brms.taxa12.negbinom.RData")
save(STBT.brms.taxa12.negbinom, file = "./models/STBT.brms.taxa12.negbinom.RData")


# XIII. Capitellidae [Family)
data_GULDgrouped2.taxa13 <- subset(data_GULDgrouped2, Artsgruppering == "Capitellidae")
GULD.brms.taxa13.negbinom <- brm(Count ~
                                   1 + BA + (1|Year), data=data_GULDgrouped2.taxa13, 
                                 family = negbinomial(),
                                 control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                 chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(GULD.brms.taxa13.negbinom)
summary(GULD.brms.taxa13.negbinom)
fixef(GULD.brms.taxa13.negbinom, probs = c(0.05, 0.95))
r2_bayes(GULD.brms.taxa13.negbinom, ci = 0.95)

data_STBTgrouped2.taxa13 <- subset(data_STBTgrouped2, Artsgruppering == "Capitellidae")
STBT.brms.taxa13.negbinom <- brm(Count ~
                                   1 + BA + (1|Year), data=data_STBTgrouped2.taxa13, 
                                 family = negbinomial(),
                                 control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                 chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(STBT.brms.taxa13.negbinom)
summary(STBT.brms.taxa13.negbinom)
fixef(STBT.brms.taxa13.negbinom, probs = c(0.05, 0.95))
r2_bayes(STBT.brms.taxa13.negbinom, ci = 0.95)

save(GULD.brms.taxa13.negbinom, file = "./models/GULD.brms.taxa13.negbinom.RData")
save(STBT.brms.taxa13.negbinom, file = "./models/STBT.brms.taxa13.negbinom.RData")


# XIV. Orbiniidae [Family)
data_GULDgrouped2.taxa14 <- subset(data_GULDgrouped2, Artsgruppering == "Orbiniidae")
GULD.brms.taxa14.negbinom <- brm(Count ~
                                   1 + BA + (1|Year), data=data_GULDgrouped2.taxa14, 
                                 family = negbinomial(),
                                 control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                 chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(GULD.brms.taxa14.negbinom)
summary(GULD.brms.taxa14.negbinom)
fixef(GULD.brms.taxa14.negbinom, probs = c(0.05, 0.95))
r2_bayes(GULD.brms.taxa14.negbinom, ci = 0.95)

data_STBTgrouped2.taxa14 <- subset(data_STBTgrouped2, Artsgruppering == "Orbiniidae")
STBT.brms.taxa14.negbinom <- brm(Count ~
                                   1 + BA + (1|Year), data=data_STBTgrouped2.taxa14, 
                                 family = negbinomial(),
                                 control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                 chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(STBT.brms.taxa14.negbinom)
summary(STBT.brms.taxa14.negbinom)
fixef(STBT.brms.taxa14.negbinom, probs = c(0.05, 0.95))
r2_bayes(STBT.brms.taxa14.negbinom, ci = 0.95)

save(GULD.brms.taxa14.negbinom, file = "./models/GULD.brms.taxa14.negbinom.RData")
save(STBT.brms.taxa14.negbinom, file = "./models/STBT.brms.taxa14.negbinom.RData")


# XV. Sabellida [Order)
data_GULDgrouped2.taxa15 <- subset(data_GULDgrouped2, Artsgruppering == "Sabellida")
GULD.brms.taxa15.negbinom <- brm(Count ~
                                   1 + BA + (1|Year), data=data_GULDgrouped2.taxa15, 
                                 family = negbinomial(),
                                 control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                 chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(GULD.brms.taxa15.negbinom)
summary(GULD.brms.taxa15.negbinom)
fixef(GULD.brms.taxa15.negbinom, probs = c(0.05, 0.95))
r2_bayes(GULD.brms.taxa15.negbinom, ci = 0.95)

data_STBTgrouped2.taxa15 <- subset(data_STBTgrouped2, Artsgruppering == "Sabellida")
STBT.brms.taxa15.negbinom <- brm(Count ~
                                   1 + BA + (1|Year), data=data_STBTgrouped2.taxa15, 
                                 family = negbinomial(),
                                 control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                 chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(STBT.brms.taxa15.negbinom)
summary(STBT.brms.taxa15.negbinom)
fixef(STBT.brms.taxa15.negbinom, probs = c(0.05, 0.95))
r2_bayes(STBT.brms.taxa15.negbinom, ci = 0.95)

save(GULD.brms.taxa15.negbinom, file = "./models/GULD.brms.taxa15.negbinom.RData")
save(STBT.brms.taxa15.negbinom, file = "./models/STBT.brms.taxa15.negbinom.RData")



# XVI. Tubificidae [Family)
data_GULDgrouped2.taxa16 <- subset(data_GULDgrouped2, Artsgruppering == "Tubificidae")
GULD.brms.taxa16.negbinom <- brm(Count ~
                                   1 + BA + (1|Year), data=data_GULDgrouped2.taxa16, 
                                 family = negbinomial(),
                                 control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                 chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(GULD.brms.taxa16.negbinom)
summary(GULD.brms.taxa16.negbinom)
fixef(GULD.brms.taxa16.negbinom, probs = c(0.05, 0.95))
r2_bayes(GULD.brms.taxa16.negbinom, ci = 0.95)

data_STBTgrouped2.taxa16 <- subset(data_STBTgrouped2, Artsgruppering == "Tubificidae")
STBT.brms.taxa16.negbinom <- brm(Count ~
                                   1 + BA + (1|Year), data=data_STBTgrouped2.taxa16, 
                                 family = negbinomial(),
                                 control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                 chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(STBT.brms.taxa16.negbinom)
summary(STBT.brms.taxa16.negbinom)
fixef(STBT.brms.taxa16.negbinom, probs = c(0.05, 0.95))
r2_bayes(STBT.brms.taxa16.negbinom, ci = 0.95)

save(GULD.brms.taxa16.negbinom, file = "./models/GULD.brms.taxa16.negbinom.RData")
save(STBT.brms.taxa16.negbinom, file = "./models/STBT.brms.taxa16.negbinom.RData")


# XVII. Chironomidae [Family)
data_GULDgrouped2.taxa17 <- subset(data_GULDgrouped2, Artsgruppering == "Chironomidae")
GULD.brms.taxa17.negbinom <- brm(Count ~
                                   1 + BA + (1|Year), data=data_GULDgrouped2.taxa17, 
                                 family = negbinomial(),
                                 control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                 chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(GULD.brms.taxa17.negbinom)
summary(GULD.brms.taxa17.negbinom)
fixef(GULD.brms.taxa17.negbinom, probs = c(0.05, 0.95))
r2_bayes(GULD.brms.taxa17.negbinom, ci = 0.95)

data_STBTgrouped2.taxa17 <- subset(data_STBTgrouped2, Artsgruppering == "Chironomidae")
STBT.brms.taxa17.negbinom <- brm(Count ~
                                   1 + BA + (1|Year), data=data_STBTgrouped2.taxa17, 
                                 family = negbinomial(),
                                 control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                 chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(STBT.brms.taxa17.negbinom)
summary(STBT.brms.taxa17.negbinom)
fixef(STBT.brms.taxa17.negbinom, probs = c(0.05, 0.95))
r2_bayes(STBT.brms.taxa17.negbinom, ci = 0.95)

save(GULD.brms.taxa17.negbinom, file = "./models/GULD.brms.taxa17.negbinom.RData")
save(STBT.brms.taxa17.negbinom, file = "./models/STBT.brms.taxa17.negbinom.RData")


# XVIII. Chrysomelidae [Family)
data_GULDgrouped2.taxa18 <- subset(data_GULDgrouped2, Artsgruppering == "Chrysomelidae")
GULD.brms.taxa18.negbinom <- brm(Count ~
                                   1 + BA + (1|Year), data=data_GULDgrouped2.taxa18, 
                                 family = negbinomial(),
                                 control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                 chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(GULD.brms.taxa18.negbinom)
summary(GULD.brms.taxa18.negbinom)
fixef(GULD.brms.taxa18.negbinom, probs = c(0.05, 0.95))
r2_bayes(GULD.brms.taxa18.negbinom, ci = 0.95)

data_STBTgrouped2.taxa18 <- subset(data_STBTgrouped2, Artsgruppering == "Chrysomelidae")
STBT.brms.taxa18.negbinom <- brm(Count ~
                                   1 + BA + (1|Year), data=data_STBTgrouped2.taxa18, 
                                 family = negbinomial(),
                                 control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                 chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(STBT.brms.taxa18.negbinom)
summary(STBT.brms.taxa18.negbinom)
fixef(STBT.brms.taxa18.negbinom, probs = c(0.05, 0.95))
r2_bayes(STBT.brms.taxa18.negbinom, ci = 0.95)

save(GULD.brms.taxa18.negbinom, file = "./models/GULD.brms.taxa18.negbinom.RData")
save(STBT.brms.taxa18.negbinom, file = "./models/STBT.brms.taxa18.negbinom.RData")


# XIX. Bryozoa [Phylum)
data_GULDgrouped2.taxa19 <- subset(data_GULDgrouped2, Artsgruppering == "Bryozoa")
GULD.brms.taxa19.negbinom <- brm(Count ~
                                   1 + BA + (1|Year), data=data_GULDgrouped2.taxa19, 
                                 family = negbinomial(),
                                 control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                 chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(GULD.brms.taxa19.negbinom)
summary(GULD.brms.taxa19.negbinom)
fixef(GULD.brms.taxa19.negbinom, probs = c(0.05, 0.95))
r2_bayes(GULD.brms.taxa19.negbinom, ci = 0.95)

data_STBTgrouped2.taxa19 <- subset(data_STBTgrouped2, Artsgruppering == "Bryozoa")
STBT.brms.taxa19.negbinom <- brm(Count ~
                                   1 + BA + (1|Year), data=data_STBTgrouped2.taxa19, 
                                 family = negbinomial(),
                                 control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                 chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(STBT.brms.taxa19.negbinom)
summary(STBT.brms.taxa19.negbinom)
fixef(STBT.brms.taxa19.negbinom, probs = c(0.05, 0.95))
r2_bayes(STBT.brms.taxa19.negbinom, ci = 0.95)

save(GULD.brms.taxa19.negbinom, file = "./models/GULD.brms.taxa19.negbinom.RData")
save(STBT.brms.taxa19.negbinom, file = "./models/STBT.brms.taxa19.negbinom.RData")


# XX. Nemertea [Phylum)
data_GULDgrouped2.taxa20 <- subset(data_GULDgrouped2, Artsgruppering == "Nemertea")
GULD.brms.taxa20.negbinom <- brm(Count ~
                                   1 + BA + (1|Year), data=data_GULDgrouped2.taxa20, 
                                 family = negbinomial(),
                                 control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                 chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(GULD.brms.taxa20.negbinom)
summary(GULD.brms.taxa20.negbinom)
fixef(GULD.brms.taxa20.negbinom, probs = c(0.05, 0.95))
r2_bayes(GULD.brms.taxa20.negbinom, ci = 0.95)

data_STBTgrouped2.taxa20 <- subset(data_STBTgrouped2, Artsgruppering == "Nemertea")
STBT.brms.taxa20.negbinom <- brm(Count ~
                                   1 + BA + (1|Year), data=data_STBTgrouped2.taxa20, 
                                 family = negbinomial(),
                                 control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                 chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(STBT.brms.taxa20.negbinom)
summary(STBT.brms.taxa20.negbinom)
fixef(STBT.brms.taxa20.negbinom, probs = c(0.05, 0.95))
r2_bayes(STBT.brms.taxa20.negbinom, ci = 0.95)

save(GULD.brms.taxa20.negbinom, file = "./models/GULD.brms.taxa20.negbinom.RData")
save(STBT.brms.taxa20.negbinom, file = "./models/STBT.brms.taxa20.negbinom.RData")









