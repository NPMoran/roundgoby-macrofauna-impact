#Discarded code


###############PREFERENCE TESTS#############



#Preferred v Non-Preferred prey items (based on >5% occurent in gut content)
data_GULDgrouped <- read.csv("data_GULDgrouped.csv", strip.white=TRUE)
data_GULDgrouped$Preference <- if_else(data_GULDgrouped$TaxaGroup == 'Littorinimorpha (small)', 'Preferred', 'NonPreferred')
data_GULDgrouped$Preference <- if_else(data_GULDgrouped$TaxaGroup == 'Littorinimorpha (large)', 'Preferred', data_GULDgrouped$Preference <- data_GULDgrouped$Preference)
data_GULDgrouped$Preference <- if_else(data_GULDgrouped$TaxaGroup == 'Neritidae', 'Preferred', data_GULDgrouped$Preference <- data_GULDgrouped$Preference)
data_GULDgrouped$Preference <- if_else(data_GULDgrouped$TaxaGroup == 'Cardiidae', 'Preferred', data_GULDgrouped$Preference <- data_GULDgrouped$Preference)

data=data_GULDgrouped_Preferred <- subset(data_GULDgrouped, Preference == 'Preferred')

GULD.brms.guttest.negbinom_Preferred <- brm(Count ~
                                              1 + BA + (BA|TaxaGroup) + (1|SampleID) + (1|Year), data=data_GULDgrouped_Preferred, 
                                            family = negbinomial(),
                                            control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                            chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(GULD.brms.guttest.negbinom_Preferred)
summary(GULD.brms.guttest.negbinom_Preferred)
fixef(GULD.brms.guttest.negbinom_Preferred)
r2_bayes(GULD.brms.guttest.negbinom_Preferred, ci = 0.95)

save(GULD.brms.guttest.negbinom_Preferred, file = "./models/GULD.brms.guttest.negbinom_Preferred.RData")


data=data_GULDgrouped_NonPreferred <- subset(data_GULDgrouped, Preference == 'NonPreferred')

GULD.brms.guttest.negbinom_NonPreferred <- brm(Count ~
                                                 1 + BA + (BA|TaxaGroup) + (1|SampleID) + (1|Year), data=data_GULDgrouped_NonPreferred, 
                                               family = negbinomial(),
                                               control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                               chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(GULD.brms.guttest.negbinom_NonPreferred)
summary(GULD.brms.guttest.negbinom_NonPreferred)
fixef(GULD.brms.guttest.negbinom_NonPreferred)
r2_bayes(GULD.brms.guttest.negbinom_NonPreferred, ci = 0.95)

save(GULD.brms.guttest.negbinom_NonPreferred, file = "./models/GULD.brms.guttest.negbinom_NonPreferred.RData")


#Prey preference hypothesis testing ----
GULD.brms.guttest.negbinom_PreferenceTEST <- brm(Count ~
                                                   1 + BA*Preference + (BA|TaxaGroup) + (1|SampleID) + (1|Year), data=data_GULDgrouped, 
                                                 family = negbinomial(),
                                                 control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                                 chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(GULD.brms.guttest.negbinom_PresenceTEST)
summary(GULD.brms.guttest.negbinom_PreferenceTEST)
fixef(GULD.brms.guttest.negbinom_PreferenceTEST)
r2_bayes(GULD.brms.guttest.negbinom_PreferenceTEST, ci = 0.95)

save(GULD.brms.guttest.negbinom_PreferenceTEST, file = "./models/GULD.brms.guttest.negbinom_PreferenceTEST2.RData")


###############DATA FOR ORIGINAL GROUPINGS#############
#Creating dataset for analysis using the original 8 groupings
data_GULDgrouped <- rbind(data_GULD.taxa1, data_GULD.taxa3, data_GULD.taxa4, data_GULD.taxa6,
                          data_GULD.taxa7, data_GULD.taxa8, data_GULD.taxa11, data_GULD.taxa12)        )
data_GULDgrouped$BA <- with(data_GULDgrouped, ifelse(Year < 2011, "aBefore","bAfter" ))

write.csv(data_GULDgrouped2, "data_GULDgrouped2.csv")

#Creating dataset for analysis using the original 8 groupings
data_STBTgrouped <- rbind(data_STBT.taxa1, data_STBT.taxa3, data_STBT.taxa4, data_STBT.taxa6,
                          data_STBT.taxa7, data_STBT.taxa8, data_STBT.taxa11, data_STBT.taxa12)
data_STBTgrouped$BA <- with(data_STBTgrouped, ifelse(Year < 2013, "aBefore","bAfter" ))

write.csv(dataSTBTgrouped2, "data_STBTgrouped2.csv")



data_GULDgrouped2 <- read.csv("data_GULDgrouped2.csv", strip.white=TRUE)
data_STBTgrouped2 <- read.csv("data_STBTgrouped2.csv", strip.white=TRUE)

data_GULDgrouped2$Year <- as.factor(data_GULDgrouped2$Year)
data_STBTgrouped2$Year <- as.factor(data_STBTgrouped2$Year)



###############ORIGINAL GROUPING MODELS#############
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



###############DUAL SITE MODELS#############
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



###############SINGLE SPECIES MODELS#############

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


load("./models/GULD.brms.taxa01.negbinom.RData")
load("./models/GULD.brms.taxa02.negbinom.RData")
load("./models/GULD.brms.taxa03.negbinom.RData")
load("./models/GULD.brms.taxa04.negbinom.RData")
load("./models/GULD.brms.taxa05.negbinom.RData")
load("./models/GULD.brms.taxa06.negbinom.RData")
load("./models/GULD.brms.taxa07.negbinom.RData")
load("./models/GULD.brms.taxa08.negbinom.RData")
load("./models/GULD.brms.taxa09.negbinom.RData")
load("./models/GULD.brms.taxa10.negbinom.RData")
load("./models/GULD.brms.taxa11.negbinom.RData")
load("./models/GULD.brms.taxa12.negbinom.RData")
load("./models/GULD.brms.taxa13.negbinom.RData")
load("./models/GULD.brms.taxa14.negbinom.RData")
load("./models/GULD.brms.taxa15.negbinom.RData")
load("./models/GULD.brms.taxa16.negbinom.RData")
load("./models/GULD.brms.taxa17.negbinom.RData")
load("./models/GULD.brms.taxa18.negbinom.RData")
load("./models/GULD.brms.taxa19.negbinom.RData")
load("./models/GULD.brms.taxa20.negbinom.RData")

load("./models/STBT.brms.taxa01.negbinom.RData")
load("./models/STBT.brms.taxa02.negbinom.RData")
load("./models/STBT.brms.taxa03.negbinom.RData")
load("./models/STBT.brms.taxa04.negbinom.RData")
load("./models/STBT.brms.taxa05.negbinom.RData")
load("./models/STBT.brms.taxa06.negbinom.RData")
load("./models/STBT.brms.taxa07.negbinom.RData")
load("./models/STBT.brms.taxa08.negbinom.RData")
load("./models/STBT.brms.taxa09.negbinom.RData")
load("./models/STBT.brms.taxa10.negbinom.RData")
load("./models/STBT.brms.taxa11.negbinom.RData")
load("./models/STBT.brms.taxa12.negbinom.RData")
load("./models/STBT.brms.taxa13.negbinom.RData")
load("./models/STBT.brms.taxa14.negbinom.RData")
load("./models/STBT.brms.taxa15.negbinom.RData")
load("./models/STBT.brms.taxa16.negbinom.RData")
load("./models/STBT.brms.taxa17.negbinom.RData")
load("./models/STBT.brms.taxa18.negbinom.RData")
load("./models/STBT.brms.taxa19.negbinom.RData")
load("./models/STBT.brms.taxa20.negbinom.RData")


## Figure 1a. Guldborgsund ----
working <- as.data.frame(ranef(GULD.brms.origtaxa.negbinom, probs = c(0.05, 0.95)))


#building dataframe
data_GULDfig <- NULL
data_GULDfig$estimate <- working$Artsgruppering.Estimate.BAbAfter
data_GULDfig$lower <- working$Artsgruppering.Q5.BAbAfter
data_GULDfig$upper <- working$Artsgruppering.Q95.BAbAfter
data_GULDfig$positions <- c( 3, 8, 5, 4, 7, 1, 6, 2)
data_GULDfig$color <- c("Group3", "Group1", "Group2", "Group2", "Group1", "Group4", "Group2", "Group4")
data_GULDfig$labels <- rownames(working)
data_GULDfig <- as.data.frame(data_GULDfig)


#calculating before and after count per sq m to annotate graph
data_GULDgrouped <- read.csv("data_GULDgrouped.csv", strip.white=TRUE)

data_GULDgrouped$Count_psqm <- data_GULDgrouped$Count/0.0143 #as each core samples 0.0143m2 of the seabed
data_GULDgrouped.Before <- as.data.frame(setDT(subset(data_GULDgrouped, BA == 'aBefore'))[ , list(Before.Count_psqm = mean(Count_psqm), sdBefore.Count_psqm = sd(Count_psqm)), by = .(Artsgruppering)])
names(data_GULDgrouped.Before)[names(data_GULDgrouped.Before) == "Artsgruppering"] <- "labels"
data_GULDgrouped.After <- as.data.frame(setDT(subset(data_GULDgrouped, BA == 'bAfter'))[ , list(After.Count_psqm = mean(Count_psqm), sdAfter.Count_psqm = sd(Count_psqm)), by = .(Artsgruppering)])
names(data_GULDgrouped.After)[names(data_GULDgrouped.After) == "Artsgruppering"] <- "labels"

data_GULDfig <- merge(data_GULDfig, data_GULDgrouped.Before, by = 'labels', all.x = TRUE) 
data_GULDfig <- merge(data_GULDfig, data_GULDgrouped.After, by = 'labels', all.x = TRUE) 

data_GULDfig$Before.Count_psqm <- round(data_GULDfig$Before.Count_psqm, digits = 0)
data_GULDfig$sdBefore.Count_psqm <- round(data_GULDfig$sdBefore.Count_psqm, digits = 0)
data_GULDfig <- transform(data_GULDfig, Before = paste(Before.Count_psqm, sdBefore.Count_psqm, sep = " ± "))

data_GULDfig$After.Count_psqm <- round(data_GULDfig$After.Count_psqm, digits = 0)
data_GULDfig$sdAfter.Count_psqm <- round(data_GULDfig$sdAfter.Count_psqm, digits = 0)
data_GULDfig <- transform(data_GULDfig, After = paste(After.Count_psqm, sdAfter.Count_psqm, sep = " ± "))
data_GULDfig <- transform(data_GULDfig, BeforeAfter = paste('(', Before, sep = ""))
data_GULDfig <- transform(data_GULDfig, BeforeAfter = paste(BeforeAfter, After, sep = " → "))
data_GULDfig <- transform(data_GULDfig, BeforeAfter = paste(BeforeAfter, ')', sep = ""))


#creating dataframe for labels
data_GULDlab <- NULL 
data_GULDlab <-
  
  #plotting
  GULDfig <- ggplot(data_GULDfig, aes(x = estimate, y = positions)) +
  scale_x_continuous(limits = c(-7.5, 5), expand = c(0, 0), breaks=c(-5.0,-2.5,0.0,2.5,5.0)) +
  scale_y_continuous(limits = c(0.5, 9.5), expand = c(0, 0), breaks=NULL) +
  theme(legend.position = "none",
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.x = element_text(size = 5, colour = "black"), 
        axis.line.x = element_line(colour = "black", size = 0.5),
        panel.background = element_rect(fill = "white"),
        axis.title.x  = element_text(size=8, vjust = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) +
  geom_segment(aes(color = color), x = data_GULDfig$lower, y = data_GULDfig$pos, xend = data_GULDfig$upper, yend = data_GULDfig$pos, size = 0.5) + 
  geom_point(aes(color = color), shape = 19, size = 1) +
  scale_color_brewer(palette = "Dark2") +
  geom_vline(xintercept = 0, linetype = 2, colour = "black", size = 0.5) +
  geom_text(aes(label=labels, fontface = 3), hjust = "left", x =-7, vjust=-2, size = 1.8) +
  geom_text(aes(label=BeforeAfter, fontface = 1), hjust = "left", x =-7, vjust=0, size = 1.8) +
  labs(x = "Before-after impact effect (Guldborgsund)",
       y = "") 
GULDfig

ggsave("./visualisations/GULDfig.jpg", width = 8, height = 10, units = "cm", GULDfig, dpi = 600)



## Figure 1b. Stege Bugt ----
working <- as.data.frame(ranef(STBT.brms.origtaxa.negbinom, probs = c(0.05, 0.95)))

#building dataframe
data_STBTfig <- NULL
data_STBTfig$estimate <- working$Artsgruppering.Estimate.BAbAfter
data_STBTfig$lower <- working$Artsgruppering.Q5.BAbAfter
data_STBTfig$upper <- working$Artsgruppering.Q95.BAbAfter
data_STBTfig$positions <- c( 3, 8, 5, 4, 7, 1, 6, 2)
data_STBTfig$color <- c("Group3", "Group1", "Group2", "Group2", "Group1", "Group4", "Group2", "Group4")
data_STBTfig$labels <- rownames(working)
data_STBTfig <- as.data.frame(data_STBTfig)


#calculating before and after count per sq m to annotate graph
data_STBTgrouped <- read.csv("data_STBTgrouped.csv", strip.white=TRUE)

data_STBTgrouped$Count_psqm <- data_STBTgrouped$Count/0.0143 #as each core samples 0.0143m2 of the seabed
data_STBTgrouped.Before <- as.data.frame(setDT(subset(data_STBTgrouped, BA == 'aBefore'))[ , list(Before.Count_psqm = mean(Count_psqm), sdBefore.Count_psqm = sd(Count_psqm)), by = .(Artsgruppering)])
names(data_STBTgrouped.Before)[names(data_STBTgrouped.Before) == "Artsgruppering"] <- "labels"
data_STBTgrouped.After <- as.data.frame(setDT(subset(data_STBTgrouped, BA == 'bAfter'))[ , list(After.Count_psqm = mean(Count_psqm), sdAfter.Count_psqm = sd(Count_psqm)), by = .(Artsgruppering)])
names(data_STBTgrouped.After)[names(data_STBTgrouped.After) == "Artsgruppering"] <- "labels"

data_STBTfig <- merge(data_STBTfig, data_STBTgrouped.Before, by = 'labels', all.x = TRUE) 
data_STBTfig <- merge(data_STBTfig, data_STBTgrouped.After, by = 'labels', all.x = TRUE) 

data_STBTfig$Before.Count_psqm <- round(data_STBTfig$Before.Count_psqm, digits = 0)
data_STBTfig$sdBefore.Count_psqm <- round(data_STBTfig$sdBefore.Count_psqm, digits = 0)
data_STBTfig <- transform(data_STBTfig, Before = paste(Before.Count_psqm, sdBefore.Count_psqm, sep = " ± "))

data_STBTfig$After.Count_psqm <- round(data_STBTfig$After.Count_psqm, digits = 0)
data_STBTfig$sdAfter.Count_psqm <- round(data_STBTfig$sdAfter.Count_psqm, digits = 0)
data_STBTfig <- transform(data_STBTfig, After = paste(After.Count_psqm, sdAfter.Count_psqm, sep = " ± "))
data_STBTfig <- transform(data_STBTfig, BeforeAfter = paste('(', Before, sep = ""))
data_STBTfig <- transform(data_STBTfig, BeforeAfter = paste(BeforeAfter, After, sep = " → "))
data_STBTfig <- transform(data_STBTfig, BeforeAfter = paste(BeforeAfter, ')', sep = ""))


#creating dataframe for labels
data_STBTlab <- NULL 
data_STBTlab <-
  
  #plotting
  STBTfig <- ggplot(data_STBTfig, aes(x = estimate, y = positions)) +
  scale_x_continuous(limits = c(-7.5, 5), expand = c(0, 0), breaks=c(-5.0,-2.5,0.0,2.5,5.0)) +
  scale_y_continuous(limits = c(0.5, 9.5), expand = c(0, 0), breaks=NULL) +
  theme(legend.position = "none",
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.x = element_text(size = 5, colour = "black"), 
        axis.line.x = element_line(colour = "black", size = 0.5),
        panel.background = element_rect(fill = "white"),
        axis.title.x  = element_text(size=8, vjust = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) +
  geom_segment(aes(color = color), x = data_STBTfig$lower, y = data_STBTfig$pos, xend = data_STBTfig$upper, yend = data_STBTfig$pos, size = 0.5) + 
  geom_point(aes(color = color), shape = 19, size = 1) +
  scale_color_brewer(palette = "Dark2") +
  geom_vline(xintercept = 0, linetype = 2, colour = "black", size = 0.5) +
  geom_text(aes(label=labels, fontface = 3), hjust = "left", x =-7, vjust=-2, size = 1.8) +
  geom_text(aes(label=BeforeAfter, fontface = 1), hjust = "left", x =-7, vjust=0, size = 1.8) +
  labs(x = "Before-after impact effect (Stege Bugt)",
       y = "") 
STBTfig

ggsave("./visualisations/STBTfig.jpg", width = 8, height = 10, units = "cm", STBTfig, dpi = 600)



#### ORIG ZEROINFL SENSITIVITY ####


##One model per site, using original 8 taxonomic groups
# - run using negative binomial model, and zero-inflated negative binomial model
GULD.brms.origtaxa.zerinfnb <- brm(Count ~
                                     1 + BA + (1 + BA|Artsgruppering) + (1|Year), data=data_GULDgrouped, 
                                   family = zero_inflated_negbinomial(),
                                   control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                   chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(GULD.brms.origtaxa.zerinfnb)
summary(GULD.brms.origtaxa.zerinfnb)
coef(GULD.brms.origtaxa.zerinfnb, probs = c(0.05, 0.95))
r2_bayes(GULD.brms.origtaxa.zerinfnb)


STBT.brms.origtaxa.zerinfnb <- brm(Count ~
                                     1 + BA + (1 + BA|Artsgruppering) + (1|Year), data=data_STBTgrouped, 
                                   family = zero_inflated_negbinomial(),
                                   control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                   chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(STBT.brms.origtaxa.zerinfnb)
summary(STBT.brms.origtaxa.zerinfnb)
coef(STBT.brms.origtaxa.zerinfnb, probs = c(0.05, 0.95))
r2_bayes(STBT.brms.origtaxa.zerinfnb)


save(GULD.brms.origtaxa.zerinfnb, file = "./models/GULD.brms.origtaxa.zerinfnb.RData")

save(STBT.brms.origtaxa.zerinfnb, file = "./models/STBT.brms.origtaxa.zerinfnb.RData")



###Single model for both sites, using original 8 taxonomic groups
## - run using zero-inflated negative binomial model
#data_GULDgrouped$Site <- "GULD"
#data_STBTgrouped$Site <- "STBT"
#data_DUALgrouped <- rbind(data_GULDgrouped, data_STBTgrouped)
#
#DUAL.brms.origtaxa.zerinfnb <- brm(Count ~
#                                     1 + BA + (1 + BA|Artsgruppering) + (1|Year) + (1|Site), data=data_DUALgrouped, 
#                                   family = zero_inflated_negbinomial(),
#                                   control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
#                                   chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
##plot(DUAL.brms.origtaxa.zerinfnb)
#summary(DUAL.brms.origtaxa.zerinfnb)
#coef(DUAL.brms.origtaxa.zerinfnb, probs = c(0.05, 0.95))
#r2_bayes(DUAL.brms.origtaxa.zerinfnb)
#
#
#save(DUAL.brms.origtaxa.zerinfnb, file = "./models/DUAL.brms.origtaxa.zerinfnb.RData")



#One model per site, using updated 20 taxonomic groups
# - run using zero-inflated negative binomial model
GULD.brms.fulltaxa.zerinfnb <- brm(Count ~
                                     1 + BA + (1 + BA|Artsgruppering) + (1|Year), data=data_GULDgrouped2, 
                                   family = zero_inflated_negbinomial(),
                                   control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                   chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
plot(GULD.brms.fulltaxa.zerinfnb)
summary(GULD.brms.fulltaxa.zerinfnb)
coef(GULD.brms.fulltaxa.zerinfnb, probs = c(0.05, 0.95))
r2_bayes(GULD.brms.fulltaxa.zerinfnb)


STBT.brms.fulltaxa.zerinfnb <- brm(Count ~
                                     1 + BA + (1 + BA|Artsgruppering) + (1|Year), data=data_STBTgrouped2, 
                                   family = zero_inflated_negbinomial(),
                                   control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                   chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(STBT.brms.fulltaxa.zerinfnb)
summary(STBT.brms.fulltaxa.zerinfnb)
coef(STBT.brms.fulltaxa.zerinfnb, probs = c(0.05, 0.95))
r2_bayes(STBT.brms.fulltaxa.zerinfnb)


save(GULD.brms.fulltaxa.zerinfnb, file = "./models/GULD.brms.fulltaxa.zerinfnb.RData")
save(STBT.brms.fulltaxa.zerinfnb, file = "./models/STBT.brms.fulltaxa.zerinfnb.RData")



##Single model for both sites, using updated 20 taxonomic groups
## - run using negative binomial model, and zero-inflated negative binomial model
#data_GULDgrouped2$Site <- "GULD"
#data_STBTgrouped2$Site <- "STBT"
#data_DUALgrouped2 <- rbind(data_GULDgrouped2, data_STBTgrouped2)
#
#DUAL.brms.fulltaxa.zerinfnb <- brm(Count ~
#                                     1 + BA + (1 + BA|Artsgruppering) + (1|Year) + (1|Site), data=data_DUALgrouped2, 
#                                   family = zero_inflated_negbinomial(),
#                                   control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
#                                   chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
##plot(DUAL.brms.fulltaxa.zerinfnb)
#summary(DUAL.brms.fulltaxa.zerinfnb)
#coef(DUAL.brms.fulltaxa.zerinfnb, probs = c(0.05, 0.95))
#r2_bayes(DUAL.brms.fulltaxa.zerinfnb)
#
#
#save(DUAL.brms.fulltaxa.zerinfnb, file = "./models/DUAL.brms.fulltaxa.zerinfnb.RData")

#Excluding 2015
GULD.brms.fulltaxa.negbinomA2 <- brm(Count ~
                                       1 + BA + (1 + BA|TaxaGroup) + (1|SampleID) + (1|Year), data=data_GULDgroupedA2, 
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
                                       1 + BA + (1 + BA|TaxaGroup) + (1|SampleID) + (1|Year), data=data_STBTgroupedA2, 
                                     family = negbinomial(),
                                     control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                     chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(STBT.brms.fulltaxa.negbinomA2)
summary(STBT.brms.fulltaxa.negbinomA2)
ranef(STBT.brms.fulltaxa.negbinomA2)
fixef(STBT.brms.fulltaxa.negbinomA2)
r2_bayes(STBT.brms.fulltaxa.negbinomA2, ci = 0.95)

save(STBT.brms.fulltaxa.negbinomA2, file = "./models/A2_sensitivity/STBT.brms.fulltaxa.negbinomA2.RData")

