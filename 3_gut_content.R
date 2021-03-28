# Study: Impacts of the invasive round goby (Neogobius melanostomus) on benthic invertebrate macrofauna - a case study from the Baltic Sea
#
# Code authored by: Nicholas Moran, The Centre for Ocean Life- DTU Aqua, Technical University of Denmark
#                   Mikael van Deurs, DTU AQUA, Technical University of Denmark
# Date: March 2021


#### 2. Before-after effects using gut content data ####

Sys.setenv(LANG = "en")
library(dplyr); library(brms); library(performance)

##BRMS gut content proportions ----
data_GULDguts <- read.csv("data_GutContentGULD.csv", strip.white=TRUE)
nrow(data_GULDguts) #383 rows
n_distinct(data_GULDguts$Nr.) #297 distinct fish

count(data_GULDguts, Bytteart)
# Taxa in gut contents:
#    Gammarus locusta
#    Gasterosteus aculeatus
#    Hydrobia ventrosa
#    Idotea balthica
#    Idotea chelpis
#    Litorina litorea
#    Mya arenaria
#    Nymfe Guldsmed
#    Palaemon adspersus
#    Palaemon elegans
#    Palaemon Sp.
#    Parvicardium sp.
#    Potamopyrgus antipodarum
#    Rissoa membranacea
#    Skæl fra artsfælle
#    Thedoxus fluviatilis

#Amphipoda
gut_amphipoda <- subset(data_GULDguts, Bytteart == 'Gammarus locusta')
n_distinct(gut_amphpoda$Nr.) #detected in 1 individuals

#Isopoda
gut_isopoda.1 <- subset(data_GULDguts, Bytteart == 'Idotea balthica')
gut_isopoda.2 <- subset(data_GULDguts, Bytteart == 'Idotea chelpis')
gut_isopoda <- rbind(gut_isopoda.1,gut_isopoda.2)
n_distinct(gut_isopoda$Nr.) #detected in 10 individuals

#Cardiidae
gut_cardiidae <- subset(data_GULDguts, Bytteart == 'Parvicardium Sp.')
n_distinct(gut_cardiidae$Nr.) #detected in 19 individuals

#Myidae
gut_myidae <- subset(data_GULDguts, Bytteart == 'Mya arenaria')
n_distinct(gut_myidae$Nr.) #detected in 2 individuals

#Neritidae
gut_neritidae <- subset(data_GULDguts, Bytteart == 'Thedoxus fluviatilis')
n_distinct(gut_neritidae$Nr.) #detected in 23 individuals

#Littorinimorpha (large)
gut_littlarge <- subset(data_GULDguts, Bytteart == 'Litorina litorea')
n_distinct(gut_littlarge$Nr.) #detected in 19 individuals

#Littorinimorpha (small)
gut_littsmall.1 <- subset(data_GULDguts, Bytteart == 'Hydrobia ventrosa')
gut_littsmall.2 <- subset(data_GULDguts, Bytteart == 'Potamopyrgus antipodarum')
gut_littsmall.3 <- subset(data_GULDguts, Bytteart == 'Rissoa membranacea')
gut_littsmall <- rbind(gut_littsmall.1,gut_littsmall.2,gut_littsmall.3)
n_distinct(gut_littsmall$Nr.) #detected in 109 individuals

#Other
gut_other.1 <- subset(data_GULDguts, Bytteart == 'Gasterosteus aculeatus')
gut_other.2 <- subset(data_GULDguts, Bytteart == 'Nymfe Guldsmed')
gut_other.3 <- subset(data_GULDguts, Bytteart == 'Palaemon adspersus')
gut_other.4 <- subset(data_GULDguts, Bytteart == 'Palaemon elegans')
gut_other.5 <- subset(data_GULDguts, Bytteart == 'Palaemon Sp.')
gut_other.6 <- subset(data_GULDguts, Bytteart == 'Skæl fra artsfælle')
gut_other <- rbind(gut_other.1,gut_other.2,gut_other.3,gut_other.4,gut_other.5,gut_other.6)
n_distinct(gut_other$Nr.) #detected in 120 individuals

data_GutPercentagesGULD <- NULL
data_GutPercentagesGULD$TaxaGroup <- c('Amphipoda','Isopoda','Cardiidae','Myidae','Neritidae','Littorinimorpha (large)','Littorinimorpha (small)','Other')
data_GutPercentagesGULD$N_Occurence <- c(1,10,19,2,23,19,109,120)
data_GutPercentagesGULD <- as.data.frame(data_GutPercentagesGULD)
data_GutPercentagesGULD$Percent_Occurence <- (data_GutPercentagesGULD$N_Occurence/297)*100

write.csv(data_GutPercentagesGULD, "data_GutPercentagesGULD.csv")


##BRMS gut content test ----
data_GULDgrouped <- read.csv("data_GULDgrouped.csv", strip.white=TRUE)
data_GULDgrouped$GutPresence <- if_else(data_GULDgrouped$TaxaGroup == 'Littorinimorpha (small)', 'Preferred', 'NonPreferred')
data_GULDgrouped$GutPresence <- if_else(data_GULDgrouped$TaxaGroup == 'Littorinimorpha (large)', 'Preferred', data_GULDgrouped$GutPresence <- data_GULDgrouped$GutPresence)
data_GULDgrouped$GutPresence <- if_else(data_GULDgrouped$TaxaGroup == 'Neritidae', 'Preferred', data_GULDgrouped$GutPresence <- data_GULDgrouped$GutPresence)
data_GULDgrouped$GutPresence <- if_else(data_GULDgrouped$TaxaGroup == 'Cardiidae', 'Preferred', data_GULDgrouped$GutPresence <- data_GULDgrouped$GutPresence)
data_GULDgrouped$GutPresence <- if_else(data_GULDgrouped$TaxaGroup == 'Isopoda', 'Preferred', data_GULDgrouped$GutPresence <- data_GULDgrouped$GutPresence)


#Preliminary model specifications
adapt_delta_value <- 0.9999
max_treedepth_value <- 20
iterations <- 6000
burnin <- 3000
thinning <- 2


#Separate models
data=data_GULDgrouped_Preferred <- subset(data_GULDgrouped, GutPresence == 'Preferred')

GULD.brms.guttest.negbinom_Preferred <- brm(Count ~
                                     1 + BA + (BA|TaxaGroup) + (1|Year), data=data_GULDgrouped_Preferred, 
                                   family = negbinomial(),
                                   control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                   chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(GULD.brms.guttest.negbinom_Preferred)
summary(GULD.brms.guttest.negbinom_Preferred)
fixef(GULD.brms.guttest.negbinom_Preferred, probs = c(0.05, 0.95))
r2_bayes(GULD.brms.guttest.negbinom_Preferred, ci = 0.95)

save(GULD.brms.guttest.negbinom_Preferred, file = "./models/GULD.brms.guttest.negbinom_Preferred.RData")


data=data_GULDgrouped2_NonPreferred <- subset(data_GULDgrouped, GutPresence == 'NonPreferred')

GULD.brms.guttest.negbinom_NonPreferred <- brm(Count ~
                                     1 + BA + (BA|TaxaGroup) + (1|Year), data=data_GULDgrouped2_NonPreferred, 
                                   family = negbinomial(),
                                   control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                   chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(GULD.brms.guttest.negbinom_NonPreferred)
summary(GULD.brms.guttest.negbinom_NonPreferred)
fixef(GULD.brms.guttest.negbinom_NonPreferred, probs = c(0.05, 0.95))
r2_bayes(GULD.brms.guttest.negbinom_NonPreferred, ci = 0.95)

save(GULD.brms.guttest.negbinom_NonPreferred, file = "./models/GULD.brms.guttest.negbinom_NonPreferred.RData")

