# Study: Impacts of the invasive round goby (Neogobius melanostomus) on benthic invertebrate macrofauna - a case study from the Baltic Sea
#
# Code authored by: Nicholas Moran, The Centre for Ocean Life- DTU Aqua, Technical University of Denmark
#                   Mikael van Deurs, DTU AQUA, Technical University of Denmark
# Date: March 2021


#### 2. Before-after effects using gut content data ####

Sys.setenv(LANG = "en")
library(dplyr); library(brms); library(performance)

## BRMS gut content proportions ----
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



## BRMS gut content test ----


#Preliminary model specifications
adapt_delta_value <- 0.9999
max_treedepth_value <- 20
iterations <- 6000
burnin <- 3000
thinning <- 2



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

GULD.brms.guttest.negbinom_Present <- brm(Count ~
                                              1 + BA + (BA|TaxaGroup) + (1|SampleID) + (1|Year), data=data_GULDgrouped_Present, 
                                            family = negbinomial(),
                                            control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                            chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(GULD.brms.guttest.negbinom_Present)
summary(GULD.brms.guttest.negbinom_Present)
fixef(GULD.brms.guttest.negbinom_Present)
r2_bayes(GULD.brms.guttest.negbinom_Present, ci = 0.95)

save(GULD.brms.guttest.negbinom_Present, file = "./models/GULD.brms.guttest.negbinom_Present.RData")


data=data_GULDgrouped_Absent <- subset(data_GULDgrouped, Presence == 'Absent')

GULD.brms.guttest.negbinom_Absent <- brm(Count ~
                                                 1 + BA + (BA|TaxaGroup) + (1|SampleID) +(1|Year), data=data_GULDgrouped_Absent, 
                                               family = negbinomial(),
                                               control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                               chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(GULD.brms.guttest.negbinom_Absent)
summary(GULD.brms.guttest.negbinom_Absent)
fixef(GULD.brms.guttest.negbinom_Absent)
r2_bayes(GULD.brms.guttest.negbinom_Absent, ci = 0.95)

save(GULD.brms.guttest.negbinom_Absent, file = "./models/GULD.brms.guttest.negbinom_Absent.RData")


                                                        
                                                        
#Presence-Absence hypothesis testing ----
GULD.brms.guttest.negbinom_PresenceTEST <- brm(Count ~
                                            1 + BA*Presence + (BA|TaxaGroup) + (1|SampleID) + (1|Year), data=data_GULDgrouped, 
                                          family = negbinomial(),
                                          control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                          chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(GULD.brms.guttest.negbinom_PresenceTEST)
summary(GULD.brms.guttest.negbinom_PresenceTEST)
fixef(GULD.brms.guttest.negbinom_PresenceTEST)
r2_bayes(GULD.brms.guttest.negbinom_PresenceTEST, ci = 0.95)

save(GULD.brms.guttest.negbinom_PresenceTEST, file = "./models/GULD.brms.guttest.negbinom_PresenceTEST.RData")