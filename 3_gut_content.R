# Study: Impacts of the invasive round goby (Neogobius melanostomus) on benthic invertebrate macrofauna - a case study from the Baltic Sea
#
# Code authored by: Nicholas Moran, The Centre for Ocean Life- DTU Aqua, Technical University of Denmark
#                   Mikael van Deurs, DTU AQUA, Technical University of Denmark
# Date: March 2021


#### 2. Before-after effects using gut content data ####

Sys.setenv(LANG = "en")
library(dplyr); library(brms); library(performance)

##BRMS gut content test ----

# Taxa in gut contents:
#   Gammarus locusta
#   gasterosteus aculeatus
#   Hydrobia ventrosa
#   Idotea balthica
#   Idotea chelpis
#   Litorina litorea
#   Mya arenaria
#   Nymfe Guldsmed
#   Palaemon adspersus
#   Palaemon elegans
#   Palaemon Sp.
#   Parvicardium sp.
#   Potamopyrgus antipodarum
#   Rissoa membranacea
#   Skæl fra artsfælle
#   Thedoxus fluviatilis

data_GULDgrouped2$GutPresence <- if_else(data_GULDgrouped2$Artsgruppering == 'Amphipoda', 'Present', 'Absent')
data_GULDgrouped2$GutPresence <- if_else(data_GULDgrouped2$Artsgruppering == 'Littorinimorpha (small)', 'Present', data_GULDgrouped2$GutPresence <- data_GULDgrouped2$GutPresence)
data_GULDgrouped2$GutPresence <- if_else(data_GULDgrouped2$Artsgruppering == 'Littorinimorpha (large)', 'Present', data_GULDgrouped2$GutPresence <- data_GULDgrouped2$GutPresence)
data_GULDgrouped2$GutPresence <- if_else(data_GULDgrouped2$Artsgruppering == 'Neritidae', 'Present', data_GULDgrouped2$GutPresence <- data_GULDgrouped2$GutPresence)
data_GULDgrouped2$GutPresence <- if_else(data_GULDgrouped2$Artsgruppering == 'Cardiidae', 'Present', data_GULDgrouped2$GutPresence <- data_GULDgrouped2$GutPresence)
data_GULDgrouped2$GutPresence <- if_else(data_GULDgrouped2$Artsgruppering == 'Myidae', 'Present', data_GULDgrouped2$GutPresence <- data_GULDgrouped2$GutPresence)
data_GULDgrouped2$GutPresence <- if_else(data_GULDgrouped2$Artsgruppering == 'Isopoda', 'Present', data_GULDgrouped2$GutPresence <- data_GULDgrouped2$GutPresence)


#Preliminary model specifications
adapt_delta_value <- 0.9999
max_treedepth_value <- 20
iterations <- 2000
burnin <- 1000
thinning <- 2

#Interaction Model
GULD.brms.gutctest.negbinom <- brm(Count ~
                                    1 + BA*GutPresence + (1|Artsgruppering) + (1|Year), data=data_GULDgrouped2, 
                                   family = negbinomial(),
                                   control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                   chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
#plot(GULD.brms.gutctest.negbinom)
summary(GULD.brms.gutctest.negbinom)
fixef(GULD.brms.gutctest.negbinom, probs = c(0.05, 0.95))
r2_bayes(GULD.brms.gutctest.negbinom, ci = 0.95)

save(GULD.brms.gutctest.negbinom, file = "./models/GULD.brms.gutctest.negbinom.RData")
