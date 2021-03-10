# Study: Impacts of the invasive round goby (Neogobius melanostomus) on benthic invertebrate macrofauna - a case study from the Baltic Sea
#
# Code authored by: Nicholas Moran, The Centre for Ocean Life- DTU Aqua, Technical University of Denmark
#                   Mikael van Deurs, DTU AQUA, Technical University of Denmark
# Date: March 2021



#### 3. Visualisations of Before-After Effects ####

Sys.setenv(LANG = "en")

library(dplyr); library(ggplot2); library(RColorBrewer)


load("./models/GULD.brms.origtaxa.negbinom.RData")
load("./models/STBT.brms.origtaxa.negbinom.RData")
load("./models/GULD.brms.fulltaxa.negbinom.RData")
load("./models/STBT.brms.fulltaxa.negbinom.RData")


working <- as.data.frame(coef(STBT.brms.origtaxa.negbinom, probs = c(0.05, 0.95)))


#Figure 1a. Guldborgsund


