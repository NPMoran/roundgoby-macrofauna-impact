
Sys.setenv(LANG = "en")

#Loading required packages- 


library(dplyr); library(ggplot2); library(ggpubr)
library(lme4); library(lmerTest); library(car); library(rptR); library(performance)
library(arm)

library(devtools)
find_rtools()
#install.packages("brms")
library(brms)
#writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")
#Sys.which("make")
### "C:\\rtools40\\usr\\bin\\make.exe"


#General theme for ggplots-
simpletheme <-   theme(axis.text.y = element_text(size = 10, colour = "black"), axis.text.x = element_text(size = 10, colour = "black"),  panel.background = element_rect(fill = "white"), axis.title.y  = element_text(size=12, vjust = 2), axis.title.x  = element_text(size=12, vjust = 0.1), panel.border = element_rect(colour = "black", fill=NA, size = 1))

#Data importing from MVD
data <- read.table("selectedgroups.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

data$count <- with(data, 1)

#d1 <- subset(data, Artsgruppering == "Neritidae" & Vandområde == "Guldborgsund")
#d2 <- subset(data, Artsgruppering == "Cardiidae" & Vandområde == "Guldborgsund")
#d3 <- subset(data, Artsgruppering == "Littorinimorpha (large)" & Vandområde == "Guldborgsund")
#d4 <- subset(data, Artsgruppering == "Neritidae" & Vandområde == "Stege Bugt")
#d5 <- subset(data, Artsgruppering == "Cardiidae" & Vandområde == "Stege Bugt")
#d6 <- subset(data, Artsgruppering == "Littorinimorpha (large)" & Vandområde == "Stege Bugt")
#
#
#d1$BA <- with(d1, ifelse(year < 2011, "aBefore","bAfter" ))
#d2$BA <- with(d2, ifelse(year < 2011, "aBefore","bAfter" ))
#d3$BA <- with(d3, ifelse(year < 2011, "aBefore","bAfter" ))
#d4$BA <- with(d4, ifelse(year < 2012, "aBefore","bAfter" ))
#d5$BA <- with(d5, ifelse(year < 2012, "aBefore","bAfter" ))
#d6$BA <- with(d6, ifelse(year < 2012, "aBefore","bAfter" ))
#
#d1$year <- as.factor(d1$year)
#summary(d1)
#
#d2$year <- as.factor(d2$year)
#summary(d2)


nrow(data)
nrow(GULDdata)
GULDdata <- subset(data, Vandområde == "Guldborgsund")
GULDdata$BA <- with(GULDdata, ifelse(year < 2011, "aBefore","bAfter" ))
GULDdata$year <- as.factor(GULDdata$year)

#lmer approach
versionlmer <- glmer.nb(Antal_stk ~ 
                                1 + BA + (1 + BA|Artsgruppering), data=GULDdata)
Anova(versionlmer)                 #Sex, ConditionFactor.C effects
summary(versionlmer)               #TankID resolves no variance, TrialRound extremely little
plot(versionlmer)                  #No clustering issues
r2_nakagawa(versionlmer)           #random structure error
coef(versionlmer)


#preliminary model specifications
adapt_delta_value <- 0.9999
max_treedepth_value <- 20
iterations <- 6000
burnin <- 3000
thinning <- 2

#for saving model
#filename <- paste0("models/versionbrms",
#                   "sharedcontrol_",
#                   iterations,"iter_",
#                   burnin,"burnin_",
#                   thinning,"thin_",
#                   adapt_delta_value,"delta_",
#                   max_treedepth_value,"treedepth.RData")

versionbrms <- brm(Antal_stk ~
                     1 + BA + (1 + BA|Artsgruppering) + (1|year), data=GULDdata, 
                   family = negbinomial(),
                   control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                   chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
plot(versionbrms)
summary(versionbrms)
coef(versionbrms)
r2_bayes(versionbrms)

coef(versionbrms2, probs = c(0.05, 0.95))

save(versionbrms, file = "./models/brmstest1.RData")
brmstest1
versionbrms2 <- brm(Antal_stk ~
                     1 + BA + (1 + BA|Artsgruppering) + (1|year), data=GULDdata, 
                   family = zero_inflated_negbinomial(),
                   control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                   chains = 2, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
plot(versionbrms2)
summary(versionbrms2)
coef(versionbrms2)
r2_bayes(versionbrms2)

save(versionbrms2, file = "./models/brmstest2.RData")

