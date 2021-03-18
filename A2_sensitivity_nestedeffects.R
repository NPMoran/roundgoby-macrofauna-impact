# Study: Impacts of the invasive round goby (Neogobius melanostomus) on benthic invertebrate macrofauna - a case study from the Baltic Sea
#
# Code authored by: Nicholas Moran, The Centre for Ocean Life- DTU Aqua, Technical University of Denmark
#                   Mikael van Deurs, DTU AQUA, Technical University of Denmark
# Date: March 2021



#### A2. Sensitivity Analysis -rerunning analysis using nested random effects ####
##BRMS nested versions ----
# - run using negative binomial model, and zero-inflated negative binomial model
GULD.brms.origtaxa.nested <- brm(Count ~
                                   1 + BA + (1 + BA|Artsgruppering) + (1|Year/Artsgruppering), data=data_GULDgrouped, 
                                 family = negbinomial(),
                                 control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                 chains = 2, cores = 4, iter = 1200, warmup = 400, thin = thinning)
#plot(GULD.brms.origtaxa.nested)
summary(GULD.brms.origtaxa.nested)
ranef(GULD.brms.origtaxa.nested, probs = c(0.05, 0.95))
r2_bayes(GULD.brms.origtaxa.nested, ci = 0.95)

GULD.brms.fulltaxa.nested <- brm(Count ~
                                   1 + BA + (1 + BA|Artsgruppering) + (1|Year/Artsgruppering), data=data_GULDgrouped2, 
                                 family = negbinomial(),
                                 control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                                 chains = 2, cores = 4, iter = 600, warmup = 200, thin = thinning)
#plot(GULD.brms.fulltaxa.nested)
summary(GULD.brms.fulltaxa.nested)
ranef(GULD.brms.fulltaxa.nested, probs = c(0.05, 0.95))
r2_bayes(GULD.brms.fulltaxa.nested, ci = 0.95)

save(GULD.brms.origtaxa.nested, file = "./models/GULD.brms.origtaxa.nested.RData")
save(GULD.brms.fulltaxa.nested, file = "./models/GULD.brms.fulltaxa.nested.RData")



