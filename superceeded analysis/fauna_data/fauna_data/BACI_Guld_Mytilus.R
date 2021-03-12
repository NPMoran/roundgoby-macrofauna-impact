

## WHERE DID I GET TO ##
#Jeg kan ikke finde ud af at installerer glmmadmb, men jeg klare mig med glmer for nu. Jeg synes ikke at have nogen data i Isefjord Inderbredning før invasionen eller hvad er det der er galt?
#Jeg tænker at jeg skal få styr på before after først og fremmest og så kører jeg logistisk på 1-0 og derefter fjerner jeg nullerne og kører en gaussian med log+1

##############################
######## defining BA & CI ####
##############################

CutYear <- 2011
Impact <- "Guldborg Sund Bredning"
Control <- "Isefjord Inderbredning"
Organism <- "Mytilus edulis"

##################################################################
####### Reading in data and preparing it for analysis ############
##################################################################

data <- read.table("df.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

d <- subset(data, Artsgruppering == Organism & (Lokalitetsnavn == Control | Lokalitetsnavn == Impact))


d$BA <- with(d, ifelse(year < CutYear, "aBefore","bAfter" ))

d$CI <- with(d, ifelse(Lokalitetsnavn == Control , "C","I" ))

d$one <- with(d, ifelse(Antal_stk> 0,1,0))
d$count <- with(d, 1)

table(d$BA,d$CI)


####################################
####### packages ###################
####################################

#install.packages("lme4")
library(lme4)
#install.packages("glmmTMB")
library("glmmTMB")

############################
### Logistisk regression ###
############################

fit <- glmer(one ~ BA + CI + BA:CI + (1|year), family= binomial(link = "logit"), data = d)
summary(fit)

fit <- glm(one ~ BA + CI + BA:CI, family= binomial(link = "logit"), data = d)
summary(fit)

table(d$BA,d$CI)

tapply(d$one, list(d$BA,d$CI), sum)

#########################################
### standard model on non-zero values ###
#########################################

dd <- subset(d, one > 0)



fit1 <- glmer(log(Antal_stk) ~ BA + CI + BA:CI + (1|year), family="gaussian", data = dd)
summary(fit1)

fit2 <- glmer(log(Antal_stk) ~ BA+CI + (1|year), family="gaussian", data = dd)
summary(fit2)

anova(fit1,fit2)

boxplot(log(dd$Antal_stk))

boxplot(log(d$Antal_stk+1))


#########################################
### log(N+1) ############################
#########################################



dd <- d

fit1 <- glmer(log(Antal_stk+1) ~ BA + CI + BA:CI + (1|year), family="gaussian", data = dd)
summary(fit1)

fit2 <- glmer(log(Antal_stk+1) ~ BA+CI + (1|year), family="gaussian", data = dd)
summary(fit2)

anova(fit1,fit2)

boxplot(log(dd$Antal_stk))

boxplot(log(d$Antal_stk+1))

###############################
### Negative binomial model ###
###############################

fit <- glmmTMB(Antal_stk ~ BA*CI + (1|year), family=nbinom1(link = "log"), data = d)
summary(fit)

#####################################
######## plot #######################
#####################################

# IMPACT: Black; CONTROL: Grey

tiff(filename = "Fig_Mytilus_Guld.tif",width =15,height=15,unit ='cm',res=300)

layout(matrix(c(1),1,1,byrow = TRUE)) 
par(mar = c(4,4,4,4))


dd <- subset(d, one > 0)

dd <- d

m <- tapply(log(dd$Antal_stk+1),list(dd$BA,dd$CI),mean,na.rm=T)
sd <- tapply(log(dd$Antal_stk+1),list(dd$BA,dd$CI),sd,na.rm=T)
n <- tapply(dd$count,list(dd$BA,dd$CI),sum,na.rm=T)
se <- sd/sqrt(n)

plot(1:2,m[,2], pch = 21, bg = "black", cex = 1, ylim = c(0.4,2), ylab = "LN(Antal+1)", main = Organism, xaxt = "n", xlim = c(0.75,2.25), xlab = "")

lines(1:2,m[,2])
lines(1:2,m[,1])
arrows(1:2,m[,2],1:2,m[,2]+se[,2], angle = 90, length=0.05)
arrows(1:2,m[,2],1:2,m[,2]-se[,2], angle = 90, length=0.05)
arrows(1:2,m[,1],1:2,m[,1]+se[,1], angle = 90, length=0.05)
arrows(1:2,m[,1],1:2,m[,1]-se[,1], angle = 90, length=0.05)

points(1:2,m[,1], pch = 21, bg = "grey", cex = 1)
points(1:2,m[,2], pch = 21, bg = "black", cex = 1)

axis(1, c(1,2), c("Før","Efter"))

dev.off()
