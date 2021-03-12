

##################################################################
####### Reading in data and preparing it for analysis ############
##################################################################

data <- read.table("selectedgroups.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

data$count <- with(data, 1)

d1 <- subset(data, Artsgruppering == "Neritidae" & Vandområde == "Guldborgsund")
d2 <- subset(data, Artsgruppering == "Cardiidae" & Vandområde == "Guldborgsund")
d3 <- subset(data, Artsgruppering == "Littorinimorpha (large)" & Vandområde == "Guldborgsund")
d4 <- subset(data, Artsgruppering == "Neritidae" & Vandområde == "Stege Bugt")
d5 <- subset(data, Artsgruppering == "Cardiidae" & Vandområde == "Stege Bugt")
d6 <- subset(data, Artsgruppering == "Littorinimorpha (large)" & Vandområde == "Stege Bugt")


d1$BA <- with(d1, ifelse(year < 2011, "aBefore","bAfter" ))
d2$BA <- with(d2, ifelse(year < 2011, "aBefore","bAfter" ))
d3$BA <- with(d3, ifelse(year < 2011, "aBefore","bAfter" ))
d4$BA <- with(d4, ifelse(year < 2012, "aBefore","bAfter" ))
d5$BA <- with(d5, ifelse(year < 2012, "aBefore","bAfter" ))
d6$BA <- with(d6, ifelse(year < 2012, "aBefore","bAfter" ))





####################################
####### packages ###################
####################################

#install.packages("lme4")
library(lme4)
#install.packages("glmmTMB")
library("glmmTMB")



###############################
### Negative binomial model ###
###############################

fit1 <- glmmTMB(Antal_stk ~ BA + (1|year), family=nbinom1(link = "log"), data = d1)
summary(fit1)


fit2 <- glmmTMB(Antal_stk ~ BA + (1|year), family=nbinom1(link = "log"), data = d2)
summary(fit2)


fit3 <- glmmTMB(Antal_stk ~ BA + (1|year), family=nbinom1(link = "log"), data = d3)
summary(fit3)


fit4 <- glmmTMB(Antal_stk ~ BA + (1|year), family=nbinom1(link = "log"), data = d4)
summary(fit4)


fit5 <- glmmTMB(Antal_stk ~ BA + (1|year), family=nbinom1(link = "log"), data = d5)
summary(fit5)


fit6 <- glmmTMB(Antal_stk ~ BA + (1|year), family=nbinom1(link = "log"), data = d6)
summary(fit6)

#####################################
######## plot #######################
#####################################

# IMPACT: Black; CONTROL: Grey

tiff(filename = "Fig_not_BACI_Guld.tif",width =15,height=15,unit ='cm',res=300)

layout(matrix(c(1),1,1,byrow = TRUE)) 
par(mar = c(5,5,5,5))




dd <- d1
m <- tapply(log(dd$Antal_stk+1),dd$BA,mean,na.rm=T)
sd <- tapply(log(dd$Antal_stk+1),dd$BA,sd,na.rm=T)
n <- tapply(dd$count,dd$BA,sum,na.rm=T)
se <- sd/sqrt(n)

plot(1:2,m, pch = 21, bg = "black", cex = 1, ylim = c(0,1.8), ylab = "ln(Counts+1)",  xaxt = "n", 
     xlim = c(0.75,2.25), xlab = "", main = "Guldborgsund", cex.lab = 1.15)

dd <- d1
m <- tapply(log(dd$Antal_stk+1),dd$BA,mean,na.rm=T)
sd <- tapply(log(dd$Antal_stk+1),dd$BA,sd,na.rm=T)
n <- tapply(dd$count,dd$BA,sum,na.rm=T)
se <- sd/sqrt(n)
lines(1:2,m)
arrows(1:2,m,1:2,m+se, angle = 90, length=0.05)
arrows(1:2,m,1:2,m-se, angle = 90, length=0.05)
points(1:2,m, pch = 21, bg = "black", cex = 1)


dd <- d2
m <- tapply(log(dd$Antal_stk+1),dd$BA,mean,na.rm=T)
sd <- tapply(log(dd$Antal_stk+1),dd$BA,sd,na.rm=T)
n <- tapply(dd$count,dd$BA,sum,na.rm=T)
se <- sd/sqrt(n)
lines(1:2,m)
arrows(1:2,m,1:2,m+se, angle = 90, length=0.05)
arrows(1:2,m,1:2,m-se, angle = 90, length=0.05)
points(1:2,m, pch = 21, bg = "grey", cex = 2)


dd <- d3
m <- tapply(log(dd$Antal_stk+1),dd$BA,mean,na.rm=T)
sd <- tapply(log(dd$Antal_stk+1),dd$BA,sd,na.rm=T)
n <- tapply(dd$count,dd$BA,sum,na.rm=T)
se <- sd/sqrt(n)
lines(1:2,m)
arrows(1:2,m,1:2,m+se, angle = 90, length=0.05)
arrows(1:2,m,1:2,m-se, angle = 90, length=0.05)
points(1:2,m, pch = 21, bg = "white", cex = 1)



axis(1, c(1,2), c("Before","After"), cex.lab = 1.25)





dev.off()
