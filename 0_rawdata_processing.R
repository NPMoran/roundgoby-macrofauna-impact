# Study: Impacts of the invasive round goby (Neogobius melanostomus) on benthic invertebrate macrofauna - a case study from the Baltic Sea
#
# Code authored by: Kristian Schreiber Plet-Hansen, DTU AQUA, Technical University of Denmark
#                   Nicholas Moran, The Centre for Ocean Life- DTU Aqua, Technical University of Denmark
#                   
# Date: March 2021



rm(list=ls())


#library(maptools)
#library(mapplots)
#library(raster)
#library(rgdal)
#library(shapefiles)
#library(ggplot2)
#library(ggmap)
#library(MASS)
#library(coda)
#library(grid)
#library(gridExtra)
#library(geosphere)
#library(scales)
#library(rgeos)
#library(raster)
#library(mgcv)
#library(RColorBrewer)
#library(devtools)
#library(reshape)
#library(GISTools)
#library(Rcpp)
#library(SpatialPosition)


library(tidyr)
library(dplyr)



# INFO FRA GRETE MAIL 16 DECEMBER 2019

#Hej Kristian,
#Blot for at f?lge op p? et par ting fra m?det i dag, og de ting jeg skulle tage mig af (Jane sender en liste med de ?vrige actions):

#  1)	N?r man laver arts-data udtr?k i ODA f?r man ikke de pr?ver med hvor der ikke blev fundet nogle dyr. Man er derfor n?d til at sammenligne to forskellige udtr?k, et med arterne (mangler 0 pr?verne) og et med sediment (som har alle pr?ver med). NB: hvis man laver udtr?kket i en omgang, alts? v?lger b?de arter og sediment, f?r man samme resultat som n?r man tr?kker arter ud (dvs. 0 pr?verne mangler)
#  2)	Vil Kan du udtr?kke b?de biomasse og antal individer per pr?ve ogs? s? vi kan sammenholde det ifht. tidslig udvikling? Samt bund-substrat forhold, salinitet og dybde p? alle stationer?
#  3)	Udtr?kke data fra Isefjord yderbredning som alternativt kontrolomr?de til Roskilde inderfjord?


#  Jeg har vedh?ftet den seneste NOVANA rapport fra 2019 med opdatering af udviklingen t.o. 2017 samt program beskrivelsen for perioden 2017-2021.


################################## READ IN DATA #######################################
#Read in Sediment data
sediment <- read.csv("2000_2019_Sediment.csv", sep = ';', stringsAsFactors = FALSE)

#Read in Arts data
art <- read.csv("2000_2019_Art.csv", sep = ';',  stringsAsFactors = FALSE)


#summary(as.factor(art$Artsnavn))

#unique(art$Artsnavn)



# Lav datas?t med r?kke med hvert artsnavn
# Create datasets in rows with each species name
Artsnavn <- unique(art$Artsnavn)                                      
Artsnavn <- as.data.frame(Artsnavn)

# S?t hvert artsnavn p? sediment data
# Put each species name on. sediment data
sediart <- merge(sediment, Artsnavn)

#antal <- summarise(group_by(sediart, Vandområde), antalarter = length(unique(Artsnavn))) #kan ikke finde year (men er kun 2018), quarter, nationl. Taget opknat i stedet

#Full join sedimentdata med artsnavne og arts-datas?t
#Full join sediment data with species names and species datasets
dat2 <- full_join(sediart, art, by = c("Vandområde","Lokalitetsnavn","MarinReferenceKode","ObservationsStedNr","ObservationsStedNavn"
                                       ,"MC.stationsnr","ObsSted_bredde","ObsSted_længde","Dato","Prøvenummer","Prøvetagningsudstyr","Artsnavn"))

# Lav en ?rskolonne
# Make an year column

dat2$year <- substr(dat2$Dato, 1,4)


#======================================================
# Subset for omr?der og efter 2005 
# Subset for areas and after 2005
#======================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

omr <- subset(dat2, Vandområde %in% c("Guldborgsund","Stege Bugt") & year > 2005)


#================================================================================================
##  Opdel p? F?r og Efter for hvert omr?de - inklusive 2 "omr?der" for kontrol (samme sted, men forskellige antal ?r)
##  Divide p? Before and After for each area - including 2 "areas" for control (same place but different number of years)
#================================================================================================


# Lav en reduceret udgave for at reducere antal kolonner som er un?dvendige i analyse
# Create a reduced version to reduce the number of columns needed in analysis
red <- omr2[c("Vandområde","Lokalitetsnavn","Dato", #,"Antal.delpr?ver..stk..x","Antal.delpr?ver..stk..y"
              "Prøvenummer","ObsSted_bredde","ObsSted_længde","Artsnavn","Artsrække",
              "Antal..stk.","Biomasse.vådvægt..g.","year","Dybde..m..x")]



#Insert 0 when NA in antal + biomasse
red$Antal..stk.[is.na(red$Antal..stk.)] <- 0
red$Biomasse.vådvægt..g.[is.na(red$Biomasse.vådvægt..g.)] <- 0
#unique(red$Biomasse.vådvægt..g.)


#Insert Ej kendt when NA in Artsrække
red$Artsrække[is.na(red$Artsrække)] <- "Ej kendt"


#renaming columns
labels(red)
names(red)[names(red) == 'Prøvenummer'] <- 'ODA_prøvenummer'
names(red)[names(red) == 'Antal..stk.'] <- 'Antal_stk'
names(red)[names(red) == 'Biomasse.vådvægt..g.'] <- 'Biomasse_vådvægt_g'
names(red)[names(red) == 'Dybde..m..x'] <- 'Depth_m'


#checking mean depth of samples at each site
depth_Guld <- subset(red, Vandområde == 'Guldborgsund')
depth_Guld <- subset(depth_Guld, Artsnavn == 'Actinia sp.')
nrow(depth_Guld)
depth_Guld$Depth_m <- sub(",", )
write.csv(depth_Guld, "depth_Guld.csv", row.names = FALSE)
depth_Guld <- read.csv("depth_Guld.csv", dec = ",")
mean(depth_Guld$Depth_m); sd(depth_Guld$Depth_m)

#checking mean depth of samples at each site
depth_Stbt <- subset(red, Vandområde == 'Stege Bugt')
depth_Stbt <- subset(depth_Stbt, Artsnavn == 'Actinia sp.')
nrow(depth_Stbt)
write.csv(depth_Stbt, "depth_Stbt.csv", row.names = FALSE)
depth_Stbt <- read.csv("depth_Stbt.csv", dec = ",")
depth_Stbt <- subset(depth_Stbt, Depth_m !=  -99.0) #excluding error recording
mean(depth_Stbt$Depth_m); sd(depth_Stbt$Depth_m)


write.csv(red,"data_ALL.csv", row.names = FALSE)



