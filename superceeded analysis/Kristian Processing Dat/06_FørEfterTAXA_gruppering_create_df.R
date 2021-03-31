
#Close all open data taps before starting on script
rm(list=ls())


library(maptools)
library(mapplots)
library(raster)
library(rgdal)
library(shapefiles)
library(ggplot2)
library(ggmap)
library(MASS)
library(coda)
library(grid)
library(gridExtra)
library(geosphere)
library(scales)
library(rgeos)
library(raster)
library(mgcv)
library(RColorBrewer)
library(devtools)
library(reshape)
library(GISTools)
library(Rcpp)
library(SpatialPosition)


library(tidyr)
library(dplyr)






setwd("C:\\Users\\kspl\\Desktop\\Efter PhD\\Sortmundet kutling\\ODA data")


# INFO FRA GRETE MAIL 16 DECEMBER 2019

#Hej Kristian,
#Blot for at følge op på et par ting fra mødet i dag, og de ting jeg skulle tage mig af (Jane sender en liste med de øvrige actions):
  
#  1)	Når man laver arts-data udtræk i ODA får man ikke de prøver med hvor der ikke blev fundet nogle dyr. Man er derfor nød til at sammenligne to forskellige udtræk, et med arterne (mangler 0 prøverne) og et med sediment (som har alle prøver med). NB: hvis man laver udtrækket i en omgang, altså vælger både arter og sediment, får man samme resultat som når man trækker arter ud (dvs. 0 prøverne mangler)
#  2)	Vil Kan du udtrække både biomasse og antal individer per prøve også så vi kan sammenholde det ifht. tidslig udvikling? Samt bund-substrat forhold, salinitet og dybde på alle stationer?
#  3)	Udtrække data fra Isefjord yderbredning som alternativt kontrolområde til Roskilde inderfjord?
  
  
#  Jeg har vedhæftet den seneste NOVANA rapport fra 2019 med opdatering af udviklingen t.o. 2017 samt program beskrivelsen for perioden 2017-2021.





################################## READ IN DATA #######################################
#Read in Seditment data
sediment <- read.csv2("C:\\Users\\kspl\\Desktop\\Efter PhD\\Sortmundet kutling\\ODA data\\2000_2019_Sediment.csv", stringsAsFactors = FALSE)

#Read in Arts data
art <- read.csv2("C:\\Users\\kspl\\Desktop\\Efter PhD\\Sortmundet kutling\\ODA data\\2000_2019_Art.csv", stringsAsFactors = FALSE)




#summary(as.factor(art$Artsnavn))

#unique(art$Artsnavn)



# Lav datasæt med række med hvert artsnavn
 Artsnavn <- unique(art$Artsnavn)                                      
 Artsnavn <- as.data.frame(Artsnavn)
 
 # Sæt hvert artsnavn på sediment data
 sediart <- merge(sediment, Artsnavn)
 
#antal <- summarise(group_by(sediart, Vandområde), antalarter = length(unique(Artsnavn))) #kan ikke finde year (men er kun 2018), quarter, nationl. Taget opknat i stedet

 #Full join sedimentdata med artsnavne og arts-datasæt
 dat2 <- full_join(sediart, art, by = c("Vandområde","Lokalitetsnavn","MarinReferenceKode","ObservationsStedNr","ObservationsStedNavn"
                                       ,"MC.stationsnr","ObsSted_bredde","ObsSted_længde","Dato","Prøvenummer","Prøvetagningsudstyr","Artsnavn"))


 
 # Lav en årskolonne
 
 dat2$year <- substr(dat2$Dato, 1,4)
 
 
 #======================================================
 # Subset for områder og efter 2005 
 #======================================================
 #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
 omr <- subset(dat2, Vandområde %in% c("Guldborgsund","Stege Bugt","Isefjord, indre") & year > 2005)
 
 # Isefjord, indre har 2 områder der måles på. Vil kun have den der er data for i 2006, 2011, 2013 og 2015. Kan kun fjernes ud fra observationsbredde eller længdegrad
 omr2 <- subset(omr, ObsSted_bredde != 5545.55)
 
 

 
# ggplot()+
 #   geom_point(data = Isefjordi, aes(x=ObsSted_bredde, y = year))
 
 #================================================================================================
 
 ##  Opdel på Før og Efter for hvert område - inklusive 2 "områder" for kontrol (samme sted, men forskellige antal år)

 #================================================================================================
  
 
 # Lav en reduceret udgave for at reducere antal kolonner som er unødvendige i analyse
 red <- omr2[c("Vandområde","Lokalitetsnavn","Dato", #,"Antal.delprøver..stk..x","Antal.delprøver..stk..y"
               "Prøvenummer","ObsSted_bredde","ObsSted_længde","Artsnavn","Artsrække",
               "Antal..stk.","Biomasse.vådvægt..g.","year")]
 
 #Opret nyt unikt række ID
 red$sampleID_new <- seq.int(nrow(red))
 
 
 # NU SKAL DER OPDELES I FØR OG EFTER INVASIONS TIDSPUNKT
 
 ## Guldborgssund før og efter
 GuldFor <- subset(red , Vandområde == "Guldborgsund" & year < 2009)
 #Tilføj TidsID
 GuldFor$TidsID <- "Before"
 #Tilføj StedID
 GuldFor$StedID <- "Område 1"
  #Tilføj detaljeret ID
 GuldFor$ID <- "Guldborg_Before"
 
 
 GuldEfter <- subset(red , Vandområde == "Guldborgsund" & year > 2009)
 #Tilføj TidsID
 GuldEfter$TidsID <- "After"
 #Tilføj StedID
 GuldEfter$StedID <- "Område 1"
 #Tilføj detaljeret ID
 GuldEfter$ID <- "Guldborg_After"
 
 ## StegeBugt før og efter
 StegeFor <- subset(red , Vandområde == "Stege Bugt" & year < 2012)
 #Tilføj TidsID
 StegeFor$TidsID <- "Before"
 #Tilføj StedID
 StegeFor$StedID <- "Område 2"
 #Tilføj detaljeret ID
 StegeFor$ID <- "Stege_Before"
 
 StegeEfter <- subset(red , Vandområde == "Stege Bugt" & year > 2012)
 #Tilføj TidsID
 StegeEfter$TidsID <- "After"
 #Tilføj StedID
 StegeEfter$StedID <- "Område 2"
 #Tilføj detaljeret ID
 StegeEfter$ID <- "Stege_After"
 
 ## 2 udgaver af Isefjord fordi der skal være for om 2011 er med i efter eller i før
 ## Isefjord før og efter 1
 IseFor1 <- subset(red , Vandområde == "Isefjord, indre" & year < 2012)
 #Tilføj TidsID
 IseFor1$TidsID <- "Before"
 #Tilføj StedID
 IseFor1$StedID <- "Kontrol 1"
 #Tilføj detaljeret ID
 IseFor1$ID <- "Ise1_Before_2006+2011"
 
 IseEfter1 <- subset(red , Vandområde == "Isefjord, indre" & year > 2012)
 #Tilføj TidsID
 IseEfter1$TidsID <- "After"
 #Tilføj StedID
 IseEfter1$StedID <- "Kontrol 1"
 #Tilføj detaljeret ID
 IseEfter1$ID <- "Ise1_After_2013+2015"
 
 ## Isefjord før og efter 2
 IseFor2 <- subset(red , Vandområde == "Isefjord, indre" & year < 2009)
#Tilføj TidsID
 IseFor2$TidsID <- "Before"
 #Tilføj StedID
 IseFor2$StedID <- "Kontrol 2"
 #Tilføj detaljeret ID
 IseFor2$ID <- "Ise2_Before_2006"
  
 IseEfter2 <- subset(red , Vandområde == "Isefjord, indre" & year > 2009)
 #Tilføj TidsID
 IseEfter2$TidsID <- "After"
 #Tilføj StedID
 IseEfter2$StedID <- "Kontrol 2"
 #Tilføj detaljeret ID
 IseEfter2$ID <- "Ise2_After_2011+2013+2015"
 
 
 
 # Sæt sammen igen til samlet datasæt
 
 df <- rbind(GuldFor,GuldEfter,StegeFor,StegeEfter,IseFor1,IseEfter1,IseFor2,IseEfter2)
 

 #Insert 0 when NA in antal + biomasse
 df$Antal..stk.[is.na(df$Antal..stk.)] <- 0
 df$Biomasse.vådvægt..g.[is.na(df$Biomasse.vådvægt..g.)] <- 0
 # unique(df$Biomasse.vådvægt..g.)
 
 #Insert Ej kendt when NA in Artsrække
 df$Artsrække[is.na(df$Artsrække)] <- "Ej kendt"

 
 unique(df$Vandområde)
 unique(df$Lokalitetsnavn)
 unique(df$Dato)
 unique(df$Prøvenummer)
 unique(df$ObsSted_bredde)
 unique(df$ObsSted_længde)
 unique(df$Artsnavn)
 unique(df$Artsrække)
 unique(df$Antal..stk.)
 unique(df$Biomasse.vådvægt..g.)
 unique(df$year)
 unique(df$sampleID_new)
 unique(df$TidsID)
 unique(df$StedID)
 unique(df$ID)
 
 
 #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #======================================================
 # Artssammenlægning
 #=======================================================
 #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 
 #Read in ArtsTaxa data
 artsTaxa <- read.csv2("C:\\Users\\kspl\\Desktop\\Efter PhD\\Sortmundet kutling\\R scripts\\arter_til_gruppering_mergeformat.csv", stringsAsFactors = FALSE, header=T, na.strings=c("","NA"))

 #Fjern Vandområde kolonne
 artsTaxa <- artsTaxa[,c(2,5:6)] 
 
 #Subset to only include where Taxa column is filled in
 
 Taxa_sum <- artsTaxa[complete.cases(artsTaxa),]
 
 
 Taxa_sum2 <- summarise(group_by( Taxa_sum, TAXA_include_as, Trivialnavn, Artsnavn))
 
   
 # Tilføj artsTaxa kolonne vha Left_join
 dfTaxa <- left_join(df, artsTaxa, by = c("Artsnavn"))
 
 

 
 
 
 ## Muslinger holdes hver for sig
 
  #unique(df$Artsnavn)
 
 
 names(dfTaxa) <- c("Vandområde","Lokalitetsnavn","Dato","ODA_prøvenummer","ObsSted_bredde","ObsSted_længde",
                "Artsnavn","Artsrække","Antal_stk","Biomasse_vådvægt_g","year","sampleID_new","TidsID","StedID",
                "ID","Artsgruppering","Trivialnavn")

 
 unique(dfTaxa$Artsgruppering)
 

 #guld <- subset(dfTaxa, StedID == "Område 1")
 
 #unique(guld$ID)
 
 # Opdel på vandområde, artsnavn , biomasse og antal
# arter_til_gruppering <- summarise(group_by(df, Vandområde, Artsnavn, year), Antal = sum(Antal_stk, na.rm = T), Biomasse = sum(Biomasse_vådvægt_g, na.rm = T)) #kan ikke finde year (men er kun 2018), quarter, nationl. Taget opknat i stedet
 
 # Find antal prøver per år
 df_sum <- summarise(group_by(dfTaxa, Vandområde, year),
                     sampleIDs = length(sampleID_new),
                     sampleDates = length(unique(Dato)),
                     sampleAreas = length(unique(ObsSted_bredde)),
                     ODA_sampleNumbers = length(unique(ODA_prøvenummer)))
 
 #write.csv( arter_til_gruppering,"C:\\Users\\kspl\\Desktop\\Efter PhD\\Sortmundet kutling\\arter_til_gruppering.csv", row.names = FALSE)
 
   write.csv(dfTaxa,"C:\\Users\\kspl\\Desktop\\Efter PhD\\Sortmundet kutling\\df.csv", row.names = FALSE)
 
 