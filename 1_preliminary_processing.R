# Study: Impacts of the invasive round goby (Neogobius melanostomus) on benthic invertebrate macrofauna - a case study from the Baltic Sea
#
# Code authored by: Nicholas Moran, The Centre for Ocean Life- DTU Aqua, Technical University of Denmark
#                   Mikael van Deurs, DTU AQUA, Technical University of Denmark
# Date: March 2021



#### 1. Preliminary processing and taxa groupings ####

Sys.setenv(LANG = "en")
library(dplyr); library(operators); library(data.table)


## Processing of datasets ----
#Uploading required dataset (preliminary data extraction and processing done by Kristian Schreiber Plet-Hansen)
#   Benthic invertebrate macrofauna and environmental data were retrieved for the period 2006-2015 from the NOVANA program database -
#   (Surface Water Database, ODA: https://odaforalle.au.dk). 
#   All fauna samples were collected with a HAPS corer (seabed area: 0.0143 m2) in accordance with the requirements of the NOVANA monitoring programme technical guidelines (Hansen et al. 2017; https://bios.au.dk/radiation/Topic Centers/fdcmarintny/; https://mst.dk/natur-vand/overvaagning-af-vand-og-natur/). 


#Full dataset
data_ALL <- read.csv("data_ALL.csv", strip.white=TRUE)
nrow(data_ALL) #70840 data points
labels(data_ALL) 



#Excluding taxa that were not detected in any samples
speciesnames_absent <- subset(data_ALL, Antal_stk != 0)
speciesnames_absent <- as.data.frame(summary(speciesnames_absent$Artsnavn, maxsum = 500))
names(speciesnames_absent)[names(speciesnames_absent) == "summary(speciesnames_absent$Artsnavn, maxsum = 500)"] <- "N_detections"
speciesnames_absent$Artsnavn <- rownames(speciesnames_absent)
rownames(speciesnames_absent) <- NULL
speciesnames_absent <- subset(speciesnames_absent, N_detections == 0)
summary(speciesnames_absent$Artsnavn)

data_REDUCED <- subset(data_ALL, Artsnavn %!in% speciesnames_absent$Artsnavn)
summary(data_REDUCED$Artsnavn)

write.csv(data_REDUCED, "data_REDUCED.csv")



#Producing separate datasets for each site
data_REDUCED <- read.csv("data_REDUCED.csv", strip.white=TRUE)
n_distinct(data_REDUCED$Artsnavn) #70 taxonomic groups identified

data_REDUCED$SampleID <- as.factor(paste(data_REDUCED$year, data_REDUCED$ODA_prøvenummer, sep = '_'))

data_GULD <- subset(data_REDUCED, Vandområde == 'Guldborgsund')
data_STBT <- subset(data_REDUCED, Vandområde == 'Stege Bugt')

n_distinct(data_GULD$SampleID) #134 total samples
summary(data_GULD$SampleID, maxsum = 500) #2007, 20 samples; 2011, 30 samples; 2013, 42 samples: 4015, 42 samples
nrow(data_GULD) #9380 datarows
nrow(subset(data_GULD, Antal_stk != 0)) #967 observations, so approx 90% zeros

n_distinct(data_STBT$SampleID) #146 total samples
summary(data_STBT$SampleID, maxsum = 500) #2009, 20 samples; 2011, 42 samples; 2013, 42 samples: 4015, 42 samples
nrow(data_STBT) #10220 datarows
nrow(subset(data_STBT, Antal_stk != 0)) #1136 observations, so approx 90% zeros




## Adding species groups for analysis (restructured groupings) ----
# Including any taxanomic group that was detected in at least 5% of cores
# Classifications consistent with World Register of Marine Organisms 
# I. Amphipoda (Order) ----
#   Phylum:	    Arthropoda
#   Subphylum:	Crustacea
#   Class:	    Malacostraca

# - Consists of species/id groups
#      Microdeutopus gryllotalpa
#      Gammarus salinus
#      Corophium insidiosum
#      Gammarus locusta
#      Corophium volutator
#      Ampithoe rubricata
#      Gammarus sp.
#      Gammarus inaequicauda
#      Melita palmata
data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'Microdeutopus gryllotalpa', 'Amphipoda', NULL)
data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'Gammarus salinus', 'Amphipoda', data_GULD$Artsgruppering <- data_GULD$Artsgruppering)
data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'Corophium insidiosum', 'Amphipoda', data_GULD$Artsgruppering <- data_GULD$Artsgruppering)
data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'Gammarus locusta', 'Amphipoda', data_GULD$Artsgruppering <- data_GULD$Artsgruppering)
data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'Corophium volutator', 'Amphipoda', data_GULD$Artsgruppering <- data_GULD$Artsgruppering)
data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'Ampithoe rubricata', 'Amphipoda', data_GULD$Artsgruppering <- data_GULD$Artsgruppering)
data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'Gammarus sp.', 'Amphipoda', data_GULD$Artsgruppering <- data_GULD$Artsgruppering)
data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'Gammarus inaequicauda', 'Amphipoda', data_GULD$Artsgruppering <- data_GULD$Artsgruppering)
data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'Melita palmata', 'Amphipoda', data_GULD$Artsgruppering <- data_GULD$Artsgruppering)

data_STBT$Artsgruppering <- if_else(data_STBT$Artsnavn == 'Microdeutopus gryllotalpa', 'Amphipoda', NULL)
data_STBT$Artsgruppering <- if_else(data_STBT$Artsnavn == 'Gammarus salinus', 'Amphipoda', data_STBT$Artsgruppering <- data_STBT$Artsgruppering)
data_STBT$Artsgruppering <- if_else(data_STBT$Artsnavn == 'Corophium insidiosum', 'Amphipoda', data_STBT$Artsgruppering <- data_STBT$Artsgruppering)
data_STBT$Artsgruppering <- if_else(data_STBT$Artsnavn == 'Gammarus locusta', 'Amphipoda', data_STBT$Artsgruppering <- data_STBT$Artsgruppering)
data_STBT$Artsgruppering <- if_else(data_STBT$Artsnavn == 'Corophium volutator', 'Amphipoda', data_STBT$Artsgruppering <- data_STBT$Artsgruppering)
data_STBT$Artsgruppering <- if_else(data_STBT$Artsnavn == 'Ampithoe rubricata', 'Amphipoda', data_STBT$Artsgruppering <- data_STBT$Artsgruppering)
data_STBT$Artsgruppering <- if_else(data_STBT$Artsnavn == 'Gammarus sp.', 'Amphipoda', data_STBT$Artsgruppering <- data_STBT$Artsgruppering)
data_STBT$Artsgruppering <- if_else(data_STBT$Artsnavn == 'Gammarus inaequicauda', 'Amphipoda', data_STBT$Artsgruppering <- data_STBT$Artsgruppering)
data_STBT$Artsgruppering <- if_else(data_STBT$Artsnavn == 'Melita palmata', 'Amphipoda', data_STBT$Artsgruppering <- data_STBT$Artsgruppering)


# II. Isopoda (Order) ----
#   Phylum:	    Arthropoda
#   Subphylum:	Crustacea
#   Class:	    Malacostraca

# - Consists of species/id groups
#      Idotea viridis
#      Idotea balthica
#      Jaera albifrons
#      Cyathura carinata
#      Sphaeroma hookeri
#      Idotea chelipes
data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'Idotea viridis', 'Isopoda', data_GULD$Artsgruppering <- data_GULD$Artsgruppering)
data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'Idotea balthica', 'Isopoda', data_GULD$Artsgruppering <- data_GULD$Artsgruppering)
data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'Jaera albifrons', 'Isopoda', data_GULD$Artsgruppering <- data_GULD$Artsgruppering)
data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'Cyathura carinata', 'Isopoda', data_GULD$Artsgruppering <- data_GULD$Artsgruppering)
data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'Sphaeroma hookeri', 'Isopoda', data_GULD$Artsgruppering <- data_GULD$Artsgruppering)
data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'Idotea chelipes', 'Isopoda', data_GULD$Artsgruppering <- data_GULD$Artsgruppering)

data_STBT$Artsgruppering <- if_else(data_STBT$Artsnavn == 'Idotea viridis', 'Isopoda', data_STBT$Artsgruppering <- data_STBT$Artsgruppering)
data_STBT$Artsgruppering <- if_else(data_STBT$Artsnavn == 'Idotea balthica', 'Isopoda', data_STBT$Artsgruppering <- data_STBT$Artsgruppering)
data_STBT$Artsgruppering <- if_else(data_STBT$Artsnavn == 'Jaera albifrons', 'Isopoda', data_STBT$Artsgruppering <- data_STBT$Artsgruppering)
data_STBT$Artsgruppering <- if_else(data_STBT$Artsnavn == 'Cyathura carinata', 'Isopoda', data_STBT$Artsgruppering <- data_STBT$Artsgruppering)
data_STBT$Artsgruppering <- if_else(data_STBT$Artsnavn == 'Sphaeroma hookeri', 'Isopoda', data_STBT$Artsgruppering <- data_STBT$Artsgruppering)
data_STBT$Artsgruppering <- if_else(data_STBT$Artsnavn == 'Idotea chelipes', 'Isopoda', data_STBT$Artsgruppering <- data_STBT$Artsgruppering)


# III. Littorinimorpha (small) (Order) ----
#   Phylum:	Mollusca
#   Class:	Gastropoda

# - Consists of species/id groups
#      Hydrobia sp.
#      Hydrobia ventrosa
#      Pusillina sarsi
#      Hydrobia ulvae
#      Rissoa membranacea
#      Potamopyrgus antipodarum
#      Rissoa sp.

data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'Hydrobia sp.', 'Littorinimorpha (small)', data_GULD$Artsgruppering)
data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'Hydrobia ventrosa', 'Littorinimorpha (small)', data_GULD$Artsgruppering <- data_GULD$Artsgruppering)
data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'Pusillina sarsi', 'Littorinimorpha (small)', data_GULD$Artsgruppering <- data_GULD$Artsgruppering)
data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'Hydrobia ulvae', 'Littorinimorpha (small)', data_GULD$Artsgruppering <- data_GULD$Artsgruppering)
data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'Rissoa membranacea', 'Littorinimorpha (small)', data_GULD$Artsgruppering <- data_GULD$Artsgruppering)
data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'Potamopyrgus antipodarum', 'Littorinimorpha (small)', data_GULD$Artsgruppering <- data_GULD$Artsgruppering)
data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'Rissoa sp.', 'Littorinimorpha (small)', data_GULD$Artsgruppering <- data_GULD$Artsgruppering)

data_STBT$Artsgruppering <- if_else(data_STBT$Artsnavn == 'Hydrobia sp.', 'Littorinimorpha (small)', data_STBT$Artsgruppering)
data_STBT$Artsgruppering <- if_else(data_STBT$Artsnavn == 'Hydrobia ventrosa', 'Littorinimorpha (small)', data_STBT$Artsgruppering <- data_STBT$Artsgruppering)
data_STBT$Artsgruppering <- if_else(data_STBT$Artsnavn == 'Pusillina sarsi', 'Littorinimorpha (small)', data_STBT$Artsgruppering <- data_STBT$Artsgruppering)
data_STBT$Artsgruppering <- if_else(data_STBT$Artsnavn == 'Hydrobia ulvae', 'Littorinimorpha (small)', data_STBT$Artsgruppering <- data_STBT$Artsgruppering)
data_STBT$Artsgruppering <- if_else(data_STBT$Artsnavn == 'Rissoa membranacea', 'Littorinimorpha (small)', data_STBT$Artsgruppering <- data_STBT$Artsgruppering)
data_STBT$Artsgruppering <- if_else(data_STBT$Artsnavn == 'Potamopyrgus antipodarum', 'Littorinimorpha (small)', data_STBT$Artsgruppering <- data_STBT$Artsgruppering)
data_STBT$Artsgruppering <- if_else(data_STBT$Artsnavn == 'Rissoa sp.', 'Littorinimorpha (small)', data_STBT$Artsgruppering <- data_STBT$Artsgruppering)


# IV. Littorinimorpha (large) (Order) ----
#   Phylum:	Mollusca
#   Class:	Gastropoda

# - Consists of species/id groups
#      Littorina saxatilis
data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'Littorina saxatilis', 'Littorinimorpha (large)', data_GULD$Artsgruppering)

data_STBT$Artsgruppering <- if_else(data_STBT$Artsnavn == 'Littorina saxatilis', 'Littorinimorpha (large)', data_STBT$Artsgruppering)


# V. Lymnaeidae (Family) ----
#   Phylum:	Mollusca
#   Class:	Gastropoda
#   Superorder:	Hygrophila
#   Superfamily:	Lymnaeoidea

# - Consists of species/id groups
#      Lymnaea sp.
#      Radix peregra
data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'Lymnaea sp.', 'Lymnaeidae', data_GULD$Artsgruppering <- data_GULD$Artsgruppering)
data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'Radix peregra', 'Lymnaeidae', data_GULD$Artsgruppering <- data_GULD$Artsgruppering)

data_STBT$Artsgruppering <- if_else(data_STBT$Artsnavn == 'Lymnaea sp.', 'Lymnaeidae', data_STBT$Artsgruppering <- data_STBT$Artsgruppering)
data_STBT$Artsgruppering <- if_else(data_STBT$Artsnavn == 'Radix peregra', 'Lymnaeidae', data_STBT$Artsgruppering <- data_STBT$Artsgruppering)


# VI. Neritidae [Family] ----
#   Phylum:	  Mollusca
#   Class:	  Gastropoda
#   Subclass:	Neritimorpha
#   Order:	  Cycloneritida

# - Consists of species/id groups
#      Theodoxus fluviatilis
data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'Theodoxus fluviatilis', 'Neritidae', data_GULD$Artsgruppering)

data_STBT$Artsgruppering <- if_else(data_STBT$Artsnavn == 'Theodoxus fluviatilis', 'Neritidae', data_STBT$Artsgruppering)


# VII. Cardiidae,[Family] ----
#   Phylum:	Mollusca
#   Class:  Bivalvia
#   Order:  Cardiida

# - Consists of species/id groups
#      Cerastoderma glaucum
#      Parvicardium exiguum
#      Parvicardium hauniense
#      Cerastoderma edule
data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'Cerastoderma glaucum', 'Cardiidae', data_GULD$Artsgruppering)
data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'Parvicardium exiguum', 'Cardiidae', data_GULD$Artsgruppering)
data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'Parvicardium hauniense', 'Cardiidae', data_GULD$Artsgruppering)
data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'Cerastoderma edule', 'Cardiidae', data_GULD$Artsgruppering)

data_STBT$Artsgruppering <- if_else(data_STBT$Artsnavn == 'Cerastoderma glaucum', 'Cardiidae', data_STBT$Artsgruppering)
data_STBT$Artsgruppering <- if_else(data_STBT$Artsnavn == 'Parvicardium exiguum', 'Cardiidae', data_STBT$Artsgruppering)
data_STBT$Artsgruppering <- if_else(data_STBT$Artsnavn == 'Parvicardium hauniense', 'Cardiidae', data_STBT$Artsgruppering)
data_STBT$Artsgruppering <- if_else(data_STBT$Artsnavn == 'Cerastoderma edule', 'Cardiidae', data_STBT$Artsgruppering)


# VIII. Mytilidae [Family] ----
#   Phylum:	Mollusca
#   Class:	Bivalvia
#   Order:	Mytilida

# - Consists of species/id groups
#      Mytilus edulis
data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'Mytilus edulis', 'Mytilidae', data_GULD$Artsgruppering)

data_STBT$Artsgruppering <- if_else(data_STBT$Artsnavn == 'Mytilus edulis', 'Mytilidae', data_STBT$Artsgruppering)


# IX. Tellinidae [Family] ----
#   Phylum:	Mollusca
#   Class:	Bivalvia
#   Order:	Cardiida

# - Consists of species/id groups
#      Macoma balthica
data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'Macoma balthica', 'Tellinidae', data_GULD$Artsgruppering <- data_GULD$Artsgruppering)

data_STBT$Artsgruppering <- if_else(data_STBT$Artsnavn == 'Macoma balthica', 'Tellinidae', data_STBT$Artsgruppering <- data_STBT$Artsgruppering)


# X. Myidae [Family] ----
#   Phylum:	Mollusca
#   Class:	Bivalvia
#   Order:	Myida

# - Consists of species/id groups
#      Mya arenaria
data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'Mya arenaria', 'Myidae', data_GULD$Artsgruppering <- data_GULD$Artsgruppering)

data_STBT$Artsgruppering <- if_else(data_STBT$Artsnavn == 'Mya arenaria', 'Myidae', data_STBT$Artsgruppering <- data_STBT$Artsgruppering)


# XI. Nereididae [Family] ----
#   Phylum:	Annelida
#   Class:	Polychaeta
#   Order:	Phyllodocida

# - Consists of species/id groups
#      Nereididae indet.
#      Hediste diversicolor
#      Platynereis dumerilii
#      Neanthes virens
data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'Nereididae indet.', 'Nereididae', data_GULD$Artsgruppering)
data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'Hediste diversicolor', 'Nereididae', data_GULD$Artsgruppering)
data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'Platynereis dumerilii', 'Nereididae', data_GULD$Artsgruppering)
data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'Neanthes virens', 'Nereididae', data_GULD$Artsgruppering)

data_STBT$Artsgruppering <- if_else(data_STBT$Artsnavn == 'Nereididae indet.', 'Nereididae', data_STBT$Artsgruppering)
data_STBT$Artsgruppering <- if_else(data_STBT$Artsnavn == 'Hediste diversicolor', 'Nereididae', data_STBT$Artsgruppering)
data_STBT$Artsgruppering <- if_else(data_STBT$Artsnavn == 'Platynereis dumerilii', 'Nereididae', data_STBT$Artsgruppering)
data_STBT$Artsgruppering <- if_else(data_STBT$Artsnavn == 'Neanthes virens', 'Nereididae', data_STBT$Artsgruppering)



# XII. Spionidae [Family] ----
#   Phylum:	 Annelida
#   Class:	 Polychaeta
#   Order:   Spionida

# - Consists of species/id groups
#      Polydora cornuta
#      Pygospio elegans
#      Spionidae indet.
#      Marenzelleria viridis
data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'Polydora cornuta', 'Spionidae', data_GULD$Artsgruppering)
data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'Pygospio elegans', 'Spionidae', data_GULD$Artsgruppering)
data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'Spionidae indet.', 'Spionidae', data_GULD$Artsgruppering)
data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'Marenzelleria viridis', 'Spionidae', data_GULD$Artsgruppering)

data_STBT$Artsgruppering <- if_else(data_STBT$Artsnavn == 'Polydora cornuta', 'Spionidae', data_STBT$Artsgruppering)
data_STBT$Artsgruppering <- if_else(data_STBT$Artsnavn == 'Pygospio elegans', 'Spionidae', data_STBT$Artsgruppering)
data_STBT$Artsgruppering <- if_else(data_STBT$Artsnavn == 'Spionidae indet.', 'Spionidae', data_STBT$Artsgruppering)
data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'Marenzelleria viridis', 'Spionidae', data_GULD$Artsgruppering)


# XIII. Capitellidae [Family] ----
#   Phylum:	 Annelida
#   Class:	 Polychaeta

# - Consists of species/id groups
#      Heteromastus filiformis
data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'Heteromastus filiformis', 'Capitellidae', data_GULD$Artsgruppering <- data_GULD$Artsgruppering)

data_STBT$Artsgruppering <- if_else(data_STBT$Artsnavn == 'Heteromastus filiformis', 'Capitellidae', data_STBT$Artsgruppering <- data_STBT$Artsgruppering)


# XIV. Orbiniidae [Family] ----
#   Phylum:	 Annelida
#   Class:	 Polychaeta

# - Consists of species/id groups
#      Scoloplos armiger
data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'Scoloplos armiger', 'Orbiniidae', data_GULD$Artsgruppering <- data_GULD$Artsgruppering)

data_STBT$Artsgruppering <- if_else(data_STBT$Artsnavn == 'Scoloplos armiger', 'Orbiniidae', data_STBT$Artsgruppering <- data_STBT$Artsgruppering)


# XV. Sabellida [Order] ----
#   Phylum:	 Annelida
#   Class:	 Polychaeta
#   (combines families Fabriciidae and Serpulidae)

# - Consists of species/id groups
#      Manayunkia aestuarina
#      Fabriciinae indet.
#      Fabricia stellaris
#      Spirorbis sp.
data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'Manayunkia aestuarina', 'Sabellida', data_GULD$Artsgruppering <- data_GULD$Artsgruppering)
data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'Fabriciinae indet.', 'Sabellida', data_GULD$Artsgruppering <- data_GULD$Artsgruppering)
data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'Fabricia stellaris', 'Sabellida', data_GULD$Artsgruppering <- data_GULD$Artsgruppering)
data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'Spirorbis sp.', 'Sabellida', data_GULD$Artsgruppering <- data_GULD$Artsgruppering)

data_STBT$Artsgruppering <- if_else(data_STBT$Artsnavn == 'Manayunkia aestuarina', 'Sabellida', data_STBT$Artsgruppering <- data_STBT$Artsgruppering)
data_STBT$Artsgruppering <- if_else(data_STBT$Artsnavn == 'Fabriciinae indet.', 'Sabellida', data_STBT$Artsgruppering <- data_STBT$Artsgruppering)
data_STBT$Artsgruppering <- if_else(data_STBT$Artsnavn == 'Fabricia stellaris', 'Sabellida', data_STBT$Artsgruppering <- data_STBT$Artsgruppering)
data_STBT$Artsgruppering <- if_else(data_STBT$Artsnavn == 'Spirorbis sp.', 'Sabellida', data_STBT$Artsgruppering <- data_STBT$Artsgruppering)


# XVI. Tubificidae [Family] ----
#   Phylum:	  Annelida
#   Class:	  Clitellata
#   Subclass: Oligochaeta
#   Order:    Haplotaxida
#   (family synonym Naididae)

# - Consists of species/id groups
#      Tubificoides benedii
#      Oligochaeta indet.
#      Tubifex costatus
data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'Tubificoides benedii', 'Tubificidae', data_GULD$Artsgruppering <- data_GULD$Artsgruppering)
data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'Oligochaeta indet.', 'Tubificidae', data_GULD$Artsgruppering <- data_GULD$Artsgruppering)
data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'Tubifex costatus', 'Tubificidae', data_GULD$Artsgruppering <- data_GULD$Artsgruppering)

data_STBT$Artsgruppering <- if_else(data_STBT$Artsnavn == 'Tubificoides benedii', 'Tubificidae', data_STBT$Artsgruppering <- data_STBT$Artsgruppering)
data_STBT$Artsgruppering <- if_else(data_STBT$Artsnavn == 'Oligochaeta indet.', 'Tubificidae', data_STBT$Artsgruppering <- data_STBT$Artsgruppering)
data_STBT$Artsgruppering <- if_else(data_STBT$Artsnavn == 'Tubifex costatus', 'Tubificidae', data_STBT$Artsgruppering <- data_STBT$Artsgruppering)


# XVII. Chironomidae [Family] ----
#   Phylum:	Arthropoda
#   Class:	Insecta
#   Order:	Diptera

# - Consists of species/id groups
#      Chironomidae indet.
#      Chironomus sp.
#      CHIRONOMIDAE
#      Procladius sp.
data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'Chironomidae indet.', 'Chironomidae', data_GULD$Artsgruppering <- data_GULD$Artsgruppering)
data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'Chironomus sp.', 'Chironomidae', data_GULD$Artsgruppering <- data_GULD$Artsgruppering)
data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'CHIRONOMIDAE', 'Chironomidae', data_GULD$Artsgruppering <- data_GULD$Artsgruppering)
data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'Procladius sp.', 'Chironomidae', data_GULD$Artsgruppering <- data_GULD$Artsgruppering)

data_STBT$Artsgruppering <- if_else(data_STBT$Artsnavn == 'Chironomidae indet.', 'Chironomidae', data_STBT$Artsgruppering <- data_STBT$Artsgruppering)
data_STBT$Artsgruppering <- if_else(data_STBT$Artsnavn == 'Chironomus sp.', 'Chironomidae', data_STBT$Artsgruppering <- data_STBT$Artsgruppering)
data_STBT$Artsgruppering <- if_else(data_STBT$Artsnavn == 'CHIRONOMIDAE', 'Chironomidae', data_STBT$Artsgruppering <- data_STBT$Artsgruppering)
data_STBT$Artsgruppering <- if_else(data_STBT$Artsnavn == 'Procladius sp.', 'Chironomidae', data_STBT$Artsgruppering <- data_STBT$Artsgruppering)


# XVIII. Chrysomelidae [Family] ----
#   Phylum:	Arthropoda
#   Class:	Insecta
#   Order:	Coleoptera

# - Consists of species/id groups
#      Macroplea mutica 
data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'Macroplea mutica', 'Chrysomelidae', data_GULD$Artsgruppering <- data_GULD$Artsgruppering)

data_STBT$Artsgruppering <- if_else(data_STBT$Artsnavn == 'Macroplea mutica', 'Chrysomelidae', data_STBT$Artsgruppering <- data_STBT$Artsgruppering)


# XIX. Bryozoa [Phylum] ----

# - Consists of species/id groups
#      Electra crustulenta (within Class Gymnolaemata, Order Cheilostomatida)
data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'Electra crustulenta', 'Bryozoa', data_GULD$Artsgruppering <- data_GULD$Artsgruppering)

data_STBT$Artsgruppering <- if_else(data_STBT$Artsnavn == 'Electra crustulenta', 'Bryozoa', data_STBT$Artsgruppering <- data_STBT$Artsgruppering)


# XX. Nemertea [Phylum] ----

# - Consists of species/id groups
#      Nemertini indet.
data_GULD$Artsgruppering <- if_else(data_GULD$Artsnavn == 'Nemertini indet.', 'Nemertea', data_GULD$Artsgruppering <- data_GULD$Artsgruppering)

data_STBT$Artsgruppering <- if_else(data_STBT$Artsnavn == 'Nemertini indet.', 'Nemertea', data_STBT$Artsgruppering <- data_STBT$Artsgruppering)


# Taxa groups in Novana data not included in one of these taxanomic groups ----
# (note: not included as taxa appear in <5% of cores, and cannot be combined into family/ order group to include in analysis)
#   Arenicola marina
#   Tanaidacea indet.
#   Acentropus niveus
#   Insecta indet.
#   Campanulariidae indet.
#   Nematoda indet.
#   Balanus sp.
#   Crangon crangon
#   Eteone longa
#   Heterotanais oerstedii
#   Praunus flexuosus
#   Rhithropanopeus harrisii
#   Trichoptera indet.


#Checking it has worked
summary(as.factor(data_GULD$Artsgruppering))
summary(as.factor(data_STBT$Artsgruppering))

write.csv(data_GULD, "data_GULD.csv")
write.csv(data_STBT, "data_STBT.csv")


## Creating grouped dataframe ----
data_GULD.taxa1 <- as.data.frame(setDT(subset(data_GULD, Artsgruppering == 'Amphipoda'))[ , list(Count = sum(Antal_stk), Year = mean(year)), by = .(SampleID)])
data_GULD.taxa1$Artsgruppering <- "Amphipoda"

data_GULD.taxa2 <- as.data.frame(setDT(subset(data_GULD, Artsgruppering == 'Isopoda'))[ , list(Count = sum(Antal_stk), Year = mean(year)), by = .(SampleID)])
data_GULD.taxa2$Artsgruppering <- "Isopoda"

data_GULD.taxa3 <- as.data.frame(setDT(subset(data_GULD, Artsgruppering == 'Littorinimorpha (small)'))[ , list(Count = sum(Antal_stk), Year = mean(year)), by = .(SampleID)])
data_GULD.taxa3$Artsgruppering <- "Littorinimorpha (small)"

data_GULD.taxa4 <- as.data.frame(setDT(subset(data_GULD, Artsgruppering == 'Littorinimorpha (large)'))[ , list(Count = sum(Antal_stk), Year = mean(year)), by = .(SampleID)])
data_GULD.taxa4$Artsgruppering <- "Littorinimorpha (large)"

data_GULD.taxa5 <- as.data.frame(setDT(subset(data_GULD, Artsgruppering == 'Lymnaeidae'))[ , list(Count = sum(Antal_stk), Year = mean(year)), by = .(SampleID)])
data_GULD.taxa5$Artsgruppering <- "Lymnaeidae"

data_GULD.taxa6 <- as.data.frame(setDT(subset(data_GULD, Artsgruppering == 'Neritidae'))[ , list(Count = sum(Antal_stk), Year = mean(year)), by = .(SampleID)])
data_GULD.taxa6$Artsgruppering <- "Neritidae"

data_GULD.taxa7 <- as.data.frame(setDT(subset(data_GULD, Artsgruppering == 'Cardiidae'))[ , list(Count = sum(Antal_stk), Year = mean(year)), by = .(SampleID)])
data_GULD.taxa7$Artsgruppering <- "Cardiidae"

data_GULD.taxa8 <- as.data.frame(setDT(subset(data_GULD, Artsgruppering == 'Mytilidae'))[ , list(Count = sum(Antal_stk), Year = mean(year)), by = .(SampleID)])
data_GULD.taxa8$Artsgruppering <- "Mytilidae"

data_GULD.taxa9 <- as.data.frame(setDT(subset(data_GULD, Artsgruppering == 'Tellinidae'))[ , list(Count = sum(Antal_stk), Year = mean(year)), by = .(SampleID)])
data_GULD.taxa9$Artsgruppering <- "Tellinidae"

data_GULD.taxa10 <- as.data.frame(setDT(subset(data_GULD, Artsgruppering == 'Myidae'))[ , list(Count = sum(Antal_stk), Year = mean(year)), by = .(SampleID)])
data_GULD.taxa10$Artsgruppering <- "Myidae"

data_GULD.taxa11 <- as.data.frame(setDT(subset(data_GULD, Artsgruppering == 'Nereididae'))[ , list(Count = sum(Antal_stk), Year = mean(year)), by = .(SampleID)])
data_GULD.taxa11$Artsgruppering <- "Nereididae"

data_GULD.taxa12 <- as.data.frame(setDT(subset(data_GULD, Artsgruppering == 'Spionidae'))[ , list(Count = sum(Antal_stk), Year = mean(year)), by = .(SampleID)])
data_GULD.taxa12$Artsgruppering <- "Spionidae"

data_GULD.taxa13 <- as.data.frame(setDT(subset(data_GULD, Artsgruppering == 'Capitellidae'))[ , list(Count = sum(Antal_stk), Year = mean(year)), by = .(SampleID)])
data_GULD.taxa13$Artsgruppering <- "Capitellidae"

data_GULD.taxa14 <- as.data.frame(setDT(subset(data_GULD, Artsgruppering == 'Orbiniidae'))[ , list(Count = sum(Antal_stk), Year = mean(year)), by = .(SampleID)])
data_GULD.taxa14$Artsgruppering <- "Orbiniidae"

data_GULD.taxa15 <- as.data.frame(setDT(subset(data_GULD, Artsgruppering == 'Sabellida'))[ , list(Count = sum(Antal_stk), Year = mean(year)), by = .(SampleID)])
data_GULD.taxa15$Artsgruppering <- "Sabellida"

data_GULD.taxa16 <- as.data.frame(setDT(subset(data_GULD, Artsgruppering == 'Tubificidae'))[ , list(Count = sum(Antal_stk), Year = mean(year)), by = .(SampleID)])
data_GULD.taxa16$Artsgruppering <- "Tubificidae"

data_GULD.taxa17 <- as.data.frame(setDT(subset(data_GULD, Artsgruppering == 'Chironomidae'))[ , list(Count = sum(Antal_stk), Year = mean(year)), by = .(SampleID)])
data_GULD.taxa17$Artsgruppering <- "Chironomidae"

data_GULD.taxa18 <- as.data.frame(setDT(subset(data_GULD, Artsgruppering == 'Chrysomelidae'))[ , list(Count = sum(Antal_stk), Year = mean(year)), by = .(SampleID)])
data_GULD.taxa18$Artsgruppering <- "Chrysomelidae"

data_GULD.taxa19 <- as.data.frame(setDT(subset(data_GULD, Artsgruppering == 'Bryozoa'))[ , list(Count = sum(Antal_stk), Year = mean(year)), by = .(SampleID)])
data_GULD.taxa19$Artsgruppering <- "Bryozoa"

data_GULD.taxa20 <- as.data.frame(setDT(subset(data_GULD, Artsgruppering == 'Nemertea'))[ , list(Count = sum(Antal_stk), Year = mean(year)), by = .(SampleID)])
data_GULD.taxa20$Artsgruppering <- "Nemertea"


#Creating dataset for analysis using the original 8 groupings
data_GULDgrouped <- rbind(data_GULD.taxa1, data_GULD.taxa3, data_GULD.taxa4, data_GULD.taxa6,
                          data_GULD.taxa7, data_GULD.taxa8, data_GULD.taxa11, data_GULD.taxa12)        )
data_GULDgrouped$BA <- with(data_GULDgrouped, ifelse(Year < 2011, "aBefore","bAfter" ))

#Creating dataset for analysis using the all 20 groupings
data_GULDgrouped2 <- rbind(data_GULD.taxa1, data_GULD.taxa2, data_GULD.taxa3, data_GULD.taxa4,
                          data_GULD.taxa5, data_GULD.taxa6, data_GULD.taxa7, data_GULD.taxa8,
                          data_GULD.taxa9, data_GULD.taxa10, data_GULD.taxa11, data_GULD.taxa12,
                          data_GULD.taxa13, data_GULD.taxa14, data_GULD.taxa15, data_GULD.taxa16,
                          data_GULD.taxa17, data_GULD.taxa18, data_GULD.taxa19, data_GULD.taxa20)
data_GULDgrouped2$BA <- with(data_GULDgrouped2, ifelse(Year < 2011, "aBefore","bAfter" ))

write.csv(data_GULDgrouped, "data_GULDgrouped.csv")
write.csv(data_GULDgrouped2, "data_GULDgrouped2.csv")




data_STBT.taxa1 <- as.data.frame(setDT(subset(data_STBT, Artsgruppering == 'Amphipoda'))[ , list(Count = sum(Antal_stk), Year = mean(year)), by = .(SampleID)])
data_STBT.taxa1$Artsgruppering <- "Amphipoda"

data_STBT.taxa2 <- as.data.frame(setDT(subset(data_STBT, Artsgruppering == 'Isopoda'))[ , list(Count = sum(Antal_stk), Year = mean(year)), by = .(SampleID)])
data_STBT.taxa2$Artsgruppering <- "Isopoda"

data_STBT.taxa3 <- as.data.frame(setDT(subset(data_STBT, Artsgruppering == 'Littorinimorpha (small)'))[ , list(Count = sum(Antal_stk), Year = mean(year)), by = .(SampleID)])
data_STBT.taxa3$Artsgruppering <- "Littorinimorpha (small)"

data_STBT.taxa4 <- as.data.frame(setDT(subset(data_STBT, Artsgruppering == 'Littorinimorpha (large)'))[ , list(Count = sum(Antal_stk), Year = mean(year)), by = .(SampleID)])
data_STBT.taxa4$Artsgruppering <- "Littorinimorpha (large)"

data_STBT.taxa5 <- as.data.frame(setDT(subset(data_STBT, Artsgruppering == 'Lymnaeidae'))[ , list(Count = sum(Antal_stk), Year = mean(year)), by = .(SampleID)])
data_STBT.taxa5$Artsgruppering <- "Lymnaeidae"

data_STBT.taxa6 <- as.data.frame(setDT(subset(data_STBT, Artsgruppering == 'Neritidae'))[ , list(Count = sum(Antal_stk), Year = mean(year)), by = .(SampleID)])
data_STBT.taxa6$Artsgruppering <- "Neritidae"

data_STBT.taxa7 <- as.data.frame(setDT(subset(data_STBT, Artsgruppering == 'Cardiidae'))[ , list(Count = sum(Antal_stk), Year = mean(year)), by = .(SampleID)])
data_STBT.taxa7$Artsgruppering <- "Cardiidae"

data_STBT.taxa8 <- as.data.frame(setDT(subset(data_STBT, Artsgruppering == 'Mytilidae'))[ , list(Count = sum(Antal_stk), Year = mean(year)), by = .(SampleID)])
data_STBT.taxa8$Artsgruppering <- "Mytilidae"

data_STBT.taxa9 <- as.data.frame(setDT(subset(data_STBT, Artsgruppering == 'Tellinidae'))[ , list(Count = sum(Antal_stk), Year = mean(year)), by = .(SampleID)])
data_STBT.taxa9$Artsgruppering <- "Tellinidae"

data_STBT.taxa10 <- as.data.frame(setDT(subset(data_STBT, Artsgruppering == 'Myidae'))[ , list(Count = sum(Antal_stk), Year = mean(year)), by = .(SampleID)])
data_STBT.taxa10$Artsgruppering <- "Myidae"

data_STBT.taxa11 <- as.data.frame(setDT(subset(data_STBT, Artsgruppering == 'Nereididae'))[ , list(Count = sum(Antal_stk), Year = mean(year)), by = .(SampleID)])
data_STBT.taxa11$Artsgruppering <- "Nereididae"

data_STBT.taxa12 <- as.data.frame(setDT(subset(data_STBT, Artsgruppering == 'Spionidae'))[ , list(Count = sum(Antal_stk), Year = mean(year)), by = .(SampleID)])
data_STBT.taxa12$Artsgruppering <- "Spionidae"

data_STBT.taxa13 <- as.data.frame(setDT(subset(data_STBT, Artsgruppering == 'Capitellidae'))[ , list(Count = sum(Antal_stk), Year = mean(year)), by = .(SampleID)])
data_STBT.taxa13$Artsgruppering <- "Capitellidae"

data_STBT.taxa14 <- as.data.frame(setDT(subset(data_STBT, Artsgruppering == 'Orbiniidae'))[ , list(Count = sum(Antal_stk), Year = mean(year)), by = .(SampleID)])
data_STBT.taxa14$Artsgruppering <- "Orbiniidae"

data_STBT.taxa15 <- as.data.frame(setDT(subset(data_STBT, Artsgruppering == 'Sabellida'))[ , list(Count = sum(Antal_stk), Year = mean(year)), by = .(SampleID)])
data_STBT.taxa15$Artsgruppering <- "Sabellida"

data_STBT.taxa16 <- as.data.frame(setDT(subset(data_STBT, Artsgruppering == 'Tubificidae'))[ , list(Count = sum(Antal_stk), Year = mean(year)), by = .(SampleID)])
data_STBT.taxa16$Artsgruppering <- "Tubificidae"

data_STBT.taxa17 <- as.data.frame(setDT(subset(data_STBT, Artsgruppering == 'Chironomidae'))[ , list(Count = sum(Antal_stk), Year = mean(year)), by = .(SampleID)])
data_STBT.taxa17$Artsgruppering <- "Chironomidae"

data_STBT.taxa18 <- as.data.frame(setDT(subset(data_STBT, Artsgruppering == 'Chrysomelidae'))[ , list(Count = sum(Antal_stk), Year = mean(year)), by = .(SampleID)])
data_STBT.taxa18$Artsgruppering <- "Chrysomelidae"

data_STBT.taxa19 <- as.data.frame(setDT(subset(data_STBT, Artsgruppering == 'Bryozoa'))[ , list(Count = sum(Antal_stk), Year = mean(year)), by = .(SampleID)])
data_STBT.taxa19$Artsgruppering <- "Bryozoa"

data_STBT.taxa20 <- as.data.frame(setDT(subset(data_STBT, Artsgruppering == 'Nemertea'))[ , list(Count = sum(Antal_stk), Year = mean(year)), by = .(SampleID)])
data_STBT.taxa20$Artsgruppering <- "Nemertea"


#Creating dataset for analysis using the original 8 groupings
data_STBTgrouped <- rbind(data_STBT.taxa1, data_STBT.taxa3, data_STBT.taxa4, data_STBT.taxa6,
                          data_STBT.taxa7, data_STBT.taxa8, data_STBT.taxa11, data_STBT.taxa12)
data_STBTgrouped$BA <- with(data_STBTgrouped, ifelse(Year < 2013, "aBefore","bAfter" ))

#Creating dataset for analysis using the all 20 groupings
data_STBTgrouped2 <- rbind(data_STBT.taxa1, data_STBT.taxa2, data_STBT.taxa3, data_STBT.taxa4,
                           data_STBT.taxa5, data_STBT.taxa6, data_STBT.taxa7, data_STBT.taxa8,
                           data_STBT.taxa9, data_STBT.taxa10, data_STBT.taxa11, data_STBT.taxa12,
                           data_STBT.taxa13, data_STBT.taxa14, data_STBT.taxa15, data_STBT.taxa16,
                           data_STBT.taxa17, data_STBT.taxa18, data_STBT.taxa19, data_STBT.taxa20)
data_STBTgrouped2$BA <- with(data_STBTgrouped2, ifelse(Year < 2013, "aBefore","bAfter" ))

write.csv(data_STBTgrouped, "data_STBTgrouped.csv")
write.csv(data_STBTgrouped2, "data_STBTgrouped2.csv")



