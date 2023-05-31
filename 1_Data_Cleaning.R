##Libraries----
#general
library(tidyverse)

#data cleaning
library(stringi)

##Data----

##Read replication survey data and shapefile

rep_data <- read_dta('Data/Pride_Amid_Prejudice_replication data_final.dta')
#put into data file?

shape <- geojson_sf("Data/geoBoundaries-BIH-ADM2.geojson")
#remove accents
shape$shapeName = stri_trans_general(str = shape$shapeName, id = "Latin-ASCII")

###Rename municipalities to allow merge by matching them----

shape[18, "shapeName"] <- "Bosanska Dubica (Kozarska Dubica)"
shape[15, "shapeName"] <- "Bosanska Gradiska (Gradiska)"
shape[20, "shapeName"] <- "Bosanski Novi (Novi Grad)"
shape[22, "shapeName"] <- "Samac (Bosanski Samac)"
shape[23, "shapeName"] <- "Bosansko Grahovo (Grahovo)"
shape[1, "shapeName"] <- "Brcko"
shape[33, "shapeName"] <- "Centar Sarajevo"
shape[48, "shapeName"] <- "Foca (Srbinje)"
shape[53, "shapeName"] <- "Gornji Vakuf - Uskopolje"
shape[74, "shapeName"] <- "Kupres (FBiH)"
shape[89, "shapeName"] <- "Novi Grad Sarajevo"
shape[97, "shapeName"] <- "Pale (RS)"
shape[104, "shapeName"] <- "Prozor (Prozor-Rama)"
shape[114, "shapeName"] <- "Skender Vakuf (Knezevo)"
shape[92, "shapeName"] <- "Stari Grad Sarajevo"
shape[125, "shapeName"] <- "Trnovo (FBiH)"
shape[139, "shapeName"] <- "Zivince"

rep_shape <- inner_join(rep_data, shape, by = join_by("municipalitystr" == "shapeName"))
#eventually get up to rep_shape6, rename this better when there

###Omit observations with missing data----
rep_shape_m <- rep_shape[complete.cases(rep_shape[,c('ethnocentric', 'religious', 'Education')]),]

###Rescale Variables----
##Rescale ethnic vote share to be out of 10 (85% of vote = 8.5)
rep_shape_m$vote_share_10 <- rep_shape_m$ethnic_vote_share/10

##Rescale age to be 1/10, (55 = 5.5)
rep_shape_m$age_10 <- rep_shape_m$age/10

##For plotting distribution of responses ----
resp_muni <- as.data.frame(table(rep_data$municipalitystr))
#merge 
resp_muni_shape <- full_join(resp_muni, shape, by = join_by("Var1" == "shapeName"))
#remove the blank municipality value, turn NAs to 0
resp_muni_shape <- resp_muni_shape[-(1),]
resp_muni_shape$Freq <- resp_muni_shape$Freq %>% replace(is.na(.),0)


#Write datafiles----
write.csv(rep_data, file = "Data/Created/rep_data.csv")
write.csv(rep_shape, file = "Data/Created/rep_shape.csv")
write.csv(rep_shape_m, file = "Data/Created/rep_shape_m.csv")
write.csv(rep_shape_p, file = "Data/Created/rep_shape_p.csv")
write.csv(shape, file = "Data/Created/shape.csv")
