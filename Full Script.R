##Libraries----
#general
library(tidyverse)

#data cleaning
library(stringi)

#for maps
library(sf)
library(geojsonsf)
library(ggpubr)

#for modelling
library(lme4)
library(haven)
library(merTools)
library(lattice)

#for presenting outputs
library(sjPlot)
library(glmmTMB)

##Data----

##Read replication survey data and shapefile

rep_data <- read_dta('Data/Pride_Amid_Prejudice_replication data_final.dta')
#put into data file?

shape <- geojson_sf("Data/geoBoundaries-BIH-ADM2.geojson")
#remove accents
shape$shapeName = stri_trans_general(str = shape$shapeName, id = "Latin-ASCII")

##Rename municipalities to allow merge by matching them

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

###Distribution of Repsondents ----
#count respondents by municipality
resp_muni <- as.data.frame(table(rep_data$municipalitystr))
#merge 
resp_muni_shape <- full_join(resp_muni, shape, by = join_by("Var1" == "shapeName"))
#remove the blank municipality value, turn NAs to 0
resp_muni_shape <- resp_muni_shape[-(1),]
resp_muni_shape$Freq <- resp_muni_shape$Freq %>% replace(is.na(.),0)

#histogram of number of respondents by municipality
ggplot(resp_muni_shape) +
  aes(x = Freq) +
  geom_histogram(binwidth = 10)

#map of number of respondents by municipality
ggplot(resp_muni_shape) +
  aes(geometry = geometry, fill = cut(Freq,
                                      breaks = c(-1,0,10,20,30,40,50,320))) +
  geom_sf() +
  scale_fill_grey(start = 1, end = 0.1) +
  labs(fill = "respondents")


##descriptive
ggplot(rep_shape_m) +
  aes(x = as.character(Unemployed)) +
  geom_bar() + scale_x_discrete("Unemployed")


##Modelling----

###null model----
#need to figure out which data to use to make the plot work
null.m <- lmer(supportpride ~ (1|municipalitystr), data = rep_shape_m, na.action="na.exclude")
summary(null.m)
pred.null.m <- fitted(null.m)

ggplot(rep_shape_m) +
  aes(x=treatment, y = pred.null.m, group = municipalitystr) +
  geom_line() +
  scale_color_continuous(guide = 'none') 

VPC = 0.1036/(0.1036+0.8145)
#=11.3% variation explained by municipality. more variation at individual level

###random slopes models----
slopes.m <- lmer(supportpride ~ treatment + (1 + treatment|municipalitystr), data = rep_shape_m, na.action="na.exclude")
summary(slopes.m)

pred.slopes.m <- fitted(slopes.m)

ggplot(rep_shape_m) +
  aes(x=treatment, y = pred.slopes.m, group = municipalitystr) +
  geom_line() +
  scale_color_continuous(guide = 'none') 
#plot shows fanning out: most supportive become more supportive; least become less
#add highlight to this to show sarajevo municipalities

#this one better?
u.slopes.m <- ranef(slopes.m, condVar = T)
dotplot(u.slopes.m)

reEX.slopes.m <-  REsim(slopes.m)
plotREsim(reEX.slopes.m, labs = T)
#only need one of these, they show the same thing. think plotREsim better?
#shows pride only significantly affected most supportive municipalities, no effect elsewhere
#this supports what paper found


####gender----
slopes.1a <- lmer(supportpride ~ treatment + Men + (1 + treatment|municipalitystr),
                  data = rep_shape_m, na.action="na.exclude")

slopes.1b <- lmer(supportpride ~ treatment + Men + (1 + treatment + Men | municipalitystr),
                  data = rep_shape_m, na.action="na.exclude")

anova(slopes.1a, slopes.1b)
#AIC and Chi show no significant evidence of the effect of gender varying by municipality, 
#slope.1a better fit

####unemployed----
slopes.2a <- lmer(supportpride ~ treatment + unemployed + Men + (1 + treatment|municipalitystr),
                  data = rep_shape_m, na.action="na.exclude")

anova(slopes.2a, slopes.1a) #better than 1a

####voteeu----
(slopes.3a <- lmer(supportpride ~ treatment + unemployed + Men + voteeu + (1 + treatment|municipalitystr),
                   data = rep_shape_m, na.action="na.exclude"))

(slopes.3b <- lmer(supportpride ~ treatment + unemployed + Men + voteeu + (1 + treatment + voteeu|municipalitystr),
                   data = rep_shape_m, na.action="na.exclude"))

anova(slopes.3a, slopes.3b) #3a better

####ethnocentric----
(slopes.4a <- lmer(supportpride ~ treatment + unemployed + Men + voteeu + ethnocentric + (1 + treatment|municipalitystr),
                   data = rep_shape_m, na.action="na.exclude"))

(slopes.4b <- lmer(supportpride ~ treatment + unemployed + Men + voteeu + ethnocentric + (1 + treatment + ethnocentric|municipalitystr),
                   data = rep_shape_m, na.action="na.exclude"))

anova(slopes.4b, slopes.4a) #4b better, effect of ethnocentrism varies by municipality


####religiosity----
(slopes.5a <- lmer(supportpride ~ treatment + unemployed + Men + voteeu + ethnocentric + religious + (1 + treatment + ethnocentric|municipalitystr),
                   data = rep_shape_m, na.action="na.exclude"))

(slopes.5b <- lmer(supportpride ~ treatment + unemployed + Men + voteeu + ethnocentric + religious + (1 + treatment + ethnocentric + religious|municipalitystr),
                   data = rep_shape_m, na.action="na.exclude"))

anova(slopes.5a, slopes.5b) 

####Bosniak, Croat, Serb; Bosnian as reference----
(slopes.6a <- lmer(supportpride ~ treatment + unemployed + Men + voteeu + ethnocentric + religious + 
                     Bosniak + Croat + Serb + (1 + treatment + ethnocentric + religious|municipalitystr),
                   data = rep_shape_m, na.action="na.exclude"))

(slopes.6b <- lmer(supportpride ~ treatment + unemployed + Men + voteeu + ethnocentric + religious + 
                     Bosniak + Croat + Serb + (1 + treatment + ethnocentric + religious + Bosniak + Serb + Croat|municipalitystr),
                   data = rep_shape_m, na.action="na.exclude"))
anova(slopes.6a, slopes.6b) #not significantly better, use 6a
summary(slopes.6a) #Croat not significant, omit from further models

####education----
(slopes.7a <- lmer(supportpride ~ treatment + unemployed + Men + voteeu + ethnocentric + religious + 
                     Bosniak + Serb + Education + (1 + treatment + ethnocentric + religious|municipalitystr),
                   data = rep_shape_m, na.action="na.exclude"))



####ethnic vote share----
(slopes.8a <- lmer(supportpride ~ treatment + unemployed + Men + voteeu + ethnocentric + religious + 
                     Bosniak + Serb + Education + vote_share_10 + (1 + treatment + ethnocentric + religious|municipalitystr),
                   data = rep_shape_m, na.action="na.exclude"))

summary(slopes.8a)
#when account for vote share, serb no longer significant
cor.test(rep_shape_m$vote_share_10, rep_shape_m$Serb) #strong correlation between vote share and serb
#remove serb from now on

####age----
(slopes.9a <- lmer(supportpride ~ treatment + unemployed + Men + voteeu + ethnocentric + religious + 
                     Bosniak + Education + vote_share_10 + age_10 + (1 + treatment + ethnocentric + religious|municipalitystr),
                   data = rep_shape_m, na.action="na.exclude"))
summary(slopes.9a) #age not significant, remove from future models

####casualty----
(slopes.10a <- lmer(supportpride ~ treatment + unemployed + Men + voteeu + ethnocentric + religious + 
                      Bosniak + Education + vote_share_10 + casualty + (1 + treatment + ethnocentric + religious|municipalitystr),
                    data = rep_shape_m, na.action="na.exclude"))
summary(slopes.10a) #casualty not significant

####population density----
slopes.11a <- lmer(supportpride ~ treatment + unemployed + Men + voteeu + ethnocentric + religious + 
                     Bosniak + Education + vote_share_10 + log_pop_density +
                     (1 + treatment + ethnocentric + religious|municipalitystr),
                   data = rep_shape_m, na.action="na.exclude")
summary(slopes.11a) #not significant 

###Final Model----
slopes.final <- lmer(supportpride ~ treatment + unemployed + Men + voteeu + ethnocentric + 
                       religious + Bosniak + Education + vote_share_10 + 
                        (1 + treatment + ethnocentric + religious|municipalitystr),
                           data = rep_shape_m, na.action="na.exclude")

#choose just one of these?
u.slopes.final <- ranef(slopes.final, condVar = T)
dotplot(u.slopes.final)

reEX.slopes.final <-  REsim(slopes.final)
plotREsim(reEX.slopes.final, labs = T)
FE.slopes.final <- FEsim(slopes.final)
plotFEsim(FE.slopes.final)
pred.slopes.final <- fitted(slopes.final)

ggplot(rep_shape_m) +
  aes(x=treatment, y = pred.slopes.final, group = municipalitystr) +
  geom_line() #no longer fans out how basic model did. 

##Results----

###
pred.slopes.final <- fitted(slopes.final)
rep_shape_m$pred.slopes.final <- pred.slopes.final
rep_shape_p <- full_join(rep_shape_m, shape, by = join_by("municipalitystr" == "shapeName"))
rep_shape_p <- subset(rep_shape_p, select = (-c(shapeISO.x, shapeGroup.x, shapeType.x, shapeID.x, geometry.x)))
rep_shape_p[["pred.slopes.final"]][is.na(rep_shape_p[["pred.slopes.final"]])] <- 0


###Regression Outputs----
slopes.final.est <- REextract(slopes.final)
slopes.final.est <- slopes.final.est %>% rename(intercept = `(Intercept)`,
                         
                                                                       intercept_se = `(Intercept)_se`)
tab_model(slopes.final)
plot_model(slopes.final, type = "re", dot.size = 0.8, line.size = 0.5, vline.color = '#000000')
plot_model(slopes.final, type = "est", dot.size = 0.8, line.size = 0.5, vline.color = '#000000', grid.breaks = 0.1)

###Maps----
slopes.final.est_shape <- left_join(shape, slopes.final.est, by = join_by("shapeName" == "groupID"))
slopes.final.est_shape[is.na(slopes.final.est_shape)] <- 0


#2.447919 is fixed intercept; 0.091094 fixed treatment
before_map <- ggplot(slopes.final.est_shape) +
  aes(fill = intercept+2.447919) +
  geom_sf() + 
  scale_fill_distiller(type = "div", palette = "PRGn", limits = c(1,4), name = "Support\nBefore Pride ") +
  theme_void()

change_map <- ggplot(slopes.final.est_shape) +
  aes(fill = treatment+0.091094) +
  geom_sf() + 
  scale_fill_distiller(type = "div", palette = "PRGn", limits = c(-0.3,0.3), name = "Change in\nSupport") + 
  theme_void()

after_map <- ggplot(slopes.final.est_shape) +
  aes(fill = intercept+treatment+2.447919+0.091094) +
  geom_sf() + 
  scale_fill_distiller(type = "div", palette = "PRGn", limits = c(1,4), name = "Support\nAfter Pride") +
  theme_void() 

ggarrange(before_map, after_map, change_map, ncol = 3)
