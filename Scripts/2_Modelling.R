##Libraries----
#general
library(tidyverse)

#for modelling
library(lme4)
library(haven)
library(merTools)
library(lattice)

##Modelling----

###null model----

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


u.slopes.m <- ranef(slopes.m, condVar = T)
dotplot(u.slopes.m)

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


u.slopes.final <- ranef(slopes.final, condVar = T)
dotplot(u.slopes.final)



pred.slopes.final <- fitted(slopes.final)

ggplot(rep_shape_m) +
  aes(x=treatment, y = pred.slopes.final, group = municipalitystr) +
  geom_line() #no longer fans out how basic model did



###Regression Outputs----
slopes.final.est <- REextract(slopes.final)
slopes.final.est <- slopes.final.est %>% rename(intercept = `(Intercept)`,
                                                
                                                intercept_se = `(Intercept)_se`)
###final model----
tab_model(slopes.final)


###all models----
tab_model(slopes.1a, slopes.2a, slopes.3a, slopes.4b, slopes.5b, slopes.6a, 
          slopes.7a, slopes.8a, slopes.9a, slopes.10a, slopes.11a,
          show.icc = F, show.ci = F, show.p = F, p.style = "stars",
          dv.labels = c("1","2", "3", "4","5","6","7","8","9","10","11"),
          show.r2 = F, title = "Table 1. Fixed and Random Effects for Models 1 to 11",
          pred.labels = c("Intercept", "Treatment","Unemployed","Male","Vote EU", "Ethnocentric", 
                          "Religiosity", "Bosniak", "Croat", "Serb", "Education","Ethnic Vote Share", 
                          "Age","Casualty", "Log Population\nDensity"))