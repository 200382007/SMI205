##Libraries----
#general
library(tidyverse)

#for maps
library(sf)
library(geojsonsf)
library(ggpubr)

#for presenting outputs
library(sjPlot)
library(glmmTMB)

##Distribution of Respondents----
###histogram of number of respondents by municipality
fig1 <- ggplot(resp_muni_shape) +
  aes(x = Freq) +
  geom_histogram(binwidth = 10) + theme_classic() +
  labs(x = "Number of Respondents", 
       y = "Frequency", 
       title = "Figure 1. Number of \nRespondents per Municipality\n")

ggsave("Outputs/Figure_1.png", fig1)

#map of number of respondents by municipality
fig2 <- ggplot(resp_muni_shape) +
  aes(geometry = geometry, fill = cut(Freq,
                                      breaks = c(-1,0,10,20,30,40,50,320), 
                                      labels = c("0", "1 to 10", "11 to 20", "21 to 30", "31 to 40", 
                                                 "41 to 50", "50+"))) +
  geom_sf() +
  scale_fill_grey(start = 1, end = 0.1) +
  labs(fill = "", title = "Figure 2. Number of Respondents\n") + theme_void()

ggsave("Outputs/Figure_2.png", fig2, bg = "#ffffff")

##Distribution of Variables----
man <- ggplot(rep_shape_m) +
  aes(x = as.character(Men)) +
  geom_bar() + scale_x_discrete("Male") + theme_minimal()

unemp <- ggplot(rep_shape_m) +
  aes(x = as.character(unemployed)) +
  geom_bar() + scale_x_discrete("Unemployed") + theme_minimal()

voteeu <- ggplot(rep_shape_m) +
  aes(x = as.character(voteeu)) +
  geom_bar() + scale_x_discrete("Vote EU") + theme_minimal()

ethno <- ggplot(rep_shape_m) +
  aes(x = as.character(ethnocentric)) +
  geom_bar() + scale_x_discrete("Ethnocentric") + theme_minimal()

relig <- ggplot(rep_shape_m) +
  aes(x = as.character(religious)) +
  geom_bar() + scale_x_discrete("Religiosity") + theme_minimal()

edu <- ggplot(rep_shape_m) +
  aes(x = Education) +
  geom_bar() + theme_minimal()

vote <- ggplot(rep_shape_m) +
  aes(x = ethnic_vote_share) +
  geom_bar() + scale_x_binned("Ethnic Vote Share") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.7, size = 5)) + theme_minimal()

pop <- ggplot(rep_shape_m) +
  aes(x = log_pop_density) +
  geom_bar() + scale_x_binned("Log Population \nDensity") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.7, size = 5)) + theme_minimal()

age <-  ggplot(rep_shape_m) +
  aes(x = age) +
  geom_bar() + scale_x_binned("Age") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.7, size = 5)) + theme_minimal()

casualty <- ggplot(rep_shape_m) +
  aes(x = log(casualty)) +
  geom_histogram(binwidth = 0.5) + theme_minimal()

ethnic <- ggplot(rep_shape_m) +
  aes(x = as.character(ethnicity)) +
  geom_bar() + scale_x_discrete("Ethnicity", limits = c("1","2","3","4","5"),
                                labels = c("Bosnian", "Croat", "Serb", "Bosniak","other")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.7, hjust = 1)) + theme_minimal()

treat <- ggplot(rep_shape_m) +
  aes(x = as.character(treatment)) +
  geom_bar() + scale_x_discrete("Treatment") + theme_minimal()

fig3 <- ggarrange(treat, man, voteeu, ethno,
          unemp, relig, casualty, ethnic,
          pop, age, edu, vote,
          nrow = 3, ncol = 4) %>% annotate_figure(top = "Figure 3: Distributions of Variables")

ggsave("Outputs/Figure_3.png", fig3, width = 7, height = 7, units = "in", bg = "#ffffff")

##Plots of Models ----
null_pred <- ggplot(rep_shape_m) +
  aes(x=treatment, y = pred.null.m, group = municipalitystr) +
  geom_line() +
  scale_color_continuous(guide = 'none') 

ggsave("Outputs/Null_Pred.png", null_plot)


fig4 <- ggplot(rep_shape_m) +
  aes(x=treatment, y = pred.slopes.m, group = municipalitystr) +
  geom_line() +
  scale_color_continuous(guide = 'none') + theme_minimal() +
  labs(tag = "Figure 4", title = "Random Effects Estimates for Treatment Variable")

ggsave("Outputs/Figure_4.png", fig4, bg = "#ffffff")

u.slopes.m <- ranef(slopes.m, condVar = T)
dotplot(u.slopes.m)


##Plots of the Final Model----
fig5 <- plot_model(slopes.final, type = "re", dot.size = 0.8, line.size = 0.5, vline.color = '#000000', 
           title = "Figure 5. Random Effects of Final Model")

save_plot("Outputs/Figure_5.png", fig5, height = 16, axis.textsize = 0.6, axis.titlesize = 0.8)

fig6 <- plot_model(slopes.final, type = "est", dot.size = 0.8, line.size = 0.5, vline.color = '#000000', grid.breaks = 0.1, 
           axis.labels = c("Ethnic Vote Share", "Education", "Bosniak","Religiosity","Ethnocentric", 
                           "Vote EU",  "Male","Unemployed", "Treatment"), 
           title = "Figure 6. Fixed Effects \nof Final Model")

save_plot("Outputs/Figure_6.png", fig6, width = 8, axis.titlesize = 0.8)


##Preparing data to map with----
pred.slopes.final <- fitted(slopes.final)
rep_shape_m$pred.slopes.final <- pred.slopes.final
rep_shape_p <- full_join(rep_shape_m, shape, by = join_by("municipalitystr" == "shapeName"))
rep_shape_p <- subset(rep_shape_p, select = (-c(shapeISO.x, shapeGroup.x, shapeType.x, shapeID.x, geometry.x)))
rep_shape_p[["pred.slopes.final"]][is.na(rep_shape_p[["pred.slopes.final"]])] <- 0


###Maps----
slopes.final.est_shape <- left_join(shape, slopes.final.est, by = join_by("shapeName" == "groupID"))
slopes.final.est_shape[is.na(slopes.final.est_shape)] <- 0


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
  scale_fill_distiller(type = "div", palette = "PRGn", limits = c(-0.3,0.3), name = "Change in  \nSupport") + 
  theme_void()

after_map <- ggplot(slopes.final.est_shape) +
  aes(fill = intercept+treatment+2.447919+0.091094) +
  geom_sf() + 
  scale_fill_distiller(type = "div", palette = "PRGn", limits = c(1,4), name = "Support\nAfter Pride") +
  theme_void()

fig7 <- ggarrange(before_map, after_map, change_map, ncol = 3) %>% 
  annotate_figure(top = "Figure 7. Support for LGBT+ People, Before and After Pride")

ggsave("Outputs/Figure_7.png", fig7, width = 12, height = 4, units = "in", bg = "#ffffff")
