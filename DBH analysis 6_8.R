##HoloLens Measurement Comparison
library(dplyr)
library(ggplot2)
library(tidyverse)
library(sjPlot)
library(mgcv)
library(visreg)
library(rsq)
HoloLens.MEM.samples.6_8 <- read.csv("~/Documents/HoloLens/HoloLens MEM samples 6_8.csv")

##Tree detection, including NAs as 0 and everything else as 1
Holo.det <- HoloLens.MEM.samples.6_8
blah <- filter(Holo.det,Holo.det$Avg.Holo. > 10)
Holo.det$Avg.Holo.dia[is.na(Holo.det$Avg.Holo.dia)] <- 0
Holo.det$Avg.Holo.dia[Holo.det$Avg.Holo.dia> 0] <- 1 


##HoloLens detection as function of DBH

det <- glm(Avg.Holo.dia ~ Measure.dia, data = Holo.det, family = "binomial")
summary(det)
rsq(det)
new <- data.frame(x = c(10,15))
colnames(new) <- c("Measure.dia")
predict(det,new,type="response")


Tree.Det <- ggplot(data = Holo.det, aes(x = Measure.dia, y = Avg.Holo.dia))+
  geom_point()+
  geom_smooth(method = "glm", formula = y~x,
              method.args = list(family = binomial))+
  theme_classic()+
  xlab("Classically measured tree DBH") + ylab("HoloLens detection probability")


#Comparison of successfully detected tree measurements
DBH <- filter(HoloLens.MEM.samples.6_8, is.na(HoloLens.MEM.samples.6_8$Holo.dia.1) == FALSE)
acc <- glm(Avg.Holo.dia ~ log(Measure.dia), data = DBH, family = gaussian(link='log'))
summary(acc)
rsq(acc)



DBH.plot <- ggplot(data = DBH, aes(x = Measure.dia, y = Avg.Holo.dia))+
  geom_point()+
  theme_classic()+
  geom_abline()+
  xlab("DBH from field measurement") + ylab("DBH from HoloLens")+
  geom_smooth(method= "lm")





##Linear model comparing hololens readings with rod transect controlling
##for plot and height

HoloLens.MEM.samples.test <- read.csv("~/Documents/HoloLens/HoloLens MEM samples test.csv")
#HoloLens.MEM.samples.test$Plot <- as.character(HoloLens.MEM.samples.test$Plot)
HoloLens.MEM.samples.test$Height <- as.character(HoloLens.MEM.samples.test$Height)
cov.mod <- glm(HoloLens ~ Rod.transect + Height, data = HoloLens.MEM.samples.test, family = "binomial")
summary(cov.mod)

rsq(cov.mod)
Veg.det <- plot_model(cov.mod, transform = NULL, ci.lvl = .95, vline.color = "black")+
  theme_classic()
  

##Plot-level vegtation complexity

HoloLens.MEM.samples.test <- read.csv("~/Documents/HoloLens/HoloLens MEM samples test.csv")
HoloLens.MEM.samples.test$Plot <- as.character(HoloLens.MEM.samples.test$Plot)
Cov.meas <- aggregate(HoloLens.MEM.samples.test$Rod.transect~HoloLens.MEM.samples.test$Plot,FUN = "sum")
Holo.meas <- aggregate(HoloLens.MEM.samples.test$HoloLens~HoloLens.MEM.samples.test$Plot,FUN = "sum")
Complex <- merge(Cov.meas,Holo.meas)
colnames(Complex) <- c("Plot","Rod.transect","HoloLens")
comp.mod <- lm(HoloLens ~ Rod.transect, data = Complex)
summary(comp.mod)
veg.comp <- ggplot(data = Complex, aes(x = Rod.transect, y = HoloLens))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_classic()+
  xlab("Vegetation complexity using rod transect") + ylab("Vegetation complexity using HoloLens")


##Arrange into single figure
ggarrange(Tree.Det,DBH.plot,Veg.det,veg.comp, nrow= 2,ncol=2)




