library(lme4)
library(lmerTest)
library(tidyverse)
library(broom)
library(ggplot2)
library(ggpubr)
library('fastDummies')
library(effsize)
library(lsr)

######################################################## Preprocessing ######################################################## 
## Load the clean of the EXP2 and 3
EXP2 = read.csv("EXP2_alldata_clean.csv")
EXP3 = read.csv("EXP3_alldata_clean.csv")

## Combine the 2 conditions and 2 experiments
alldata_clean=rbind(select(EXP2,sub_id,exp,Onset,Length,fOnset,fOnsetE,Explabel,rLength,Shorter,Correct),select(EXP3,sub_id,exp,Onset,fOnset,fOnsetE,Explabel,rLength,Length,Shorter,Correct))
alldata_clean = alldata_clean %>%
  extract(Explabel, into = c("EXP", "TargetType"),
          regex = "(EXP[0-9]+)([ab])")

######################################################## Statistical test ######################################################## 

#############################################################################################################################
############################################## Run logistic mixed-effect model ##############################################
#############################################################################################################################
## Coding the contrast
# Contrast the 3 Onset Times (early ontime late) by simple coding: comparing early vs ontime, early vs late, but centered (-1/3,2/3,-1/3; -1/3,-1/3, 2/3)
alldata_clean = alldata_clean %>%
  mutate(earlyVSontime=ifelse(fOnsetE=="ontime",2/3,-1/3),
         earlyVSlate=ifelse(fOnsetE=="late",2/3,-1/3), # note that early is always -1/3, so it's reference level
         exp2VSexp3=ifelse(EXP=="EXP9",-0.5,0.5), # EXP9 = tone context + speech/tone
         speechVStone=ifelse(TargetType=="a",-0.5,0.5), # a = speech
  ) 
## Check the coding
alldata_clean %>% group_by(exp2VSexp3,earlyVSontime,earlyVSlate) %>% summarise(n())

## Use BUILDMER package to figure out which random fx to drop https://cran.r-project.org/web/packages/buildmer/vignettes/buildmer.html
library(buildmer)
fmla=Shorter ~ (exp2VSexp3)*(speechVStone)*(earlyVSontime+earlyVSlate)*rLength  + 
  (1 + (earlyVSontime+earlyVSlate)*rLength|sub_id)
m <- buildmer(fmla,data=alldata_clean,
              family="binomial",
              buildmerControl=buildmerControl(direction='order',
                                              args=list(control=glmerControl(optimizer='bobyqa'))))
summary(m) # random fx retained:      (1 + rLength + earlyVSlate | sub_id)


## Full model
lm = glmer(Shorter ~ exp2VSexp3*speechVStone*(earlyVSontime+earlyVSlate)*rLength  + 
             (1 + (earlyVSontime+earlyVSlate)*rLength|sub_id),
           data= alldata_clean,family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
summary(lm)

## Test the omnibus Auditory Targets x Onset Times 2-way interaction 2-way interaction  
lm_nointeraction = glmer(Shorter ~ exp2VSexp3*speechVStone*(earlyVSontime+earlyVSlate)*rLength  -
                           exp2VSexp3:speechVStone:(earlyVSontime+earlyVSlate) + 
                           (1 + earlyVSlate + rLength|sub_id),
                         data= alldata_clean,family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  

anova(lm,lm_nointeraction)      
