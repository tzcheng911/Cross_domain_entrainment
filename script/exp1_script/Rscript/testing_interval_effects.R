library(lme4)
library(lmerTest)
library(tidyverse)
library(broom)
#Set working directory (where the data file is)
setwd("/Users/t.z.cheng/Google_Drive/Research/Delaydoesmatter/real_exp/exp1/data_analysis/Rdata")

#Import data
# mydata = read.csv('learning_effect.csv')
mydata = read.csv('new_learning_effect.csv')
str(mydata) #data inspection, you should see that all the variables are "int" they need to be centered and scaled
mydata = mydata %>%
  mutate(Delay_length = case_when(nDelay_length == 2 ~ "Short", nDelay_length == 4 ~ "Long"))
mydata = mydata %>%
  mutate(Comp_onset = case_when(nComp_onset == 5 ~ "Early", nComp_onset == 8 ~ "Ontime", nComp_onset == 9 ~ "Late"))
mydata = mydata %>%
  mutate(Present_order = case_when(nPresent_order == 1 ~ "short_long", nPresent_order == 2 ~ "long_short"))

# filter the first block
mydata = mydata %>%
  filter((Delay_length == "Short" & Present_order == "short_long") | (Delay_length == "Long" & Present_order == "long_short"))
# mydata = mydata %>%
#   filter((Delay_length == 2 & Present_order == 1) | (Delay_length == 4 & Present_order == 2))

# Center and scaling each variable 
mydata$Trial_num = scale(mydata$Trial_num, center = TRUE, scale = TRUE)
mydata$Delay_length = scale(mydata$Delay_length, center = TRUE, scale = TRUE)
mydata$Comp_onset = scale(mydata$Comp_onset, center = TRUE, scale = TRUE)
mydata$nComp_length = scale(mydata$nComp_length, center = TRUE, scale = TRUE)
mydata$Present_order = scale(mydata$Present_order, center = TRUE, scale = TRUE)

numIters = 10000  

# Examine the main effect of Trial_num
lm_withtrial = glmer(Correctness~Delay_length + Trial_num + (Trial_num|Subj_num),data= mydata,family="binomial", REML=FALSE, control=glmerControl(optCtrl=list(maxfun=numIters)), verbose=2)  
lm_withouttrial = glmer(Correctness~Delay_length + (Trial_num|Subj_num),data= mydata,family="binomial", REML=FALSE, control=glmerControl(optCtrl=list(maxfun=numIters)), verbose=2) 
anova(lm_withtrial,lm_withouttrial)
summary(lm_withtrial)

# Examine the interaciton between Delay_length and Trial_num
lm_withtrial = glmer(Correctness~Delay_length + Delay_length:Trial_num + (Trial_num|Subj_num),data= mydata,family="binomial", REML=FALSE, control=glmerControl(optCtrl=list(maxfun=numIters)), verbose=2)  
lm_withouttrial = glmer(Correctness~Delay_length + (Trial_num|Subj_num),data= mydata,family="binomial", REML=FALSE, control=glmerControl(optCtrl=list(maxfun=numIters)), verbose=2) 
anova(lm_withtrial,lm_withouttrial)
summary(lm_withtrial)

