##Load the packages
library(lme4)
library(lmerTest)
library(tidyverse)
library(broom)
library(effsize)
library('fastDummies')

## Load the data
mydataEXP4 = read.csv("/Users/t.z.cheng/Google_Drive/Research/cross_domain_entrainment/Delaydoesmatter/real_exp/exp4_20CR12/4c/results/EXP4c_clean_n59.csv") # exp4c
mydataEXP6 = read.csv("/Users/t.z.cheng/Google_Drive/Research/cross_domain_entrainment/exp6_21CR03_Vowel_length/FF2021/results/EXP6_clean_n64_v1.csv") # exp6 VL

## flag the overlapping subjects
overlapsubj = intersect(mydataEXP4$sub_id,mydataEXP6$sub_id)
length(overlapsubj)
mydataEXP4$overlap = ifelse(Reduce(`|`, lapply(overlapsubj, `==`, mydataEXP4$sub_id)),1,0)
mydataEXP6$overlap = ifelse(Reduce(`|`, lapply(overlapsubj, `==`, mydataEXP6$sub_id)),1,0)

## combine the data
alldata=rbind(select(mydataEXP4,participant_id,sub_id,expt_id,Onset,Length,overlap,Shorter),select(mydataEXP6,participant_id,sub_id,expt_id,Onset,Length,overlap,Shorter))
alldata$Exptag=ifelse(alldata$expt_id=="620ddeee49655b42f1242551",1,-1)
alldata$Explabel=ifelse(alldata$expt_id=="620ddeee49655b42f1242551","Tones","Speech")
alldata = alldata %>%
  filter(alldata$overlap == 0) %>%
  droplevels

## Rescale and mutate new factors
alldata = alldata %>%
  mutate(rLength = scale(Length, center = TRUE, scale = TRUE)) # scale the steps ## 4c: scale Length; 6: scale comparison
alldata = alldata %>%
  mutate(fOnset = as.factor(Onset))

## Onset coding and reference 
alldata$fOnsetR = relevel(alldata$fOnset, ref="ontime") # make ontime condition the reference 
alldata$fOnsetE = relevel(alldata$fOnset, ref="early") # make ontime condition the reference 
alldata$OnsetNum=ifelse(alldata$fOnsetR=="early",-1,ifelse(alldata$fOnsetR=="ontime",0,1))
alldata$OnsetHelm1=ifelse(alldata$fOnsetR=="early",2/3,-1/3)
alldata$OnsetHelm2=ifelse(alldata$fOnsetR=="early",0,ifelse(alldata$fOnsetR=="ontime",1/2,-1/2))

## very flat slopes, maybe too hard to estimate
allbadsubs=c("394a9","5a2fc","6222b","6ae89","8be33","a4432","bf5f1","ce0e9","16b53","421ca","488cc","5d964","6933d","97e84","c3ee1") # by Zoe
alldata$badsubs=ifelse(alldata$sub_id %in% allbadsubs,"badsub","ok")

## glm for loop for each subject
all_sub_id = unique(alldata$sub_id)
all_onset = unique(alldata$Onset)

tmp_intercept = array(1000, dim = c(101,3)) # initialize to 1000, an impossible value for intercept or goodness or 50point, so failures to update this vector are easily spotted
tmp_goodness = array(1000, dim = c(101,3))
tmp_50point = array(1000, dim = c(101,3))

for(i in 1:length(all_sub_id)){
  for(j in 1:length(all_onset)){
    fit = glm(Shorter ~ rLength,data= filter(alldata,sub_id == all_sub_id[i] & Onset == all_onset[j]),family=binomial())
    tmp_intercept[i,j] = fit$coefficients["(Intercept)"]
    tmp_goodness[i,j] = fit$deviance
    tmp_50point[i,j] = -fit$coefficients['(Intercept)']/fit$coefficients['rLength']
  }}

## There is an easier way to do it in broom, see entrainment_ANOVA.R 

## All as expected
colMeans(tmp_intercept)
colMeans(tmp_goodness)
colMeans(tmp_50point)
summary(fit) # display results

## Store the value back to alldata

# use these to exclude subjects?
residuals(fit, type="deviance") # residuals
confint(fit) # 95% CI for the coefficients
