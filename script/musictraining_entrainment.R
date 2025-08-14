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
## Load the original and the clean data, use the clean data subjects to get the language experience and the music training years
EXP1tone = read.csv("/Users/tzu-hanzoecheng/Documents/GitHub/Cross_domain_entrainment/exp8/results/old/EXP8b_clean_n84.csv")
EXP1all = read.csv("/Users/tzu-hanzoecheng/Documents/GitHub/Cross_domain_entrainment/exp8/results/old/combined_csv.csv")
EXP2tone = read.csv("/Users/tzu-hanzoecheng/Documents/GitHub/Cross_domain_entrainment/exp9ab/results/EXP9b_clean_n76.csv")
EXP2all = read.csv("/Users/tzu-hanzoecheng/Documents/GitHub/Cross_domain_entrainment/exp9ab/results/EXP9ab_combined_r1_4.csv")

## Combine the 2 experiments
tone = rbind(select(EXP1tone,participant_id,sub_id,exp,Onset,Length,Shorter,Correct),select(EXP2tone,participant_id,sub_id,exp,Onset,Length,Shorter,Correct))
all = rbind(select(EXP1all,participant_id,trial_template,response_value),select(EXP2all,participant_id,trial_template,response_value))

## Rescale the 8 lengths and mutate new factors rLength, fOnset and Explabel
tone = tone %>%
  mutate(rLength = scale(Length, center = TRUE, scale = TRUE)) 
tone = tone %>%
  mutate(fOnset = as.factor(Onset))
tone = tone %>%
  mutate(Explabel = as.factor(exp))

## Make early onset as reference
tone$fOnsetE = relevel(tone$fOnset, ref="early") # make early condition the reference 

## Sort the data to be subjects x onset for each condition
aovdata=tone %>% 
  group_by(fOnsetE,fOnset,Length,sub_id) %>% summarise(Shorter=mean(Shorter)) # Important!! Use Length for plotting (Length 1-8, instead of the scaled Length -1.52 - 1.52 for easier visualization), use rLength for fitting model & ANOVA test

## Run logistic fit on each subject and condition
aovmeans=aovdata %>% 
  group_by(sub_id,fOnsetE) %>% 
  do(glmfit = glm(Shorter ~ Length,data =.,family=binomial())) # Important!! Use Length for plotting (Length 1-8, instead of the scaled Length -1.52 - 1.52 for easier visualization), use rLength for fitting model & ANOVA test

## Get the coefficients 
aovmeans = aovmeans %>%
  mutate(intercept = coef(glmfit)[1], slope = coef(glmfit)[2], fifty = -coef(glmfit)[1]/coef(glmfit)[2], deviance = glmfit$aic)

## Mutate columes of pps
pps = aovdata %>% group_by(sub_id,fOnsetE) %>% summarise(ShorterM=mean(Shorter),ShorterSD=sd(Shorter),Nsubs=n_distinct(sub_id),SE=ShorterSD/sqrt(Nsubs))
aovmeans = cbind(pps$ShorterM,aovmeans)
colnames(aovmeans)[1] = 'Shorter'

## Flag outliers based on slope
# who have reverse slopes, flat lines
aovmeans$outliers_slope = ifelse(aovmeans$slope>= 0,1,0)
outliers_slope_subj = filter(aovmeans,outliers_slope==1)
aovmeans_clean = filter(aovmeans, !(sub_id %in% unique(outliers_slope_subj$sub_id)))

## Get the entrainment effect (early pps - late pps)
aovmeans_clean = aovmeans_clean %>%
  group_by(sub_id) %>%
  mutate(earlyVSlate = Shorter[fOnsetE == "early"] - Shorter[fOnsetE == "late"]) 

aovmeans_clean = aovmeans_clean %>%
  group_by(sub_id) %>%
  summarize(earlyVSlate = mean(earlyVSlate))

## Get descriptive stats and plot ready
summary_aovmeans_clean = aovmeans_clean %>%
  group_by(fOnsetE) %>%
  summarize(mfifty = mean(fifty), mShorter = mean(Shorter), Nsubs=n_distinct(sub_id), sefifty = sd(fifty)/sqrt(Nsubs), seShorter = sd(Shorter)/sqrt(Nsubs),sdShorter = sd(Shorter))
