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
## Pilot: EXP4abc
## Real: EXP8, 9, 10
EXP4atone = read.csv("/Users/tzu-hanzoecheng/Documents/Research/cross_domain_entrainment/Delaydoesmatter/real_exp/exp4_20CR12/4ab/results_shortdelay_2020/EXP4a_clean_n53.csv")
EXP4aall = read.csv("/Users/tzu-hanzoecheng/Documents/Research/cross_domain_entrainment/Delaydoesmatter/real_exp/exp4_20CR12/4ab/results_shortdelay_2020/20CR12_5fb6ae2b172cc12b1c5b3050-data.csv")
EXP4btone = read.csv("/Users/tzu-hanzoecheng/Documents/Research/cross_domain_entrainment/Delaydoesmatter/real_exp/exp4_20CR12/4ab/results_shortlongdelay_2021/EXP4b_clean_n67.csv")
EXP4ball = read.csv("/Users/tzu-hanzoecheng/Documents/Research/cross_domain_entrainment/Delaydoesmatter/real_exp/exp4_20CR12/4ab/results_shortlongdelay_2021/EXP4b_combined.csv")
EXP4ctone = read.csv("/Users/tzu-hanzoecheng/Documents/Research/cross_domain_entrainment/Delaydoesmatter/real_exp/exp4_20CR12/4c/results/EXP4c_clean_n71.csv")
EXP4call = read.csv("/Users/tzu-hanzoecheng/Documents/Research/cross_domain_entrainment/Delaydoesmatter/real_exp/exp4_20CR12/4c/results/EXP4c_combined.csv")

## Combine the 3 experiments
tone = rbind(select(EXP4atone,participant_id,exp,Onset,Length,Shorter),select(EXP4btone,participant_id,exp,Onset,Length,Shorter),select(EXP4ctone,participant_id,exp,Onset,Length,Shorter))
all = rbind(select(EXP4aall,participant_id,trial_template,response_value),select(EXP4ball,participant_id,trial_template,response_value),select(EXP4call,participant_id,trial_template,response_value))
lang = filter(all, trial_template == "langBackground")
music = filter(all, trial_template == "musicBackground")

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
  group_by(fOnsetE,fOnset,Length,participant_id) %>% summarise(Shorter=mean(Shorter)) # Important!! Use Length for plotting (Length 1-8, instead of the scaled Length -1.52 - 1.52 for easier visualization), use rLength for fitting model & ANOVA test

## Run logistic fit on each subject and condition
aovmeans=aovdata %>% 
  group_by(participant_id,fOnsetE) %>% 
  do(glmfit = glm(Shorter ~ Length,data =.,family=binomial())) # Important!! Use Length for plotting (Length 1-8, instead of the scaled Length -1.52 - 1.52 for easier visualization), use rLength for fitting model & ANOVA test

## Get the coefficients 
aovmeans = aovmeans %>%
  mutate(intercept = coef(glmfit)[1], slope = coef(glmfit)[2], fifty = -coef(glmfit)[1]/coef(glmfit)[2], deviance = glmfit$aic)

## Mutate columes of pps
pps = aovdata %>% group_by(participant_id,fOnsetE) %>% summarise(ShorterM=mean(Shorter),ShorterSD=sd(Shorter),Nsubs=n_distinct(participant_id),SE=ShorterSD/sqrt(Nsubs))
aovmeans = cbind(pps$ShorterM,aovmeans)
colnames(aovmeans)[1] = 'Shorter'

## Flag outliers based on slope
# who have reverse slopes, flat lines
aovmeans$outliers_slope = ifelse(aovmeans$slope>= 0,1,0)
outliers_slope_subj = filter(aovmeans,outliers_slope==1)
aovmeans_clean = filter(aovmeans, !(participant_id %in% unique(outliers_slope_subj$participant_id)))

## Get the entrainment effect (early pps - late pps)
aovmeans_clean = aovmeans_clean %>%
  group_by(participant_id) %>%
  mutate(earlyVSlate = Shorter[fOnsetE == "early"] - Shorter[fOnsetE == "late"]) 

aovmeans_clean = aovmeans_clean %>%
  group_by(participant_id) %>%
  summarize(earlyVSlate = mean(earlyVSlate))

## Get descriptive stats and plot ready
summary_aovmeans_clean = aovmeans_clean %>%
  group_by(fOnsetE) %>%
  summarize(mfifty = mean(fifty), mShorter = mean(Shorter), Nsubs=n_distinct(participant_id), sefifty = sd(fifty)/sqrt(Nsubs), seShorter = sd(Shorter)/sqrt(Nsubs),sdShorter = sd(Shorter))

## Get the lang and music survey from subjects who have entrainment effect 
lang = filter(lang, participant_id %in% aovmeans_clean$participant_id)
music = filter(music, participant_id %in% aovmeans_clean$participant_id)
