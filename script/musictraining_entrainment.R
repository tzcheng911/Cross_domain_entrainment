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
## Pilot: EXP4c has both language and music background; EXP4ab only has music background
## Real: EXP8, 9, 10
EXP4ctone = read.csv("/Users/tzu-hanzoecheng/Documents/Research/cross_domain_entrainment/Delaydoesmatter/real_exp/exp4_20CR12/4c/results/EXP4c_clean_n71.csv")
EXP4call = read.csv("/Users/tzu-hanzoecheng/Documents/Research/cross_domain_entrainment/Delaydoesmatter/real_exp/exp4_20CR12/4c/results/EXP4c_combined.csv")

## Combine across experiments
# tone = rbind(select(EXP4atone,participant_id,exp,Onset,Length,Shorter),select(EXP4btone,participant_id,exp,Onset,Length,Shorter),select(EXP4ctone,participant_id,exp,Onset,Length,Shorter))
# all = rbind(select(EXP4aall,participant_id,trial_template,response_value),select(EXP4ball,participant_id,trial_template,response_value),select(EXP4call,participant_id,trial_template,response_value))
tone = EXP4ctone
all = EXP4call
all = all %>%
  mutate(sub_id = substr(participant_id,start = 1, stop = 5))
lang = filter(all, trial_template == "langBackground")
music = filter(all, trial_template == "musicBackground")
age_gender = filter(all, trial_template == "basicDialogue")

## Rescale the 8 lengths and mutate new factors rLength, fOnset and Explabel
tone = tone %>%
  mutate(rLength = scale(Length, center = TRUE, scale = TRUE)) 
tone = tone %>%
  mutate(fOnset = as.factor(Onset))
tone = tone %>%
  mutate(Explabel = as.factor(exp))
tone = tone %>%
  mutate(sub_id = substr(participant_id,start = 1, stop = 5))

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
aovmeans_clean0 = filter(aovmeans, !(sub_id %in% unique(outliers_slope_subj$sub_id)))

## Get the entrainment effect (early pps - late pps)
aovmeans_clean = aovmeans_clean0 %>%
  group_by(sub_id) %>%
  mutate(PPSearlyVSlate = Shorter[fOnsetE == "early"] - Shorter[fOnsetE == "late"]) %>%
  mutate(fiftyearlyVSlate = fifty[fOnsetE == "early"] - fifty[fOnsetE == "late"])

aovmeans_clean = aovmeans_clean %>%
  group_by(sub_id) %>%
  summarize(PPSearlyVSlate = mean(PPSearlyVSlate), fiftyearlyVSlate = mean(fiftyearlyVSlate))

## Get descriptive stats and plot ready
summary_aovmeans_clean = aovmeans_clean0 %>%
  group_by(fOnsetE) %>%
  summarize(mfifty = mean(fifty), mShorter = mean(Shorter), Nsubs=n_distinct(sub_id), sefifty = sd(fifty)/sqrt(Nsubs), seShorter = sd(Shorter)/sqrt(Nsubs),sdShorter = sd(Shorter))

## Get the lang and music survey from subjects who have entrainment effect 
lang = select(filter(lang, sub_id %in% aovmeans_clean$sub_id),sub_id,response_value)
music = select(filter(music, sub_id %in% aovmeans_clean$sub_id),sub_id,response_value)
age_gender = select(filter(age_gender, sub_id %in% aovmeans_clean$sub_id),sub_id,response_value)
gender = age_gender[seq(1,nrow(age_gender),by = 5),1:2]
age = age_gender[seq(2,nrow(age_gender),by = 5),1:2]
sorted_gender <- gender[order(gender$sub_id), 2]
sorted_age <- age[order(age$sub_id), 2]
aovmeans_clean$gender = sorted_gender
aovmeans_clean$age = sorted_age

## Make the negative values 0
aovmeans_clean$PPSearlyVSlate[aovmeans_clean$PPSearlyVSlate < 0] <- 0
aovmeans_clean$fiftyearlyVSlate[aovmeans_clean$fiftyearlyVSlate < 0] <- 0

write.csv(aovmeans_clean,"~/Downloads/filename.csv", row.names = FALSE)

## Calculate for each step
aovdata_clean = select(filter(aovdata, sub_id %in% aovmeans_clean$sub_id),sub_id,fOnsetE,Length, Shorter)

epsilon <- 1e-6 

df = aovdata_clean %>%
  group_by(sub_id, Length, fOnsetE) %>%
  summarise(mean_shorter = mean(Shorter, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = fOnsetE,
    values_from = mean_shorter
  ) %>%
  filter(late != 0) %>%
  mutate(ratio = early / late - 1)

df$ratio[df$ratio<0] = 0 # truncate the late > early == late = early

## set the negative values to zeros
df_group <- df %>%
  group_by(sub_id) %>%
  summarise(
    mean_ratio = mean(ratio, na.rm = TRUE),
    sd_ratio   = sd(ratio, na.rm = TRUE),
    n          = sum(!is.na(ratio)),
    .groups = "drop"
  )

write.csv(df_group,"~/Downloads/filename.csv", row.names = FALSE)

# things to deal with 
# 1. contain 0
# 2. the ratio can be much more than 1

ggplot(aovdata,aes(x=Length,y=Shorter,color=fOnsetE))+
  scale_color_manual(values=c("red","blue","gray"))+
  geom_point()+
  # geom_line()+
  #geom_smooth(method="lm",formula=y ~ exp(x)/(1+exp(x)),se=FALSE)+
  geom_smooth(method="lm",se=FALSE) +
  facet_wrap(sub_id~.)
