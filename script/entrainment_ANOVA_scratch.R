library(lme4)
library(lmerTest)
library(tidyverse)
library(broom)
library(ggplot2)
library('fastDummies')

## Load the data
# EXPtone = read.csv("/Users/t.z.cheng/Google_Drive/Research/cross_domain_entrainment/Delaydoesmatter/real_exp/exp4_20CR12/4c/results/EXP4c_clean_n59.csv") # exp4c
# EXPspeech = read.csv("/Users/t.z.cheng/Google_Drive/Research/cross_domain_entrainment/exp6_21CR03_Vowel_length/FF2021/results/EXP6_clean_n64_v1.csv") # exp6 VL

EXPtone = read.csv("/Users/t.z.cheng/Google_Drive/Research/cross_domain_entrainment/exp8/results/EXP8b_clean_n86.csv") 
EXPspeech = read.csv("/Users/t.z.cheng/Google_Drive/Research/cross_domain_entrainment/exp8/results/EXP8a_clean_n82.csv")
EXPtoneasspeech = read.csv("/Users/t.z.cheng/Google_Drive/Research/cross_domain_entrainment/exp8/results/EXP8c_clean_n88.csv") 

## flag the overlapping subjects for EXP4c and EXP6
# overlapsubj = intersect(EXPtone$sub_id,EXPspeech$sub_id)
# length(overlapsubj)
# EXPtone$overlap = ifelse(Reduce(`|`, lapply(overlapsubj, `==`, EXPtone$sub_id)),1,0)
# EXPspeech$overlap = ifelse(Reduce(`|`, lapply(overlapsubj, `==`, EXPspeech$sub_id)),1,0)

## combine the data
# alldata=rbind(select(EXPtone,participant_id,sub_id,expt_id,Onset,Length,overlap,Shorter),select(EXPspeech,participant_id,sub_id,expt_id,Onset,Length,overlap,Shorter))
# alldata$Exptag=ifelse(alldata$expt_id=="620ddeee49655b42f1242551",1,-1)
# alldata$Explabel=ifelse(alldata$expt_id=="620ddeee49655b42f1242551","Tones","Speech")
# alldata = alldata %>%
#   filter(alldata$overlap == 0) %>%
#   droplevels

## combine the data EXP8
alldata=rbind(select(EXPtone,participant_id,sub_id,exp,Onset,Length,Shorter),select(EXPspeech,participant_id,sub_id,exp,Onset,Length,Shorter),select(EXPtoneasspeech,participant_id,sub_id,exp,Onset,Length,Shorter))

## Rescale and mutate new factors
alldata = alldata %>%
  mutate(rLength = scale(Length, center = TRUE, scale = TRUE)) # scale the steps ## 4c: scale Length; 6: scale comparison
alldata = alldata %>%
  mutate(fOnset = as.factor(Onset))
alldata = alldata %>%
  mutate(Explabel = as.factor(exp))

## Onset coding and reference 
alldata$fOnsetR = relevel(alldata$fOnset, ref="ontime") # make ontime condition the reference 
alldata$fOnsetE = relevel(alldata$fOnset, ref="early") # make early condition the reference 
alldata$OnsetNum=ifelse(alldata$fOnsetR=="early",-1,ifelse(alldata$fOnsetR=="ontime",0,1))
alldata$OnsetHelm1=ifelse(alldata$fOnsetR=="early",2/3,-1/3)
alldata$OnsetHelm2=ifelse(alldata$fOnsetR=="early",0,ifelse(alldata$fOnsetR=="ontime",1/2,-1/2))
# partialalldata = filter(alldata,alldata$Length < 6) # exclude the step 6,7,8
# alldata = partialalldata

## sort the data to be subjects x onset for each experiment
aovdata=alldata %>% #filter(comparison>=6) %>% ### if you limit speech to continua 1-5, no diffs. strong for 6-8
  group_by(fOnsetR,Explabel,OnsetNum,rLength,sub_id) %>% summarise(Shorter=mean(Shorter))

ggplot(aovdata,aes(x=rLength,y=Shorter,color=fOnsetR))+
  scale_color_manual(values=c("red","blue","gray"))+
  geom_point()+
  # geom_line()+
  #geom_smooth(method="lm",formula=y ~ exp(x)/(1+exp(x)),se=FALSE)+
  geom_smooth(method="lm",se=FALSE) +
  facet_wrap(sub_id~.)

# run logistic fit on each subject and condition
aovmeans=aovdata %>% 
  group_by(sub_id,fOnsetR,Explabel) %>% 
  do(glmfit = glm(Shorter ~ rLength,data =.,family=binomial())) 

# get the coefficients 
aovmeans = aovmeans %>%
  mutate(intercept = coef(glmfit)[1], slope = coef(glmfit)[2], fifty = -coef(glmfit)[1]/coef(glmfit)[2], deviance = glmfit$aic)

# mutate columes of pps
pps = aovdata %>% group_by(sub_id,fOnsetR,Explabel) %>% summarise(ShorterM=mean(Shorter),ShorterSD=sd(Shorter),Nsubs=n_distinct(sub_id),SE=ShorterSD/sqrt(Nsubs))
aovmeans = cbind(pps$ShorterM,aovmeans)
colnames(aovmeans)[1] = 'Shorter'

## flag outliers based on slope
# who have reverse slopes, flat lines
# 'af90a' press the same button across all experiment 
aovmeans$outliers_slope = ifelse(aovmeans$slope>= 0 | aovmeans$sub_id == 'af90a',1,0)
outliers_slope_subj = filter(aovmeans,outliers_slope==1)

aovdata_clean1 = filter(aovdata, !(sub_id %in% unique(outliers_slope_subj$sub_id)))
aovmeans_clean1 = filter(aovmeans, !(sub_id %in% unique(outliers_slope_subj$sub_id)))

## log transform to use stats criteria 
hist(log(abs(aovmeans_clean1$slope)),200)

# IQR
aovmeans_clean1$log_abs_slope = log(abs(aovmeans_clean1$slope))
q1 = quantile(aovmeans_clean1$log_abs_slope,.25)
q3 = quantile(aovmeans_clean1$log_abs_slope,.75)
iqr = IQR(aovmeans_clean1$log_abs_slope)

aovmeans_clean1$outliers_slope = ifelse(aovmeans_clean1$log_abs_slope < (q1 - 1.5*iqr) | aovmeans_clean1$log_abs_slope > (q3 + 1.5*iqr),1,0)
outliers_slope_subj = filter(aovmeans_clean1,outliers_slope==1)
aovmeans_clean2 = filter(aovmeans_clean1, !(sub_id %in% unique(outliers_slope_subj$sub_id)))

## flag outliers based on 50% point 
# plot the histogram
hist(aovmeans_clean2$fifty,200)
ggplot(aovmeans_clean,aes(x=fifty ,color=fOnsetR))+
  geom_histogram()+
  facet_grid(fOnsetR ~ Explabel)

# 3*SD criteria
ub = mean(aovmeans_clean2$fifty) + 3*sd(aovmeans_clean2$fifty)
lb = mean(aovmeans_clean2$fifty) - 3*sd(aovmeans_clean2$fifty)
aovmeans_outlier = aovmeans_clean2 %>%
  filter(fifty < lb | fifty > ub)
aovmeans_clean2$outliers=ifelse(aovmeans_clean2$sub_id %in% unique(aovmeans_outlier$sub_id),"outliers","clean")

# raw 50% point except for the subject af90a
aovmeans_clean4 = filter(aovmeans,sub_id != 'f6891')

# 3*SD criteria 
ub = median(aovmeans_clean4$fifty) + 3*sd(aovmeans_clean4$fifty)
lb = median(aovmeans_clean4$fifty) - 3*sd(aovmeans_clean4$fifty)
aovmeans_outlier = aovmeans_clean4 %>%
  filter(fifty < lb | fifty > ub)
aovmeans_clean4$outliers=ifelse(aovmeans_clean4$sub_id %in% unique(aovmeans_outlier$sub_id),"outliers","clean")
aovmeans_clean4 = aovmeans_clean4 %>%
  filter(outliers == 'clean')

# IQR criteria
q1 = quantile(aovmeans_clean4$fifty,.25)
q3 = quantile(aovmeans_clean4$fifty,.75)
iqr = IQR(aovmeans_clean4$fifty)

aovmeans_clean4$outliers = ifelse(aovmeans_clean4$fifty < (q1 - 1.5*iqr) | aovmeans_clean4$fifty > (q3 + 1.5*iqr),1,0)
outliers_subj = filter(aovmeans_clean4,outliers==1)
aovmeans_clean4 = filter(aovmeans_clean4, !(sub_id %in% unique(outliers_subj$sub_id)))
hist(aovmeans_clean4$fifty,200)

# Descriptive stats
aovmeans_clean4 = aovmeans_clean4 %>%
  filter(outliers == 'clean')
aovmeans_clean4 %>%
  group_by(Explabel,fOnsetR) %>%
  summarize(mean(fifty),mean(intercept),mean(Shorter))

# individual plot 
aovdata_clean = filter(aovdata, sub_id %in% unique(aovmeans_clean4$sub_id)) 

ggplot(aovdata_clean,aes(x=rLength,y=Shorter,color=fOnsetR))+
  scale_color_manual(values=c("red","blue","gray"))+
  geom_point()+
  # geom_line()+
  #geom_smooth(method="lm",formula=y ~ exp(x)/(1+exp(x)),se=FALSE)+
  geom_smooth(method="lm",se=FALSE) +
  facet_wrap(sub_id~.)

## relabel Speech to EXP8a and Tone to EXP8b and 
aovmeans_clean4$Explabel = ifelse(aovmeans_clean4$Explabel == "EXP8a","Speech",ifelse(aovmeans_clean4$Explabel == "EXP8b","Tones","ToneasSpeech"))
aovmeans_clean2$Explabel = ifelse(aovmeans_clean2$Explabel == "EXP8a","Speech",ifelse(aovmeans_clean2$Explabel == "EXP8b","Tones","ToneasSpeech"))

## ANOVA on proportion short
summary(aov(Shorter~fOnsetR*Explabel+Error(sub_id/fOnsetR),data=aovmeans_clean2)) 
summary(aov(Shorter~fOnsetR+Error(sub_id/fOnsetR),data=filter(aovmeans_clean2,Explabel=="Speech"))) 
summary(aov(Shorter~fOnsetR+Error(sub_id/fOnsetR),data=filter(aovmeans_clean2,Explabel=="Tones"))) 
summary(aov(Shorter~fOnsetR+Error(sub_id/fOnsetR),data=filter(aovmeans_clean2,Explabel=="ToneasSpeech"))) 

summary(aov(Shorter~fOnsetR*Explabel+Error(sub_id/fOnsetR),data=filter(aovmeans_clean3,Explabel!="Speech"))) 
summary(aov(Shorter~fOnsetR*Explabel+Error(sub_id/fOnsetR),data=filter(aovmeans_clean3,Explabel!="Tones"))) 
summary(aov(Shorter~fOnsetR*Explabel+Error(sub_id/fOnsetR),data=filter(aovmeans_clean3,Explabel!="ToneasSpeech"))) 

# Speech
t.test(filter(aovmeans_clean2,Explabel=="Speech" & fOnsetR=="early")$Shorter,filter(aovmeans_clean2,Explabel=="Speech" & fOnsetR=="ontime")$Shorter,paired=T)
t.test(filter(aovmeans_clean2,Explabel=="Speech" & fOnsetR=="early")$Shorter,filter(aovmeans_clean2,Explabel=="Speech" & fOnsetR=="late")$Shorter,paired=T)
t.test(filter(aovmeans_clean2,Explabel=="Speech" & fOnsetR=="late")$Shorter,filter(aovmeans_clean2,Explabel=="Speech" & fOnsetR=="ontime")$Shorter,paired=T)
# Tone
t.test(filter(aovmeans_clean2,Explabel=="Tones" & fOnsetR=="early")$Shorter,filter(aovmeans_clean2,Explabel=="Tones" & fOnsetR=="ontime")$Shorter,paired=T)
t.test(filter(aovmeans_clean2,Explabel=="Tones" & fOnsetR=="early")$Shorter,filter(aovmeans_clean2,Explabel=="Tones" & fOnsetR=="late")$Shorter,paired=T)
t.test(filter(aovmeans_clean2,Explabel=="Tones" & fOnsetR=="late")$Shorter,filter(aovmeans_clean2,Explabel=="Tones" & fOnsetR=="ontime")$Shorter,paired=T)
# ToneasSpeech
t.test(filter(aovmeans_clean2,Explabel=="ToneasSpeech" & fOnsetR=="early")$Shorter,filter(aovmeans_clean2,Explabel=="ToneasSpeech" & fOnsetR=="ontime")$Shorter,paired=T)
t.test(filter(aovmeans_clean2,Explabel=="ToneasSpeech" & fOnsetR=="early")$Shorter,filter(aovmeans_clean2,Explabel=="ToneasSpeech" & fOnsetR=="late")$Shorter,paired=T)
t.test(filter(aovmeans_clean2,Explabel=="ToneasSpeech" & fOnsetR=="late")$Shorter,filter(aovmeans_clean2,Explabel=="ToneasSpeech" & fOnsetR=="ontime")$Shorter,paired=T)

## ANOVA on 50% point
summary(aov(fifty~fOnsetR*Explabel+Error(sub_id/fOnsetR),data=aovmeans_clean2)) 
summary(aov(fifty~fOnsetR+Error(sub_id/fOnsetR),data=filter(aovmeans_clean2,Explabel=="Speech"))) 
summary(aov(fifty~fOnsetR+Error(sub_id/fOnsetR),data=filter(aovmeans_clean2,Explabel=="Tones"))) 
summary(aov(fifty~fOnsetR+Error(sub_id/fOnsetR),data=filter(aovmeans_clean2,Explabel=="ToneasSpeech"))) 
m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[2]/sum(m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`)
eta_squared(aov(fifty~fOnsetR*Explabel+Error(sub_id/fOnsetR),data=aovmeans_clean3))

summary(aov(fifty~fOnsetR*Explabel+Error(sub_id/fOnsetR),data=filter(aovmeans_clean3,Explabel!="Speech"))) 
summary(aov(fifty~fOnsetR*Explabel+Error(sub_id/fOnsetR),data=filter(aovmeans_clean3,Explabel!="Tones"))) 
summary(aov(fifty~fOnsetR*Explabel+Error(sub_id/fOnsetR),data=filter(aovmeans_clean3,Explabel!="ToneasSpeech"))) 

# Speech
t.test(filter(aovmeans_clean3,Explabel=="Speech" & fOnsetR=="early")$fifty,filter(aovmeans_clean3,Explabel=="Speech" & fOnsetR=="ontime")$fifty,paired=T)
t.test(filter(aovmeans_clean3,Explabel=="Speech" & fOnsetR=="early")$fifty,filter(aovmeans_clean3,Explabel=="Speech" & fOnsetR=="late")$fifty,paired=T)
t.test(filter(aovmeans_clean3,Explabel=="Speech" & fOnsetR=="late")$fifty,filter(aovmeans_clean3,Explabel=="Speech" & fOnsetR=="ontime")$fifty,paired=T)
# Tone
t.test(filter(aovmeans_clean3,Explabel=="Tones" & fOnsetR=="early")$fifty,filter(aovmeans_clean3,Explabel=="Tones" & fOnsetR=="ontime")$fifty,paired=T)
t.test(filter(aovmeans_clean3,Explabel=="Tones" & fOnsetR=="early")$fifty,filter(aovmeans_clean3,Explabel=="Tones" & fOnsetR=="late")$fifty,paired=T)
t.test(filter(aovmeans_clean3,Explabel=="Tones" & fOnsetR=="late")$fifty,filter(aovmeans_clean3,Explabel=="Tones" & fOnsetR=="ontime")$fifty,paired=T)
# ToneasSpeech
t.test(filter(aovmeans_clean3,Explabel=="ToneasSpeech" & fOnsetR=="early")$fifty,filter(aovmeans_clean3,Explabel=="ToneasSpeech" & fOnsetR=="ontime")$fifty,paired=T)
t.test(filter(aovmeans_clean3,Explabel=="ToneasSpeech" & fOnsetR=="early")$fifty,filter(aovmeans_clean3,Explabel=="ToneasSpeech" & fOnsetR=="late")$fifty,paired=T)
t.test(filter(aovmeans_clean3,Explabel=="ToneasSpeech" & fOnsetR=="late")$fifty,filter(aovmeans_clean3,Explabel=="ToneasSpeech" & fOnsetR=="ontime")$fifty,paired=T)
