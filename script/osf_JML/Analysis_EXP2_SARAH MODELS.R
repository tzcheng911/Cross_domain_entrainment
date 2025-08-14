library(lme4)
library(lmerTest)
library(tidyverse)
library(broom)
library(ggplot2)
library(ggpubr)
library('fastDummies')
library(effsize)
library(lsr)

# SARAH
setwd("~/Documents/current studies/A0 priority/Zoe speech study 2 (FindingFive version)/Zoe code 7-15-2025")
# /SARAH


######################################################## Preprocessing ######################################################## 
## Load the data
EXPspeech = read.csv("EXP2a_clean_n79.csv")
EXPtone = read.csv("EXP2b_clean_n76.csv")

## Combine the 2 conditions
alldata=rbind(select(EXPtone,participant_id,sub_id,exp,Onset,Length,Shorter,Correct),select(EXPspeech,participant_id,sub_id,exp,Onset,Length,Shorter,Correct))

## Rescale the 8 lengths and mutate new factors rLength, fOnset and Explabel
alldata = alldata %>%
  mutate(rLength = scale(Length, center = TRUE, scale = TRUE)) 
alldata = alldata %>%
  mutate(fOnset = as.factor(Onset))
alldata = alldata %>%
  mutate(Explabel = as.factor(exp))

## Make early onset as reference
alldata$fOnsetE = relevel(alldata$fOnset, ref="early") # make early condition the reference 

## Sort the data to be subjects x onset for each condition
aovdata=alldata %>% 
  group_by(fOnsetE,Explabel,fOnset,rLength,sub_id) %>% summarise(Shorter=mean(Shorter)) # change rLength to Length for visualization

## Run logistic fit on each subject and condition
aovmeans=aovdata %>% 
  group_by(sub_id,fOnsetE,Explabel) %>% 
  do(glmfit = glm(Shorter ~ rLength,data =.,family=binomial())) # change rLength to Length for visualization

## Get the coefficients 
aovmeans = aovmeans %>%
  mutate(intercept = coef(glmfit)[1], slope = coef(glmfit)[2], fifty = -coef(glmfit)[1]/coef(glmfit)[2], deviance = glmfit$aic)

## Mutate columes of pps
pps = aovdata %>% group_by(sub_id,fOnsetE,Explabel) %>% summarise(ShorterM=mean(Shorter),ShorterSD=sd(Shorter),Nsubs=n_distinct(sub_id),SE=ShorterSD/sqrt(Nsubs))
aovmeans = cbind(pps$ShorterM,aovmeans)
colnames(aovmeans)[1] = 'Shorter'

## Flag outliers based on slope
# who have reverse slopes, flat lines
# '8db1d','074c2' press the same button across all experiment 
aovmeans$outliers_slope = ifelse(aovmeans$slope>= 0 | aovmeans$sub_id == '8db1d' | aovmeans$sub_id == '074c2',1,0)
outliers_slope_subj = filter(aovmeans,outliers_slope==1)
aovmeans_clean1 = filter(aovmeans, !(sub_id %in% unique(outliers_slope_subj$sub_id)))

## Flag outliers based on 50% point based on 1.5*IQR criteria
q1 = quantile(aovmeans_clean1$fifty,.25)
q3 = quantile(aovmeans_clean1$fifty,.75)
iqr = IQR(aovmeans_clean1$fifty)
aovmeans_clean1$outliers_50 = ifelse(aovmeans_clean1$fifty < (q1 - 1.5*iqr) | aovmeans_clean1$fifty > (q3 + 1.5*iqr),1,0)
outliers_subj_50 = filter(aovmeans_clean1,outliers_50==1)
aovmeans_clean2 = filter(aovmeans_clean1, !(sub_id %in% unique(outliers_subj_50$sub_id)))
hist(aovmeans_clean2$fifty,200)

## Get descriptive stats and plot ready
summary_aovmeans_clean2 = aovmeans_clean2 %>%
  group_by(Explabel,fOnsetE) %>%
  summarize(mfifty = mean(fifty), mShorter = mean(Shorter), Nsubs=n_distinct(sub_id), sefifty = sd(fifty)/sqrt(Nsubs), seShorter = sd(Shorter)/sqrt(Nsubs),sdShorter = sd(Shorter))

aovdata_clean = filter(aovdata, sub_id %in% unique(aovmeans_clean2$sub_id)) 
aovdata_outlier_slope = filter(aovdata, sub_id %in% unique(outliers_slope_subj$sub_id)) 
aovdata_outlier_50 = filter(aovdata, sub_id %in% unique(outliers_subj_50$sub_id)) 
aovdata_clean$fOnsetE = factor(aovdata_clean$fOnsetE, levels = c("early","ontime","late"))
aovdata_outlier_slope$fOnsetE = factor(aovdata_outlier_slope$fOnsetE, levels = c("early","ontime","late"))
aovdata_outlier_50$fOnsetE = factor(aovdata_outlier_50$fOnsetE, levels = c("early","ontime","late"))
aovdata_clean_plot = aovdata_clean %>% group_by(rLength,fOnsetE,Explabel) %>% summarise(mShorter=mean(Shorter),SD=sd(Shorter),Nsubs=n_distinct(sub_id))
aovdata_clean_plot$fOnsetE = factor(aovdata_clean_plot$fOnsetE, levels = c("early","ontime","late"))
aovdata_clean$Explabel = factor(aovdata_clean$Explabel, levels = c("EXP9a","EXP9b"))
aovdata_outlier_slope$Explabel = factor(aovdata_outlier_slope$Explabel, levels = c("EXP9a","EXP9b"))
aovdata_outlier_50$Explabel = factor(aovdata_outlier_50$Explabel, levels = c("EXP9a","EXP9b"))
aovdata_clean_plot$Explabel = factor(aovdata_clean_plot$Explabel, levels = c("EXP9a","EXP9b"))

ggplot(aovdata_clean_plot,aes(x=rLength,y=mShorter,color=fOnsetE,linetype=Explabel,group=interaction(fOnsetE,Explabel)))+
  geom_point()+
  scale_x_continuous(breaks = seq(1, 8, by = 1))+
  geom_line()+
  geom_errorbar(aes(ymin=mShorter-SD/sqrt(Nsubs),ymax=mShorter+SD/sqrt(Nsubs)),width=0)+
  facet_grid(Explabel~.)+
  theme_bw()

# Plot individual curves of the clean data
# ggplot(aovdata_outlier_50,aes(x=Length,y=Shorter,color=fOnsetE,shape=Explabel))+
#   scale_color_manual(values=c("red","green","blue"))+
#   geom_point()+
#   # geom_line()+
#   # geom_smooth(method="lm",formula=y ~ exp(x)/(1+exp(x)),se=FALSE)+
#   # geom_smooth(method="lm",se=FALSE) +
#   geom_smooth(method="glm",method.args = list(family = "binomial"),se=FALSE) +
#   facet_wrap(sub_id~.) +
#   theme(strip.text.x = element_blank())

## Relabel Exp9ab to Speech and Tone
aovmeans_clean2$Explabel = ifelse(aovmeans_clean2$Explabel == "EXP9a","Speech","Tones")
aovmeans_clean2$Explabel = factor(aovmeans_clean2$Explabel, levels = c("Speech","Tones"))
aovmeans_clean2$fOnsetE = factor(aovmeans_clean2$fOnsetE, levels = c("early","ontime","late"))

## Plot the bar plot: can change the rlength to length so the 50% point is more interpretable
ggplot(aovmeans_clean2, aes(x = Explabel, y = Shorter, fill = fOnsetE)) +
  geom_bar(stat="summary", fun.y = "mean", position='dodge') +
  stat_summary(fun.data=mean_se, geom="errorbar", position = position_dodge(width = 0.9), width=.1,color="grey") +
  ylim(0,0.8) + 
  geom_point(position = position_jitterdodge(jitter.width = 0.3,dodge.width = 0.9), color="black")+
  theme_bw()

ggplot(aovmeans_clean2, aes(x = Explabel, y = fifty, fill = fOnsetE)) +
  geom_bar(stat="summary", fun.y = "mean", position='dodge') +
  stat_summary(fun.data=mean_se, geom="errorbar", position = position_dodge(width = 0.9), width=.1,color="grey") +
  ylim(0,8) + 
  geom_point(position = position_jitterdodge(jitter.width = 0.3,dodge.width = 0.9), color="black")+
  theme_bw()

## Get final sample size after excluding the outliers
exp9a = filter(aovmeans_clean2,Explabel == "Speech")
exp9b = filter(aovmeans_clean2,Explabel == "Tones")
length(unique(exp9a$sub_id))
length(unique(exp9b$sub_id))

######################################################## Statistical test ######################################################## 
## Run logistic mixed-effect model 
alldata_clean = filter(alldata, sub_id %in% unique(aovmeans_clean2$sub_id)) 
alldata_clean$Explabel = ifelse(alldata_clean$Explabel=="EXP9a",-0.5,0.5) # sum coding for the two conditions being compared


alldata_clean_exp2 = alldata_clean %>%
    mutate(earlyVSontime=ifelse(fOnsetE=="ontime",2/3,-1/3),
         earlyVSlate=ifelse(fOnsetE=="late",2/3,-1/3), # note that early is always -1/3, so it's reference level
         speechVSothers=ifelse(exp=="EXP9a",-.5,.5)) # 8b = tone, 8c = tas
# check the recoding
alldata_clean_exp2 %>% group_by(exp,speechVSothers,
                                       earlyVSontime,earlyVSlate) %>% summarise(n())

##

# drop some fx--geez. singular down to rLength
lm_Sarah = glmer(Shorter ~ speechVSothers*(earlyVSontime+earlyVSlate)*rLength  + 
                   (1 + rLength|sub_id),
                 data= alldata_clean_exp2,family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
summary(lm_Sarah)
# Number of obs: 40608, groups:  sub_id, 141
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                          -0.36668    0.05514  -6.650 2.93e-11 ***
# speechVSothers                       -0.37446    0.11010  -3.401 0.000671 ***
# earlyVSontime                        -0.12273    0.03094  -3.966 7.30e-05 ***
# earlyVSlate                          -0.20717    0.03119  -6.642 3.09e-11 ***
# rLength                              -1.69875    0.06242 -27.215  < 2e-16 ***
# speechVSothers:earlyVSontime         -0.10879    0.06187  -1.758 0.078666 .  
# speechVSothers:earlyVSlate           -0.17458    0.06236  -2.800 0.005115 ** 
# speechVSothers:rLength                0.02021    0.12437   0.163 0.870896    
# earlyVSontime:rLength                 0.01729    0.03606   0.480 0.631565    
# earlyVSlate:rLength                  -0.01773    0.03638  -0.487 0.625978    
# speechVSothers:earlyVSontime:rLength -0.13583    0.07211  -1.884 0.059609 .  
# speechVSothers:earlyVSlate:rLength   -0.26588    0.07274  -3.655 0.000257 ***

# Target Type x Onset: SIGNIFICANT
lm_Sarah_nointeraction = glmer(Shorter ~ speechVSothers*(earlyVSontime+earlyVSlate)*rLength  -
                                 speechVSothers:(earlyVSontime+earlyVSlate) + # i.e., the targetType x delay interaction 
                                 (1 + rLength | sub_id),
                               data= alldata_clean_exp2,family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
anova(lm_Sarah,lm_Sarah_nointeraction) 
# lm_Sarah_nointeraction   13 39034 39146 -19504    39008                       
# lm_Sarah                 15 39030 39160 -19500    39000 7.9641  2    0.01865 *

# Speech condition alone
lmSp = glmer(Shorter ~ (earlyVSontime+earlyVSlate)*rLength  + 
                   (1 + rLength|sub_id),
                 data= filter(alldata_clean_exp2,exp=="EXP9a"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
summary(lmSp)
# earlyVSontime         -0.06854    0.04363  -1.571  0.11623    
# earlyVSlate           -0.12025    0.04356  -2.760  0.00577 ** 

# Speech onset effect: SIGNIFICANT
lmSpNoOnset = glmer(Shorter ~ (earlyVSontime+earlyVSlate)*rLength  -
                      (earlyVSontime+earlyVSlate) + 
                  (1 + rLength|sub_id),
                data= filter(alldata_clean_exp2,exp=="EXP9a"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
anova(lmSp,lmSpNoOnset)
# lmSpNoOnset    7 19630 19686 -9808.2    19616                       
# lmSp           9 19627 19698 -9804.4    19609 7.6081  2    0.02228 *
  

# Tone condition alone  
lmTn = glmer(Shorter ~ (earlyVSontime+earlyVSlate)*rLength  + 
               (1 + rLength|sub_id),
             data= filter(alldata_clean_exp2,exp=="EXP9b"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
summary(lmTn)
# earlyVSontime         -0.17659    0.04389  -4.024 5.73e-05 ***
# earlyVSlate           -0.29358    0.04465  -6.575 4.85e-11 ***

# Tone onset effect: SIGNIFICANT
lmTnNoOnset = glmer(Shorter ~ (earlyVSontime+earlyVSlate)*rLength  -
                      (earlyVSontime+earlyVSlate) + 
                      (1 + rLength|sub_id),
                    data= filter(alldata_clean_exp2,exp=="EXP9b"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
anova(lmTn,lmTnNoOnset)
# lmTnNoOnset    7 19435 19490 -9710.5    19421                         
# lmTn           9 19395 19466 -9688.5    19377 43.974  2  2.825e-10 ***



########### END OF SARAH GLMER SECTION ###########
########### END OF SARAH GLMER SECTION ###########
########### END OF SARAH GLMER SECTION ###########
########### END OF SARAH GLMER SECTION ###########
########### END OF SARAH GLMER SECTION ###########






# Full model
lmall = glmer(Shorter ~ Explabel*fOnsetE*rLength  + (1 + fOnsetE*rLength|sub_id),data= alldata_clean,family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
summary(lmall) # Use early as the reference

# Reduce Target Duration (rLength)
lmall_norLength = glmer(Shorter ~ Explabel*fOnsetE*rLength-rLength  + (1 + fOnsetE*rLength|sub_id),data= alldata_clean,family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
summary(lmall_norLength) 
anova(lmall,lmall_norLength)

# Reduce 2 way
lmall_no2way = glmer(Shorter ~ Explabel*fOnsetE*rLength-Explabel:fOnsetE  + (1 + fOnsetE*rLength|sub_id),data= alldata_clean,family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
summary(lmall_no2way) 
anova(lmall,lmall_no2way)

# Reduce 3 way
lmall_no3way = glmer(Shorter ~ Explabel*fOnsetE*rLength-Explabel:fOnsetE:rLength  + (1 + fOnsetE*rLength|sub_id),data= alldata_clean,family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
summary(lmall_no3way) 
anova(lmall,lmall_no3way)

saveRDS(lmall_norLength, file = "EXP9_glmer_full_norLength.RData") 
saveRDS(lmall_no2way, file = "EXP9_glmer_full_no2way.RData") 
saveRDS(lmall_no3way, file = "EXP9_glmer_full_no3way.RData") 

# Submodels
lmall_speech = glmer(Shorter ~ fOnsetE*rLength  + (1 + fOnsetE*rLength|sub_id),data= filter(alldata_clean,exp=="EXP9a"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
lmall_tone = glmer(Shorter ~ fOnsetE*rLength  + (1 + fOnsetE*rLength|sub_id),data= filter(alldata_clean,exp=="EXP9b"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
lmall_speech_noOnset = glmer(Shorter ~ fOnsetE*rLength - fOnsetE  + (1 + fOnsetE*rLength|sub_id),data= filter(alldata_clean,exp=="EXP9a"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
lmall_tone_noOnset = glmer(Shorter ~ fOnsetE*rLength - fOnsetE + (1 + fOnsetE*rLength|sub_id),data= filter(alldata_clean,exp=="EXP9b"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
summary(lmall_speech) 
summary(lmall_tone) 
anova(lmall_speech,lmall_speech_noOnset)
anova(lmall_tone,lmall_tone_noOnset)

# Calculate effects based on the confidence interval wald test
confint(lmall_speech, level = 0.95, method = "Wald")
confint(lmall_tone, level = 0.95, method = "Wald")

## Implement ANOVA on proportion short: Onset Times x Auditory Targets
m = summary(aov(Shorter~fOnsetE*Explabel+Error(sub_id/fOnsetE),data=aovmeans_clean2)) 
# calculate partial generalized eta sq https://www.aggieerin.com/shiny-server/tests/gesmixss.html and based on Olejnik & Algina (2003)
m$'Error: sub_id'[[1]]$`Sum Sq`[1]/(m$'Error: sub_id'[[1]]$`Sum Sq`[1]+m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]) # Target
m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[1]/(m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[1]+m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]) # Onset
m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[2]/(m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[2]+m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]+m$'Error: sub_id'[[1]]$`Sum Sq`[1]) # Onset*Target

m = summary(aov(Shorter~fOnsetE+Error(sub_id/fOnsetE),data=filter(aovmeans_clean2,Explabel=="Speech"))) 
m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[1]/(sum(m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`)+m$'Error: sub_id'[[1]]$`Sum Sq`[1]) # calculate partial generalized eta sq
m = summary(aov(Shorter~fOnsetE+Error(sub_id/fOnsetE),data=filter(aovmeans_clean2,Explabel=="Tones"))) 
m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[1]/(sum(m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`)+m$'Error: sub_id'[[1]]$`Sum Sq`[1]) # calculate partial generalized eta sq

## Implement pairwise ttests between Onset Times (Early vs. Ontime, Early vs. Late, Ontime vs. Late) on each of the Auditory Targets (Speech, Tone)
# Speech
p = t.test(filter(aovmeans_clean2,Explabel=="Speech" & fOnsetE=="early")$Shorter,filter(aovmeans_clean2,Explabel=="Speech" & fOnsetE=="ontime")$Shorter,paired=T)
p
cohen.d(filter(aovmeans_clean2,Explabel=="Speech" & fOnsetE=="early")$Shorter,filter(aovmeans_clean2,Explabel=="Speech" & fOnsetE=="ontime")$Shorter,paired=T) # this is cohen'd rm
p.adjust(p[["p.value"]], method = "bonferroni", n = 3)

p = t.test(filter(aovmeans_clean2,Explabel=="Speech" & fOnsetE=="early")$Shorter,filter(aovmeans_clean2,Explabel=="Speech" & fOnsetE=="late")$Shorter,paired=T)
p
cohen.d(filter(aovmeans_clean2,Explabel=="Speech" & fOnsetE=="early")$Shorter,filter(aovmeans_clean2,Explabel=="Speech" & fOnsetE=="late")$Shorter,paired=T)
p.adjust(p[["p.value"]], method = "bonferroni", n = 3)

p = t.test(filter(aovmeans_clean2,Explabel=="Speech" & fOnsetE=="ontime")$Shorter,filter(aovmeans_clean2,Explabel=="Speech" & fOnsetE=="late")$Shorter,paired=T)
p
cohen.d(filter(aovmeans_clean2,Explabel=="Speech" & fOnsetE=="ontime")$Shorter,filter(aovmeans_clean2,Explabel=="Speech" & fOnsetE=="late")$Shorter,paired=T)
p.adjust(p[["p.value"]], method = "bonferroni", n = 3)

# Tone
p = t.test(filter(aovmeans_clean2,Explabel=="Tones" & fOnsetE=="early")$Shorter,filter(aovmeans_clean2,Explabel=="Tones" & fOnsetE=="ontime")$Shorter,paired=T)
p
cohen.d(filter(aovmeans_clean2,Explabel=="Tones" & fOnsetE=="early")$Shorter,filter(aovmeans_clean2,Explabel=="Tones" & fOnsetE=="ontime")$Shorter,paired=T)
p.adjust(p[["p.value"]], method = "bonferroni", n = 3)

p = t.test(filter(aovmeans_clean2,Explabel=="Tones" & fOnsetE=="early")$Shorter,filter(aovmeans_clean2,Explabel=="Tones" & fOnsetE=="late")$Shorter,paired=T)
p
cohen.d(filter(aovmeans_clean2,Explabel=="Tones" & fOnsetE=="early")$Shorter,filter(aovmeans_clean2,Explabel=="Tones" & fOnsetE=="late")$Shorter,paired=T)
p.adjust(p[["p.value"]], method = "bonferroni", n = 3)

p = t.test(filter(aovmeans_clean2,Explabel=="Tones" & fOnsetE=="ontime")$Shorter,filter(aovmeans_clean2,Explabel=="Tones" & fOnsetE=="late")$Shorter,paired=T)
p
cohen.d(filter(aovmeans_clean2,Explabel=="Tones" & fOnsetE=="ontime")$Shorter,filter(aovmeans_clean2,Explabel=="Tones" & fOnsetE=="late")$Shorter,paired=T)
p.adjust(p[["p.value"]], method = "bonferroni", n = 3)

## Implement ANOVA on 50% point: Onset Times x Auditory Targets
m = summary(aov(fifty~fOnsetE*Explabel+Error(sub_id/fOnsetE),data=aovmeans_clean2)) 
m$'Error: sub_id'[[1]]$`Sum Sq`[1]/(m$'Error: sub_id'[[1]]$`Sum Sq`[1]+m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]) # Target
m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[1]/(m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[1]+m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]) # Onset
m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[2]/(m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[2]+m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]+m$'Error: sub_id'[[1]]$`Sum Sq`[1]) # Onset*Target

m = summary(aov(fifty~fOnsetE+Error(sub_id/fOnsetE),data=filter(aovmeans_clean2,Explabel=="Speech"))) 
m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[1]/(sum(m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`)+m$'Error: sub_id'[[1]]$`Sum Sq`[1]) # calculate partial generalized eta sq
m = summary(aov(fifty~fOnsetE+Error(sub_id/fOnsetE),data=filter(aovmeans_clean2,Explabel=="Tones"))) 
m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[1]/(sum(m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`)+m$'Error: sub_id'[[1]]$`Sum Sq`[1]) # calculate partial generalized eta sq

## Implement pairwise ttests between Onset Times (Early vs. Ontime, Early vs. Late, Ontime vs. Late) on each of the Auditory Targets (Speech, Tone)
# Speech
p = t.test(filter(aovmeans_clean2,Explabel=="Speech" & fOnsetE=="early")$fifty,filter(aovmeans_clean2,Explabel=="Speech" & fOnsetE=="ontime")$fifty,paired=T)
p
cohen.d(filter(aovmeans_clean2,Explabel=="Speech" & fOnsetE=="early")$fifty,filter(aovmeans_clean2,Explabel=="Speech" & fOnsetE=="ontime")$fifty,paired=T)
p.adjust(p[["p.value"]], method = "bonferroni", n = 3)

p = t.test(filter(aovmeans_clean2,Explabel=="Speech" & fOnsetE=="early")$fifty,filter(aovmeans_clean2,Explabel=="Speech" & fOnsetE=="late")$fifty,paired=T)
p
cohen.d(filter(aovmeans_clean2,Explabel=="Speech" & fOnsetE=="early")$fifty,filter(aovmeans_clean2,Explabel=="Speech" & fOnsetE=="late")$fifty,paired=T)
p.adjust(p[["p.value"]], method = "bonferroni", n = 3)

p = t.test(filter(aovmeans_clean2,Explabel=="Speech" & fOnsetE=="ontime")$fifty,filter(aovmeans_clean2,Explabel=="Speech" & fOnsetE=="late")$fifty,paired=T)
p
cohen.d(filter(aovmeans_clean2,Explabel=="Speech" & fOnsetE=="ontime")$fifty,filter(aovmeans_clean2,Explabel=="Speech" & fOnsetE=="late")$fifty,paired=T)
p.adjust(p[["p.value"]], method = "bonferroni", n = 3)

# Tone
p = t.test(filter(aovmeans_clean2,Explabel=="Tones" & fOnsetE=="early")$fifty,filter(aovmeans_clean2,Explabel=="Tones" & fOnsetE=="ontime")$fifty,paired=T)
p
cohen.d(filter(aovmeans_clean2,Explabel=="Tones" & fOnsetE=="early")$fifty,filter(aovmeans_clean2,Explabel=="Tones" & fOnsetE=="ontime")$fifty,paired=T)
p.adjust(p[["p.value"]], method = "bonferroni", n = 3)

p = t.test(filter(aovmeans_clean2,Explabel=="Tones" & fOnsetE=="early")$fifty,filter(aovmeans_clean2,Explabel=="Tones" & fOnsetE=="late")$fifty,paired=T)
p
cohen.d(filter(aovmeans_clean2,Explabel=="Tones" & fOnsetE=="early")$fifty,filter(aovmeans_clean2,Explabel=="Tones" & fOnsetE=="late")$fifty,paired=T)
p.adjust(p[["p.value"]], method = "bonferroni", n = 3)

p = t.test(filter(aovmeans_clean2,Explabel=="Tones" & fOnsetE=="ontime")$fifty,filter(aovmeans_clean2,Explabel=="Tones" & fOnsetE=="late")$fifty,paired=T)
p
cohen.d(filter(aovmeans_clean2,Explabel=="Tones" & fOnsetE=="ontime")$fifty,filter(aovmeans_clean2,Explabel=="Tones" & fOnsetE=="late")$fifty,paired=T)
p.adjust(p[["p.value"]], method = "bonferroni", n = 3)