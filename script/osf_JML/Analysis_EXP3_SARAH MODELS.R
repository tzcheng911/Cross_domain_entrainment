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
EXPspeech = read.csv("EXP3a_clean_n67.csv") 
EXPtone = read.csv("EXP3b_clean_n67.csv") 

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
  group_by(fOnsetE,Explabel,fOnset,Length,sub_id) %>% summarise(Shorter=mean(Shorter)) # change rLength to Length for visualization

## Run logistic fit on each subject and condition
aovmeans=aovdata %>% 
  group_by(sub_id,fOnsetE,Explabel) %>% 
  do(glmfit = glm(Shorter ~ Length,data =.,family=binomial())) # change rLength to Length for visualization

## Get the coefficients 
aovmeans = aovmeans %>%
  mutate(intercept = coef(glmfit)[1], slope = coef(glmfit)[2], fifty = -coef(glmfit)[1]/coef(glmfit)[2], deviance = glmfit$aic)

## Mutate columes of pps
pps = aovdata %>% group_by(sub_id,fOnsetE,Explabel) %>% summarise(ShorterM=mean(Shorter),ShorterSD=sd(Shorter),Nsubs=n_distinct(sub_id),SE=ShorterSD/sqrt(Nsubs))
aovmeans = cbind(pps$ShorterM,aovmeans)
colnames(aovmeans)[1] = 'Shorter'

## Flag outliers based on slope
# who have reverse slopes, flat lines
aovmeans$outliers_slope = ifelse(aovmeans$slope>= 0 | aovmeans$sub_id == '03597' | aovmeans$sub_id == '59a4e' | 
                                   aovmeans$sub_id == '7369f' | aovmeans$sub_id == '82952'| 
                                   aovmeans$sub_id == '83e6c' | aovmeans$sub_id == '906af',1,0) 
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
aovdata_clean_plot = aovdata_clean %>% group_by(Length,fOnsetE,Explabel) %>% summarise(mShorter=mean(Shorter),SD=sd(Shorter),Nsubs=n_distinct(sub_id)) # change rLength to Length for visualization
aovdata_clean_plot$fOnsetE = factor(aovdata_clean_plot$fOnsetE, levels = c("early","ontime","late"))
aovdata_clean$Explabel = factor(aovdata_clean$Explabel, levels = c("EXP9a","EXP9b"))
aovdata_outlier_slope$Explabel = factor(aovdata_outlier_slope$Explabel, levels = c("EXP9a","EXP9b"))
aovdata_outlier_50$Explabel = factor(aovdata_outlier_50$Explabel, levels = c("EXP9a","EXP9b"))
aovdata_clean_plot$Explabel = factor(aovdata_clean_plot$Explabel, levels = c("EXP9a","EXP9b"))

ggplot(aovdata_clean_plot,aes(x=Length,y=mShorter,color=fOnsetE,linetype=Explabel,group=interaction(fOnsetE,Explabel)))+
  geom_point()+
  scale_x_continuous(breaks = seq(1, 8, by = 1))+
  geom_line()+
  geom_errorbar(aes(ymin=mShorter-SD/sqrt(Nsubs),ymax=mShorter+SD/sqrt(Nsubs)),width=0)+
  facet_grid(Explabel~.)+
  theme_bw()

                    # # Plot individual curves of the clean data
                    # ggplot(aovdata_clean,aes(x=Length,y=Shorter,color=fOnsetE,shape=Explabel))+
                    #   scale_color_manual(values=c("red","green","blue"))+
                    #   geom_point()+
                    #   # geom_line()+
                    #   # geom_smooth(method="lm",formula=y ~ exp(x)/(1+exp(x)),se=FALSE)+
                    #   # geom_smooth(method="lm",se=FALSE) +
                    #   geom_smooth(method="glm",method.args = list(family = "binomial"),se=FALSE) +
                    #   facet_wrap(sub_id~.) +
                    #   theme(strip.text.x = element_blank())

## Relabel Exp10ab to Speech and Tone
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
alldata_clean$Explabel = ifelse(alldata_clean$Explabel=="EXP10a",-0.5,0.5) # sum coding for the two conditions being compared. 
# ???? HOW DOES EXP10a above work if labeled in file as 9a/9b ????

alldata_clean_exp3 = alldata_clean %>%
  mutate(earlyVSontime=ifelse(fOnsetE=="ontime",2/3,-1/3),
         earlyVSlate=ifelse(fOnsetE=="late",2/3,-1/3), # note that early is always -1/3, so it's reference level
         speechVSothers=ifelse(exp=="EXP9a",-.5,.5)) # what are SUPPOSED to be 10a, 10b are labeled as 9a (speech), 9b (tone)
# check the recoding
alldata_clean_exp3 %>% group_by(exp,speechVSothers,
                                earlyVSontime,earlyVSlate) %>% summarise(n())


# Basically: 
# no Target Type x Onset interaction (!)
# there is a barely significant 3-way which is due to a marginal 2-way for speech, no 2-way for tones
# both tones and speech show entrainment


lm_Sarah = glmer(Shorter ~ speechVSothers*(earlyVSontime+earlyVSlate)*rLength  + 
                   (1 + earlyVSlate+rLength|sub_id),
                 data= alldata_clean_exp3,family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
summary(lm_Sarah)
# Fixed effects:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                          -0.3726534  0.0593084  -6.283 3.31e-10 ***
# speechVSothers                       -0.2505510  0.1184146  -2.116  0.03436 *  
# earlyVSontime                        -0.0918678  0.0353653  -2.598  0.00939 ** 
# earlyVSlate                          -0.2122422  0.0388328  -5.466 4.61e-08 ***
# rLength                              -1.6529015  0.0741795 -22.282  < 2e-16 ***
# speechVSothers:earlyVSontime         -0.0144084  0.0707061  -0.204  0.83853    
# speechVSothers:earlyVSlate           -0.0489354  0.0757465  -0.646  0.51825    
# speechVSothers:rLength               -0.3715770  0.1477707  -2.515  0.01192 *  
# earlyVSontime:rLength                -0.0006092  0.0411869  -0.015  0.98820    
# earlyVSlate:rLength                   0.0400805  0.0418761   0.957  0.33851    
# speechVSothers:earlyVSontime:rLength -0.1091633  0.0823476  -1.326  0.18496    
# speechVSothers:earlyVSlate:rLength   -0.2043001  0.0821316  -2.487  0.01287 *  

# Target Type x Onset
lm_Sarah_nointeraction = glmer(Shorter ~ speechVSothers*(earlyVSontime+earlyVSlate)*rLength  -
                                 speechVSothers:(earlyVSontime+earlyVSlate) + # i.e., the targetType x delay interaction 
                                 (1 + earlyVSlate + rLength | sub_id),
                               data= alldata_clean_exp3,family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
anova(lm_Sarah,lm_Sarah_nointeraction) 
# lm_Sarah_nointeraction   16 29951 30084 -14960    29919                     
# lm_Sarah                 18 29954 30104 -14959    29918 0.4244  2     0.8088

# Target Type x Onset x Length
lm_Sarah_no3wayinteraction = glmer(Shorter ~ speechVSothers*(earlyVSontime+earlyVSlate)*rLength  -
                                 speechVSothers:(earlyVSontime+earlyVSlate):rLength + # 3 way interaction 
                                 (1 + earlyVSlate + rLength | sub_id),
                               data= alldata_clean_exp3,family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
anova(lm_Sarah,lm_Sarah_no3wayinteraction) 
# lm_Sarah_no3wayinteraction   16 29957 30090 -14962    29925                       
# lm_Sarah                     18 29954 30104 -14959    29918 6.1224  2    0.04683 *


lmSp = glmer(Shorter ~ (earlyVSontime+earlyVSlate)*rLength  + 
               (1 + earlyVSlate + rLength|sub_id),
             data= filter(alldata_clean_exp3,exp=="EXP9a"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
summary(lmSp)
# earlyVSontime         -0.08450    0.05004  -1.689 0.091296 .  
# earlyVSlate           -0.20095    0.05660  -3.551 0.000384 ***
# rLength               -1.46548    0.10391 -14.103  < 2e-16 ***
# earlyVSontime:rLength  0.05398    0.05700   0.947 0.343629    
# earlyVSlate:rLength    0.12975    0.05755   2.255 0.024154 *  
lmSpNoOnset = glmer(Shorter ~ (earlyVSontime+earlyVSlate)*rLength  -
                      (earlyVSontime+earlyVSlate) + 
                      (1 + earlyVSlate + rLength|sub_id),
                    data= filter(alldata_clean_exp3,exp=="EXP9a"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
anova(lmSp,lmSpNoOnset)
# lmSpNoOnset   10 14799 14874 -7389.3    14779                        
# lmSp          12 14790 14881 -7383.1    14766 12.442  2   0.001987 **

lmSpNo2way = glmer(Shorter ~ (earlyVSontime+earlyVSlate)*rLength  -
                      (earlyVSontime+earlyVSlate):rLength + 
                      (1 + earlyVSlate + rLength|sub_id),
                    data= filter(alldata_clean_exp3,exp=="EXP9a"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
anova(lmSp,lmSpNo2way)
# lmSpNo2way   10 14791 14867 -7385.6    14771                       
# lmSp         12 14790 14881 -7383.1    14766 5.0659  2    0.07943 .



lmTn = glmer(Shorter ~ (earlyVSontime+earlyVSlate)*rLength  + 
               (1 + earlyVSlate + rLength|sub_id),
             data= filter(alldata_clean_exp3,exp=="EXP9b"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
summary(lmTn)
# earlyVSontime         -0.09926    0.04998  -1.986   0.0471 *  
# earlyVSlate           -0.22172    0.05403  -4.104 4.07e-05 ***
# rLength               -1.84039    0.10495 -17.536  < 2e-16 ***
# earlyVSontime:rLength -0.05524    0.05948  -0.929   0.3530    
# earlyVSlate:rLength   -0.04826    0.06086  -0.793   0.4278    

lmTnNoOnset = glmer(Shorter ~ (earlyVSontime+earlyVSlate)*rLength  -
                      (earlyVSontime+earlyVSlate) + 
                      (1 + earlyVSlate + rLength|sub_id),
                    data= filter(alldata_clean_exp3,exp=="EXP9b"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
anova(lmTn,lmTnNoOnset)
# lmTnNoOnset   10 15183 15260 -7581.3    15163                         
# lmTn          12 15171 15263 -7573.3    15147 15.978  2  0.0003391 ***




########### END OF SARAH GLMER SECTION ###########
########### END OF SARAH GLMER SECTION ###########
########### END OF SARAH GLMER SECTION ###########
########### END OF SARAH GLMER SECTION ###########
########### END OF SARAH GLMER SECTION ###########









# Full model
lmall = glmer(Shorter ~ Explabel*fOnsetE*rLength  + (1 + fOnsetE*rLength|sub_id),data= alldata_clean,family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
summary(lmall) # Use early as the reference
emmeans(lmall, pairwise ~ fOnsetE | Explabel)

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
