library(lme4)
library(lmerTest)
library(tidyverse)
library(broom)
library(ggplot2)
library(ggpubr)
library('fastDummies')
library(effsize)
library(lsr)

## Load the data
EXPNattone = read.csv("/Users/t.z.cheng/Documents/GitHub/Cross_domain_entrainment/Delaydoesmatter/real_exp/exp4_20CR12/4ab/results_shortdelay_2020/EXP4a_clean_n53.csv") 
EXPAmtone = read.csv("/Users/t.z.cheng/Documents/GitHub//cross_domain_entrainment/Delaydoesmatter/real_exp/exp4_20CR12/4c/results/EXP4c_clean_n71.csv") 

EXPNattone$Shorter=ifelse(EXPNattone$response_value=="Shorter",1,0)
EXPNattone$Correct=ifelse(EXPNattone$response_correct=='True',1,0)
EXPNattone$Onset = ifelse(EXPNattone$Onset==5,'early',ifelse(EXPNattone$Onset==8,'ontime','late'))
EXPNattone = EXPNattone %>%
  mutate(exp = 'EXP4a') # scale the steps ## 4c: scale Length; 6: scale comparison

alldata=rbind(select(EXPNattone,participant_id,exp,Onset,Length,Shorter,Correct),select(EXPAmtone,participant_id,exp,Onset,Length,Shorter,Correct))

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
  group_by(fOnsetE,OnsetNum,rLength,participant_id,Explabel) %>% summarise(Shorter=mean(Shorter)) # change rLength to Length for visualization

## Very slow!!!!!!!
# ggplot(aovdata,aes(x=rLength,y=Shorter,color=fOnsetR))+
#   scale_color_manual(values=c("red","blue","gray"))+
#   geom_point()+
#   # geom_line()+
#   #geom_smooth(method="lm",formula=y ~ exp(x)/(1+exp(x)),se=FALSE)+
#   geom_smooth(method="lm",se=FALSE) +
#   facet_wrap(participant_id~.)

# run logistic fit on each subject and condition
aovmeans=aovdata %>% 
  group_by(participant_id,fOnsetE,Explabel) %>% 
  do(glmfit = glm(Shorter ~ rLength,data =.,family=binomial())) 

# get the coefficients 
aovmeans = aovmeans %>%
  mutate(intercept = coef(glmfit)[1], slope = coef(glmfit)[2], fifty = -coef(glmfit)[1]/coef(glmfit)[2], deviance = glmfit$aic)

# mutate columes of pps
pps = aovdata %>% group_by(participant_id,fOnsetE,Explabel) %>% summarise(ShorterM=mean(Shorter),ShorterSD=sd(Shorter),Nsubs=n_distinct(participant_id),SE=ShorterSD/sqrt(Nsubs))
aovmeans = cbind(pps$ShorterM,aovmeans)
colnames(aovmeans)[1] = 'Shorter'

## flag outliers based on slope
# who have reverse slopes, flat lines
# 'af90a' press the same button across all experiment 
aovmeans$outliers_slope = ifelse(aovmeans$slope>= 0 | aovmeans$participant_id == 'af90a',1,0)
outliers_slope_subj = filter(aovmeans,outliers_slope==1)
aovmeans_clean1 = filter(aovmeans, !(participant_id %in% unique(outliers_slope_subj$participant_id)))

## flag outliers based on 50% point 
# plot the histogram
# hist(aovmeans_clean1$fifty,200)
# ggplot(aovmeans_clean,aes(x=fifty ,color=fOnsetR))+
#   geom_histogram()+
#   facet_grid(fOnsetR ~ Explabel)

# IQR criteria
q1 = quantile(aovmeans_clean1$fifty,.25)
q3 = quantile(aovmeans_clean1$fifty,.75)
iqr = IQR(aovmeans_clean1$fifty)

aovmeans_clean1$outliers_50 = ifelse(aovmeans_clean1$fifty < (q1 - 1.5*iqr) | aovmeans_clean1$fifty > (q3 + 1.5*iqr),1,0)
outliers_subj_50 = filter(aovmeans_clean1,outliers_50==1)
aovmeans_clean2 = filter(aovmeans_clean1, !(participant_id %in% unique(outliers_subj_50$participant_id)))
hist(aovmeans_clean2$fifty,200)

# Descriptive stats
aovmeans_clean2 %>%
  group_by(Explabel,fOnsetE) %>%
  summarize(mean(fifty),mean(intercept),mean(Shorter))

# plot overall results
aovdata_clean = filter(aovdata, participant_id %in% unique(aovmeans_clean2$participant_id)) 
aovdata_outlier_slope = filter(aovdata, participant_id %in% unique(outliers_slope_subj$participant_id)) 
aovdata_outlier_50 = filter(aovdata, participant_id %in% unique(outliers_subj_50$participant_id)) 
aovdata_clean$fOnsetE = factor(aovdata_clean$fOnsetE, levels = c("early","ontime","late"))
aovdata_outlier_slope$fOnsetE = factor(aovdata_outlier_slope$fOnsetE, levels = c("early","ontime","late"))
aovdata_outlier_50$fOnsetE = factor(aovdata_outlier_50$fOnsetE, levels = c("early","ontime","late"))

aovdata_clean_plot = aovdata_clean %>% group_by(rLength,fOnsetE,Explabel) %>% summarise(mShorter=mean(Shorter),SD=sd(Shorter),Nsubs=n_distinct(participant_id))
aovdata_clean_plot$fOnsetE = factor(aovdata_clean_plot$fOnsetE, levels = c("early","ontime","late"))
aovdata_clean$Explabel = factor(aovdata_clean$Explabel, levels = c("EXP9a","EXP9b"))
aovdata_outlier_slope$Explabel = factor(aovdata_outlier_slope$Explabel, levels = c("EXP9a","EXP9b"))
aovdata_outlier_50$Explabel = factor(aovdata_outlier_50$Explabel, levels = c("EXP9a","EXP9b"))
aovdata_clean_plot$Explabel = factor(aovdata_clean_plot$Explabel, levels = c("EXP9a","EXP9b"))

aovdata_clean_plot$Explabel = ifelse(aovdata_clean_plot$Explabel == "EXP4a","Nat Tone (N = 36)","Am Tone (N = 58)")

ggplot(aovdata_clean_plot,aes(x=rLength,y=mShorter,color=fOnsetE,linetype=Explabel,group=interaction(fOnsetE,Explabel)))+
  geom_point()+
  scale_x_continuous(breaks = seq(1, 8, by = 1))+
  geom_line()+
  geom_errorbar(aes(ymin=mShorter-SD/sqrt(Nsubs),ymax=mShorter+SD/sqrt(Nsubs)),width=0)+
  facet_grid(Explabel~.)

# Get the endpoint accuracy
# a = filter(aovdata_clean_plot,fOnsetR == "ontime" & Explabel == "EXP8a" & abs(rLength) > 1.52)
# c = filter(aovdata_clean_plot,fOnsetR == "ontime" & Explabel == "EXP8c" & abs(rLength) > 1.52)

# Very slow!!! individual plot for clean data
ggplot(aovdata_clean,aes(x=rLength,y=Shorter,color=fOnsetR))+
  scale_color_manual(values=c("red","green","blue"))+
  geom_point()+
  # geom_line()+
  #  geom_smooth(method="lm",formula=y ~ exp(x)/(1+exp(x)),se=FALSE)+
  #  geom_smooth(method="lm",se=FALSE) +
  geom_smooth(method="glm",method.args = list(family = "binomial"),se=FALSE) +
  facet_wrap(participant_id~.)

## relabel Speech to EXP8a and Tone to EXP8b and 
aovmeans_clean2$Explabel = ifelse(aovmeans_clean2$Explabel == "EXP4a","Nat Tone (N = 36)","Am Tone (N = 58)")
aovmeans_clean2$fOnsetR = factor(aovmeans_clean2$fOnsetE, levels = c("early","ontime","late"))

ggplot(aovmeans_clean2, aes(x = Explabel, y = fifty, fill = fOnsetR)) +
  geom_boxplot(outlier.size = 0) + 
  geom_point(position = position_jitterdodge(jitter.width = 0.1)) +
  labs(x = "Onset")
ggplot(aovmeans_clean2, aes(x = Explabel, y = Shorter, fill = fOnsetR)) +
  geom_boxplot(outlier.size = 0) + 
  geom_point(position = position_jitterdodge(jitter.width = 0.1)) +
  labs(x = "Onset")

## Final sample size
exp8a = filter(aovmeans_clean2,Explabel == "Nat Tone")
exp8b = filter(aovmeans_clean2,Explabel == "Am Tone")
length(unique(exp8a$participant_id))
length(unique(exp8b$participant_id))

## ANOVA on proportion short
m = summary(aov(Shorter~fOnsetE*Explabel+Error(participant_id/fOnsetE),data=aovmeans_clean2)) 
# calculate partial generalized eta sq https://www.aggieerin.com/shiny-server/tests/gesmixss.html and based on Olejnik & Algina (2003)
m$'Error: participant_id'[[1]]$`Sum Sq`[1]/(m$'Error: participant_id'[[1]]$`Sum Sq`[1]+m$'Error: participant_id:fOnsetR'[[1]]$`Sum Sq`[3]+m$'Error: participant_id'[[1]]$`Sum Sq`[2]) # Target
m$'Error: participant_id:fOnsetR'[[1]]$`Sum Sq`[1]/(m$'Error: participant_id:fOnsetR'[[1]]$`Sum Sq`[1]+m$'Error: participant_id:fOnsetR'[[1]]$`Sum Sq`[3]+m$'Error: participant_id'[[1]]$`Sum Sq`[2]) # Onset
m$'Error: participant_id:fOnsetR'[[1]]$`Sum Sq`[2]/(m$'Error: participant_id:fOnsetR'[[1]]$`Sum Sq`[2]+m$'Error: participant_id:fOnsetR'[[1]]$`Sum Sq`[3]+m$'Error: participant_id'[[1]]$`Sum Sq`[2]+m$'Error: participant_id'[[1]]$`Sum Sq`[1]) # Onset*Target

m = summary(aov(Shorter~fOnsetR+Error(participant_id/fOnsetR),data=filter(aovmeans_clean2,Explabel=="Nat Tone (N = 36)"))) 
m$'Error: participant_id:fOnsetR'[[1]]$`Sum Sq`[1]/(sum(m$'Error: participant_id:fOnsetR'[[1]]$`Sum Sq`)+m$'Error: participant_id'[[1]]$`Sum Sq`[1]) # calculate partial generalized eta sq
m = summary(aov(Shorter~fOnsetR+Error(participant_id/fOnsetR),data=filter(aovmeans_clean2,Explabel=="Am Tone (N = 58)"))) 
m$'Error: participant_id:fOnsetR'[[1]]$`Sum Sq`[1]/(sum(m$'Error: participant_id:fOnsetR'[[1]]$`Sum Sq`)+m$'Error: participant_id'[[1]]$`Sum Sq`[1]) # calculate partial generalized eta sq

## glmer on proportion short
alldata_clean = filter(alldata, participant_id %in% unique(aovmeans_clean2$participant_id)) 

# full
lmall = glmer(Shorter ~ Explabel*fOnsetE*rLength  + (1 + fOnsetE*rLength|participant_id),data= alldata_clean,family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
summary(lmall) # Use early as the reference

# reduce 2 way
lmall_no2way = glmer(Shorter ~ Explabel*fOnsetE*rLength-Explabel:fOnsetE  + (1 + fOnsetE*rLength|participant_id),data= alldata_clean,family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
summary(lmall_no2way) # Use early as the reference
anova(lmall,lmall_no2way)

# reduce 3 way
lmall_no3way = glmer(Shorter ~ Explabel*fOnsetE*rLength-Explabel:fOnsetE:rLength  + (1 + fOnsetE*rLength|participant_id),data= alldata_clean,family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
summary(lmall_no3way) # Use early as the reference
anova(lmall,lmall_no3way)

lmall_NatTone = glmer(Shorter ~ fOnsetE*rLength  + (1 + fOnsetE*rLength|participant_id),data= filter(alldata_clean,exp=="EXP4a"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
lmall_AmTone = glmer(Shorter ~ fOnsetE*rLength  + (1 + fOnsetE*rLength|participant_id),data= filter(alldata_clean,exp=="EXP4c"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
lmall_NatTone_noOnset = glmer(Shorter ~ fOnsetE*rLength - fOnsetE  + (1 + fOnsetE*rLength|participant_id),data= filter(alldata_clean,exp=="EXP4a"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
lmall_AmTone_noOnset = glmer(Shorter ~ fOnsetE*rLength - fOnsetE + (1 + fOnsetE*rLength|participant_id),data= filter(alldata_clean,exp=="EXP4c"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
summary(lmall_NatTone) # Use early as the reference
summary(lmall_AmTone) # Use early as the reference
anova(lmall_NatTone,lmall_NatTone_noOnset)
anova(lmall_AmTone,lmall_AmTone_noOnset)