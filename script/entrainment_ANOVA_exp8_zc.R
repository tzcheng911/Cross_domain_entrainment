library(lme4)
library(lmerTest)
library(tidyverse)
library(broom)
library(ggplot2)
library(ggpubr)
library('fastDummies')
library(effsize)
library(lsr)

## sc updated 03/20/2023
## Load the data
EXPtone = read.csv("/Users/t.z.cheng/Documents/GitHub/Cross_domain_entrainment/exp8/results/old/EXP8b_clean_n84.csv") 
EXPspeech = read.csv("/Users/t.z.cheng/Documents/GitHub/Cross_domain_entrainment/exp8/results/old/EXP8a_clean_n80.csv")
EXPtoneasspeech = read.csv("/Users/t.z.cheng/Documents/GitHub/Cross_domain_entrainment/exp8/results/old/EXP8c_clean_n88.csv") 
EXPtoneasspeech = read.csv("/Users/t.z.cheng/Documents/GitHub/Cross_domain_entrainment/exp8/results/old/EXP8c-s_clean_n20.csv") 
EXPtoneasspeech = read.csv("/Users/t.z.cheng/Documents/GitHub/Cross_domain_entrainment/exp8/results/EXP8c-s_clean_n34.csv") 
EXPspeech_pre = read.csv("/Users/t.z.cheng/Documents/GitHub/Cross_domain_entrainment/exp8/results/old/EXP8a_prescreen_clean_n80.csv")
EXPspeech_disc = read.csv("/Users/t.z.cheng/Documents/GitHub/Cross_domain_entrainment/exp8/results/old/EXP8a_discrimination_clean_n80.csv")

## EXP8: 3 conditions
alldata=rbind(select(EXPtone,participant_id,sub_id,exp,Onset,Length,Shorter,Correct),select(EXPspeech,participant_id,sub_id,exp,Onset,Length,Shorter,Correct),
              select(EXPtoneasspeech,participant_id,sub_id,exp,Onset,Length,Shorter,Correct))

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
  group_by(fOnsetR,Explabel,OnsetNum,rLength,sub_id) %>% summarise(Shorter=mean(Shorter)) # change rLength to Length for visualization

## Very slow!!!!!!!
# ggplot(aovdata,aes(x=rLength,y=Shorter,color=fOnsetR))+
#   scale_color_manual(values=c("red","blue","gray"))+
#   geom_point()+
#   # geom_line()+
#   #geom_smooth(method="lm",formula=y ~ exp(x)/(1+exp(x)),se=FALSE)+
#   geom_smooth(method="lm",se=FALSE) +
#   facet_wrap(sub_id~.)

# run logistic fit on each subject and condition
aovmeans=aovdata %>% 
  group_by(sub_id,fOnsetR,Explabel) %>% 
  do(glmfit = glm(Shorter ~ rLength,data =.,family=binomial())) # change rLength to Length for visualization

# get the coefficients 
aovmeans = aovmeans %>%
  mutate(intercept = coef(glmfit)[1], slope = coef(glmfit)[2], fifty = -coef(glmfit)[1]/coef(glmfit)[2], deviance = glmfit$aic)

# mutate columes of pps
pps = aovdata %>% group_by(sub_id,fOnsetR,Explabel) %>% summarise(ShorterM=mean(Shorter),ShorterSD=sd(Shorter),Nsubs=n_distinct(sub_id),SE=ShorterSD/sqrt(Nsubs))
aovmeans = cbind(pps$ShorterM,aovmeans)
colnames(aovmeans)[1] = 'Shorter'

## EXP9 flag outliers based on slope
# who have reverse slopes, flat lines
# '8db1d','074c2' press the same button across all experiment 
aovmeans$outliers_slope = ifelse(aovmeans$slope>= 0 | aovmeans$sub_id == '8db1d' | aovmeans$sub_id == '074c2',1,0)
outliers_slope_subj = filter(aovmeans,outliers_slope==1)
aovmeans_clean1 = filter(aovmeans, !(sub_id %in% unique(outliers_slope_subj$sub_id)))

## EXP8 flag outliers based on slope
# who have reverse slopes, flat lines
# 'af90a' press the same button across all experiment 
aovmeans$outliers_slope = ifelse(aovmeans$slope>= 0 | aovmeans$sub_id == 'af90a',1,0)
outliers_slope_subj = filter(aovmeans,outliers_slope==1)
aovmeans_clean1 = filter(aovmeans, !(sub_id %in% unique(outliers_slope_subj$sub_id)))

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
aovmeans_clean2 = filter(aovmeans_clean1, !(sub_id %in% unique(outliers_subj_50$sub_id)))
hist(aovmeans_clean2$fifty,200)

# Descriptive stats
aovmeans_clean2 %>%
  group_by(Explabel,fOnsetR) %>%
  summarize(mean(fifty),mean(intercept),mean(Shorter))
summary_aovmeans_clean2 = aovmeans_clean2 %>%
  group_by(Explabel,fOnsetR) %>%
  summarize(mfifty = mean(fifty), mShorter = mean(Shorter), Nsubs=n_distinct(sub_id), sefifty = sd(fifty)/sqrt(Nsubs), seShorter = sd(Shorter)/sqrt(Nsubs),sdShorter = sd(Shorter))

# plot overall results
aovdata_clean = filter(aovdata, sub_id %in% unique(aovmeans_clean2$sub_id)) 
aovdata_outlier_slope = filter(aovdata, sub_id %in% unique(outliers_slope_subj$sub_id)) 
aovdata_outlier_50 = filter(aovdata, sub_id %in% unique(outliers_subj_50$sub_id)) 
aovdata_clean$fOnsetR = factor(aovdata_clean$fOnsetR, levels = c("early","ontime","late"))
aovdata_outlier_slope$fOnsetR = factor(aovdata_outlier_slope$fOnsetR, levels = c("early","ontime","late"))
aovdata_outlier_50$fOnsetR = factor(aovdata_outlier_50$fOnsetR, levels = c("early","ontime","late"))

## EXP8
aovdata_clean$Explabel = factor(aovdata_clean$Explabel, levels = c("EXP8a","EXP8b","EXP8c"))
aovdata_outlier_slope$Explabel = factor(aovdata_outlier_slope$Explabel, levels = c("EXP8a","EXP8b","EXP8c"))
aovdata_outlier_50$Explabel = factor(aovdata_outlier_50$Explabel, levels = c("EXP8a","EXP8b","EXP8c"))
aovdata_clean_plot$Explabel = factor(aovdata_clean_plot$Explabel, levels = c("EXP8a","EXP8b","EXP8c"))

aovdata_clean_plot = aovdata_clean %>% group_by(rLength,fOnsetR,Explabel) %>% summarise(mShorter=mean(Shorter),SD=sd(Shorter),Nsubs=n_distinct(sub_id))
aovdata_clean_plot$fOnsetR = factor(aovdata_clean_plot$fOnsetR, levels = c("early","ontime","late"))

testSD = aovdata_clean_plot %>% group_by(fOnsetE,Explabel) %>% summarise(mSD=mean(SD))

ggplot(aovdata_clean_plot,aes(x=rLength,y=mShorter,color=fOnsetR,linetype=Explabel,group=interaction(fOnsetR,Explabel)))+
  geom_point()+
  scale_x_continuous(breaks = seq(1, 8, by = 1))+
  geom_line()+
  geom_errorbar(aes(ymin=mShorter-SD/sqrt(Nsubs),ymax=mShorter+SD/sqrt(Nsubs)),width=0)+
  facet_grid(Explabel~.)+
  theme_bw()

# Get the endpoint accuracy
# a = filter(aovdata_clean_plot,fOnsetR == "ontime" & Explabel == "EXP8a" & abs(rLength) > 1.52)
# c = filter(aovdata_clean_plot,fOnsetR == "ontime" & Explabel == "EXP8c" & abs(rLength) > 1.52)

# Very slow!!! individual plot for clean data
# ggplot(aovdata_clean,aes(x=rLength,y=Shorter,color=fOnsetR,shape=Explabel))+
#   scale_color_manual(values=c("red","green","blue"))+
#   geom_point()+
#   # geom_line()+
#   #  geom_smooth(method="lm",formula=y ~ exp(x)/(1+exp(x)),se=FALSE)+
#    geom_smooth(method="lm",se=FALSE) +
#   # geom_smooth(method="glm",method.args = list(family = "binomial"),se=FALSE) +
#   facet_wrap(sub_id~.) +
#   theme(strip.text.x = element_blank())

## EXP8 relabel
aovmeans_clean2$Explabel = ifelse(aovmeans_clean2$Explabel == "EXP8a","Speech",ifelse(aovmeans_clean2$Explabel == "EXP8b","Tones","ToneasSpeech"))
aovmeans_clean2$Explabel = factor(aovmeans_clean2$Explabel, levels = c("Speech","Tones","ToneasSpeech"))
aovmeans_clean2$fOnsetR = factor(aovmeans_clean2$fOnsetR, levels = c("early","ontime","late"))

## need to change the rlength to length so the 50% point is more interpretable
## box plot
ggplot(aovmeans_clean2, aes(x = Explabel, y = Shorter, fill = fOnsetR)) +
  geom_boxplot(outlier.size = 0) + 
  ylim(0,0.8) +
  geom_point(position = position_jitterdodge(jitter.width = 0.1)) +
  labs(x = "Onset")+
  theme_bw()
ggplot(aovmeans_clean2, aes(x = Explabel, y = fifty, fill = fOnsetR)) +
  geom_boxplot(outlier.size = 0) +
  geom_point(position = position_jitterdodge(jitter.width = 0.1)) +
  labs(x = "Onset")+
  theme_bw()

## bar plot
ggplot(aovmeans_clean2, aes(x = Explabel, y = Shorter, fill = fOnsetR)) +
  geom_bar(stat="summary", fun.y = "mean", position='dodge') +
  stat_summary(fun.data=mean_se, geom="errorbar", position = position_dodge(width = 0.9), width=.1,color="grey") +
  ylim(0,0.8) + 
  geom_point(position = position_jitterdodge(jitter.width = 0.3,dodge.width = 0.9), color="black")+
  theme_bw()

ggplot(aovmeans_clean2, aes(x = Explabel, y = fifty, fill = fOnsetR)) +
  geom_bar(stat="summary", fun.y = "mean", position='dodge') +
  stat_summary(fun.data=mean_se, geom="errorbar", position = position_dodge(width = 0.9), width=.1,color="grey") +
  ylim(0,8) + 
  geom_point(position = position_jitterdodge(jitter.width = 0.3,dodge.width = 0.9), color="black")+
  theme_bw()

## EXP8 Final sample size
exp8a = filter(aovmeans_clean2,Explabel == "Speech")
exp8b = filter(aovmeans_clean2,Explabel == "Tones")
exp8c = filter(aovmeans_clean2,Explabel == "ToneasSpeech")
length(unique(exp8a$sub_id))
length(unique(exp8b$sub_id))
length(unique(exp8c$sub_id))

## Descriptive stats of pretests
# exclude outliers as identified from the main task 
outliers1 = filter(outliers_subj_50,Explabel == "EXP8a")
outliers2 = filter(outliers_slope_subj,Explabel == "EXP8a")
outliers = rbind(select(outliers1,sub_id),select(outliers2,sub_id))

EXPspeech_pre = filter(EXPspeech_pre, !(sub_id %in% unique(outliers$sub_id)))
EXPspeech_disc = filter(EXPspeech_disc, !(sub_id %in% unique(outliers$sub_id)))

# rescale the Length
EXPspeech_pre = EXPspeech_pre %>%
  mutate(rLength = scale(Length, center = TRUE, scale = TRUE)) # scale the steps ## 4c: scale Length; 6: scale comparison
EXPspeech_disc = EXPspeech_disc %>%
  mutate(rLength = scale(Length, center = TRUE, scale = TRUE)) # scale the steps ## 4c: scale Length; 6: scale comparison
EXPspeech_pre$rLength=as.factor(EXPspeech_pre$rLength)
EXPspeech_disc$rLength=as.factor(EXPspeech_disc$rLength)

# average across trials 
prescreen = EXPspeech_pre%>%
  group_by(sub_id,rLength) %>%
  summarize(ShorterM = mean(Shorter))

discrimination = EXPspeech_disc%>%
  group_by(sub_id,rLength) %>%
  summarize(ShorterM = mean(Shorter))

discriminationM = discrimination%>%
  group_by(rLength) %>%
  summarize(ShorterM = mean(ShorterM))

ggplot(discrimination, aes(x = Length, y = ShorterM)) +
  geom_point(position = 'jitter') +
  geom_bar(data = discriminationM, stat = "identity", alpha = .3)

ggplot(discrimination, aes(x = rLength, y = ShorterM, fill=rLength)) +
  geom_violin()

## ANOVA on pretest discrimination lap/lab continuum
m = summary(aov(ShorterM~rLength+Error(sub_id/rLength),data=discrimination)) 
m$'Error: sub_id:rLength'[[1]]$`Sum Sq`[1]/(m$'Error: sub_id:rLength'[[1]]$`Sum Sq`[1]+m$'Error: sub_id:rLength'[[1]]$`Sum Sq`[2])

## ttest on pretest of prescreen 6 endpoints pair
p = t.test(filter(prescreen,Length == 1)$ShorterM,filter(prescreen,Length == 8)$ShorterM,paired=T)
p 
cohen.d(filter(prescreen,Length == 1)$ShorterM,filter(prescreen,Length == 8)$ShorterM,paired=T)
p.adjust(p[["p.value"]], method = "bonferroni", n = 3)

## ANOVA on Length x Onset in Speech condition
# EXP8
aovdata_clean$rLength = factor(aovdata_clean$rLength)
m = summary(aov(Shorter~fOnsetR*rLength+Error(sub_id/(fOnsetR*rLength)),data = filter(aovdata_clean,Explabel=="EXP8a")))
m$'Error: sub_id:rLength'[[1]]$`Sum Sq`[1]/(m$'Error: sub_id:rLength'[[1]]$`Sum Sq`[1]+m$'Error: sub_id:rLength'[[1]]$`Sum Sq`[2]) # rLength

## ANOVA on proportion short
m = summary(aov(Shorter~fOnsetR*Explabel+Error(sub_id/fOnsetR),data=aovmeans_clean2)) 
# calculate partial generalized eta sq https://www.aggieerin.com/shiny-server/tests/gesmixss.html and based on Olejnik & Algina (2003)
m$'Error: sub_id'[[1]]$`Sum Sq`[1]/(m$'Error: sub_id'[[1]]$`Sum Sq`[1]+m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]) # Target
m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[1]/(m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[1]+m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]) # Onset
m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[2]/(m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[2]+m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]+m$'Error: sub_id'[[1]]$`Sum Sq`[1]) # Onset*Target

m = summary(aov(Shorter~fOnsetR*Explabel+Error(sub_id/fOnsetR),data=filter(aovmeans_clean2,Explabel!="ToneasSpeech"))) 
# calculate partial generalized eta sq https://www.aggieerin.com/shiny-server/tests/gesmixss.html and based on Olejnik & Algina (2003)
m$'Error: sub_id'[[1]]$`Sum Sq`[1]/(m$'Error: sub_id'[[1]]$`Sum Sq`[1]+m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]) # Target
m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[1]/(m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[1]+m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]) # Onset
m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[2]/(m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[2]+m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]+m$'Error: sub_id'[[1]]$`Sum Sq`[1]) # Onset*Target

m = summary(aov(Shorter~fOnsetR*Explabel+Error(sub_id/fOnsetR),data=filter(aovmeans_clean2,Explabel!="Tones")))
# calculate partial generalized eta sq https://www.aggieerin.com/shiny-server/tests/gesmixss.html and based on Olejnik & Algina (2003)
m$'Error: sub_id'[[1]]$`Sum Sq`[1]/(m$'Error: sub_id'[[1]]$`Sum Sq`[1]+m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]) # Target
m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[1]/(m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[1]+m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]) # Onset
m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[2]/(m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[2]+m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]+m$'Error: sub_id'[[1]]$`Sum Sq`[1]) # Onset*Target

m = summary(aov(Shorter~fOnsetR*Explabel+Error(sub_id/fOnsetR),data=filter(aovmeans_clean2,Explabel!="Speech"))) 
# calculate partial generalized eta sq https://www.aggieerin.com/shiny-server/tests/gesmixss.html and based on Olejnik & Algina (2003)
m$'Error: sub_id'[[1]]$`Sum Sq`[1]/(m$'Error: sub_id'[[1]]$`Sum Sq`[1]+m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]) # Target
m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[1]/(m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[1]+m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]) # Onset
m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[2]/(m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[2]+m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]+m$'Error: sub_id'[[1]]$`Sum Sq`[1]) # Onset*Target

m = summary(aov(Shorter~fOnsetR+Error(sub_id/fOnsetR),data=filter(aovmeans_clean2,Explabel=="Speech"))) 
m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[1]/(sum(m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`)+m$'Error: sub_id'[[1]]$`Sum Sq`[1]) # calculate partial generalized eta sq
m = summary(aov(Shorter~fOnsetR+Error(sub_id/fOnsetR),data=filter(aovmeans_clean2,Explabel=="Tones"))) 
m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[1]/(sum(m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`)+m$'Error: sub_id'[[1]]$`Sum Sq`[1]) # calculate partial generalized eta sq
m = summary(aov(Shorter~fOnsetR+Error(sub_id/fOnsetR),data=filter(aovmeans_clean2,Explabel=="ToneasSpeech"))) 
m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[1]/(sum(m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`)+m$'Error: sub_id'[[1]]$`Sum Sq`[1]) # calculate partial generalized eta sq

# Speech
p = t.test(filter(aovmeans_clean2,Explabel=="Speech" & fOnsetR=="early")$Shorter,filter(aovmeans_clean2,Explabel=="Speech" & fOnsetR=="ontime")$Shorter,paired=T)
p
cohen.d(filter(aovmeans_clean2,Explabel=="Speech" & fOnsetR=="early")$Shorter,filter(aovmeans_clean2,Explabel=="Speech" & fOnsetR=="ontime")$Shorter,paired=T)
p.adjust(p[["p.value"]], method = "bonferroni", n = 3)

p = t.test(filter(aovmeans_clean2,Explabel=="Speech" & fOnsetR=="early")$Shorter,filter(aovmeans_clean2,Explabel=="Speech" & fOnsetR=="late")$Shorter,paired=T)
p
cohen.d(filter(aovmeans_clean2,Explabel=="Speech" & fOnsetR=="early")$Shorter,filter(aovmeans_clean2,Explabel=="Speech" & fOnsetR=="late")$Shorter,paired=T)
p.adjust(p[["p.value"]], method = "bonferroni", n = 3)

p = t.test(filter(aovmeans_clean2,Explabel=="Speech" & fOnsetR=="ontime")$Shorter,filter(aovmeans_clean2,Explabel=="Speech" & fOnsetR=="late")$Shorter,paired=T)
p
cohen.d(filter(aovmeans_clean2,Explabel=="Speech" & fOnsetR=="ontime")$Shorter,filter(aovmeans_clean2,Explabel=="Speech" & fOnsetR=="late")$Shorter,paired=T)
p.adjust(p[["p.value"]], method = "bonferroni", n = 3)

# Tone
p = t.test(filter(aovmeans_clean2,Explabel=="Tones" & fOnsetR=="early")$Shorter,filter(aovmeans_clean2,Explabel=="Tones" & fOnsetR=="ontime")$Shorter,paired=T)
p
cohen.d(filter(aovmeans_clean2,Explabel=="Tones" & fOnsetR=="early")$Shorter,filter(aovmeans_clean2,Explabel=="Tones" & fOnsetR=="ontime")$Shorter,paired=T)
p.adjust(p[["p.value"]], method = "bonferroni", n = 3)

p = t.test(filter(aovmeans_clean2,Explabel=="Tones" & fOnsetR=="early")$Shorter,filter(aovmeans_clean2,Explabel=="Tones" & fOnsetR=="late")$Shorter,paired=T)
p
cohen.d(filter(aovmeans_clean2,Explabel=="Tones" & fOnsetR=="early")$Shorter,filter(aovmeans_clean2,Explabel=="Tones" & fOnsetR=="late")$Shorter,paired=T)
p.adjust(p[["p.value"]], method = "bonferroni", n = 3)

p = t.test(filter(aovmeans_clean2,Explabel=="Tones" & fOnsetR=="ontime")$Shorter,filter(aovmeans_clean2,Explabel=="Tones" & fOnsetR=="late")$Shorter,paired=T)
p
cohen.d(filter(aovmeans_clean2,Explabel=="Tones" & fOnsetR=="ontime")$Shorter,filter(aovmeans_clean2,Explabel=="Tones" & fOnsetR=="late")$Shorter,paired=T)
p.adjust(p[["p.value"]], method = "bonferroni", n = 3)

# ToneasSpeech
p = t.test(filter(aovmeans_clean2,Explabel=="ToneasSpeech" & fOnsetR=="early")$Shorter,filter(aovmeans_clean2,Explabel=="ToneasSpeech" & fOnsetR=="ontime")$Shorter,paired=T)
p
cohen.d(filter(aovmeans_clean2,Explabel=="ToneasSpeech" & fOnsetR=="early")$Shorter,filter(aovmeans_clean2,Explabel=="ToneasSpeech" & fOnsetR=="ontime")$Shorter,paired=T)
p.adjust(p[["p.value"]], method = "bonferroni", n = 3)

p = t.test(filter(aovmeans_clean2,Explabel=="ToneasSpeech" & fOnsetR=="early")$Shorter,filter(aovmeans_clean2,Explabel=="ToneasSpeech" & fOnsetR=="late")$Shorter,paired=T)
p
cohen.d(filter(aovmeans_clean2,Explabel=="ToneasSpeech" & fOnsetR=="early")$Shorter,filter(aovmeans_clean2,Explabel=="ToneasSpeech" & fOnsetR=="late")$Shorter,paired=T)
p.adjust(p[["p.value"]], method = "bonferroni", n = 3)

p = t.test(filter(aovmeans_clean2,Explabel=="ToneasSpeech" & fOnsetR=="ontime")$Shorter,filter(aovmeans_clean2,Explabel=="ToneasSpeech" & fOnsetR=="late")$Shorter,paired=T)
p
cohen.d(filter(aovmeans_clean2,Explabel=="ToneasSpeech" & fOnsetR=="ontime")$Shorter,filter(aovmeans_clean2,Explabel=="ToneasSpeech" & fOnsetR=="late")$Shorter,paired=T)
p.adjust(p[["p.value"]], method = "bonferroni", n = 3)

## ANOVA on 50% point
m = summary(aov(fifty~fOnsetR*Explabel+Error(sub_id/fOnsetR),data=aovmeans_clean2)) 
# calculate partial generalized eta sq https://www.aggieerin.com/shiny-server/tests/gesmixss.html and based on Olejnik & Algina (2003)
m$'Error: sub_id'[[1]]$`Sum Sq`[1]/(m$'Error: sub_id'[[1]]$`Sum Sq`[1]+m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]) # Target
m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[1]/(m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[1]+m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]) # Onset
m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[2]/(m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[2]+m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]+m$'Error: sub_id'[[1]]$`Sum Sq`[1]) # Onset*Target

m = summary(aov(fifty~fOnsetR*Explabel+Error(sub_id/fOnsetR),data=filter(aovmeans_clean2,Explabel!="ToneasSpeech"))) 
# calculate partial generalized eta sq https://www.aggieerin.com/shiny-server/tests/gesmixss.html and based on Olejnik & Algina (2003)
m$'Error: sub_id'[[1]]$`Sum Sq`[1]/(m$'Error: sub_id'[[1]]$`Sum Sq`[1]+m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]) # Target
m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[1]/(m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[1]+m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]) # Onset
m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[2]/(m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[2]+m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]+m$'Error: sub_id'[[1]]$`Sum Sq`[1]) # Onset*Target

m = summary(aov(fifty~fOnsetR*Explabel+Error(sub_id/fOnsetR),data=filter(aovmeans_clean2,Explabel!="Speech"))) 
# calculate partial generalized eta sq https://www.aggieerin.com/shiny-server/tests/gesmixss.html and based on Olejnik & Algina (2003)
m$'Error: sub_id'[[1]]$`Sum Sq`[1]/(m$'Error: sub_id'[[1]]$`Sum Sq`[1]+m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]) # Target
m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[1]/(m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[1]+m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]) # Onset
m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[2]/(m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[2]+m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]+m$'Error: sub_id'[[1]]$`Sum Sq`[1]) # Onset*Target

m = summary(aov(fifty~fOnsetR+Error(sub_id/fOnsetR),data=filter(aovmeans_clean2,Explabel=="Speech"))) 
m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[1]/(sum(m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`)+m$'Error: sub_id'[[1]]$`Sum Sq`[1]) # calculate partial generalized eta sq
m = summary(aov(fifty~fOnsetR+Error(sub_id/fOnsetR),data=filter(aovmeans_clean2,Explabel=="Tones"))) 
m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[1]/(sum(m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`)+m$'Error: sub_id'[[1]]$`Sum Sq`[1]) # calculate partial generalized eta sq
m = summary(aov(fifty~fOnsetR+Error(sub_id/fOnsetR),data=filter(aovmeans_clean2,Explabel=="ToneasSpeech"))) 
m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[1]/(sum(m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`)+m$'Error: sub_id'[[1]]$`Sum Sq`[1]) # calculate partial generalized eta sq

m = summary(aov(fifty~fOnsetR*Explabel+Error(sub_id/fOnsetR),data=filter(aovmeans_clean2,Explabel!="Tones")))
# calculate partial generalized eta sq https://www.aggieerin.com/shiny-server/tests/gesmixss.html and based on Olejnik & Algina (2003)
m$'Error: sub_id'[[1]]$`Sum Sq`[1]/(m$'Error: sub_id'[[1]]$`Sum Sq`[1]+m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]) # Target
m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[1]/(m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[1]+m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]) # Onset
m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[2]/(m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[2]+m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]+m$'Error: sub_id'[[1]]$`Sum Sq`[1]) # Onset*Target

# Speech
p = t.test(filter(aovmeans_clean2,Explabel=="Speech" & fOnsetR=="early")$fifty,filter(aovmeans_clean2,Explabel=="Speech" & fOnsetR=="ontime")$fifty,paired=T)
p
cohen.d(filter(aovmeans_clean2,Explabel=="Speech" & fOnsetR=="early")$fifty,filter(aovmeans_clean2,Explabel=="Speech" & fOnsetR=="ontime")$fifty,paired=T)
p.adjust(p[["p.value"]], method = "bonferroni", n = 3)

p = t.test(filter(aovmeans_clean2,Explabel=="Speech" & fOnsetR=="early")$fifty,filter(aovmeans_clean2,Explabel=="Speech" & fOnsetR=="late")$fifty,paired=T)
p
cohen.d(filter(aovmeans_clean2,Explabel=="Speech" & fOnsetR=="early")$fifty,filter(aovmeans_clean2,Explabel=="Speech" & fOnsetR=="late")$fifty,paired=T)
p.adjust(p[["p.value"]], method = "bonferroni", n = 3)

p = t.test(filter(aovmeans_clean2,Explabel=="Speech" & fOnsetR=="ontime")$fifty,filter(aovmeans_clean2,Explabel=="Speech" & fOnsetR=="late")$fifty,paired=T)
p
cohen.d(filter(aovmeans_clean2,Explabel=="Speech" & fOnsetR=="ontime")$fifty,filter(aovmeans_clean2,Explabel=="Speech" & fOnsetR=="late")$fifty,paired=T)
p.adjust(p[["p.value"]], method = "bonferroni", n = 3)

# Tone
p = t.test(filter(aovmeans_clean2,Explabel=="Tones" & fOnsetR=="early")$fifty,filter(aovmeans_clean2,Explabel=="Tones" & fOnsetR=="ontime")$fifty,paired=T)
p
cohen.d(filter(aovmeans_clean2,Explabel=="Tones" & fOnsetR=="early")$fifty,filter(aovmeans_clean2,Explabel=="Tones" & fOnsetR=="ontime")$fifty,paired=T)
p.adjust(p[["p.value"]], method = "bonferroni", n = 3)

p = t.test(filter(aovmeans_clean2,Explabel=="Tones" & fOnsetR=="early")$fifty,filter(aovmeans_clean2,Explabel=="Tones" & fOnsetR=="late")$fifty,paired=T)
p
cohen.d(filter(aovmeans_clean2,Explabel=="Tones" & fOnsetR=="early")$fifty,filter(aovmeans_clean2,Explabel=="Tones" & fOnsetR=="late")$fifty,paired=T)
p.adjust(p[["p.value"]], method = "bonferroni", n = 3)

p = t.test(filter(aovmeans_clean2,Explabel=="Tones" & fOnsetR=="ontime")$fifty,filter(aovmeans_clean2,Explabel=="Tones" & fOnsetR=="late")$fifty,paired=T)
p
cohen.d(filter(aovmeans_clean2,Explabel=="Tones" & fOnsetR=="ontime")$fifty,filter(aovmeans_clean2,Explabel=="Tones" & fOnsetR=="late")$fifty,paired=T)
p.adjust(p[["p.value"]], method = "bonferroni", n = 3)

# ToneasSpeech
p = t.test(filter(aovmeans_clean2,Explabel=="ToneasSpeech" & fOnsetR=="early")$fifty,filter(aovmeans_clean2,Explabel=="ToneasSpeech" & fOnsetR=="ontime")$fifty,paired=T)
p
cohen.d(filter(aovmeans_clean2,Explabel=="ToneasSpeech" & fOnsetR=="early")$fifty,filter(aovmeans_clean2,Explabel=="ToneasSpeech" & fOnsetR=="ontime")$fifty,paired=T)
p.adjust(p[["p.value"]], method = "bonferroni", n = 3)

p = t.test(filter(aovmeans_clean2,Explabel=="ToneasSpeech" & fOnsetR=="early")$fifty,filter(aovmeans_clean2,Explabel=="ToneasSpeech" & fOnsetR=="late")$fifty,paired=T)
p
cohen.d(filter(aovmeans_clean2,Explabel=="ToneasSpeech" & fOnsetR=="early")$fifty,filter(aovmeans_clean2,Explabel=="ToneasSpeech" & fOnsetR=="late")$fifty,paired=T)
p.adjust(p[["p.value"]], method = "bonferroni", n = 3)

p = t.test(filter(aovmeans_clean2,Explabel=="ToneasSpeech" & fOnsetR=="ontime")$fifty,filter(aovmeans_clean2,Explabel=="ToneasSpeech" & fOnsetR=="late")$fifty,paired=T)
p
cohen.d(filter(aovmeans_clean2,Explabel=="ToneasSpeech" & fOnsetR=="ontime")$fifty,filter(aovmeans_clean2,Explabel=="ToneasSpeech" & fOnsetR=="late")$fifty,paired=T)
p.adjust(p[["p.value"]], method = "bonferroni", n = 3)

## glmer on proportion short
alldata_clean_allEXPlabel = filter(alldata, sub_id %in% unique(aovmeans_clean2$sub_id)) 
alldata_cleanGLM = filter(alldata_clean_allEXPlabel, Explabel!= "EXP8a") # could be EXP8a, EXP8b or EXP8c
alldata_cleanGLM$Explabel = ifelse(alldata_cleanGLM$exp=="EXP8b",-0.5,0.5) # sum coding for the two conditions being compared, could be EXP8a, EXP8b or EXP8c

# full
lmall = glmer(Shorter ~ Explabel*fOnsetE*rLength  + (1 + fOnsetE*rLength|sub_id),data= alldata_cleanGLM,family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
summary(lmall) # Use early as the reference

# reduce Target Duration (rLength)
lmall_norLength = glmer(Shorter ~ Explabel*fOnsetE*rLength-rLength  + (1 + fOnsetE*rLength|sub_id),data= alldata_cleanGLM,family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
summary(lmall_norLength) # Use early as the reference
anova(lmall,lmall_norLength)

# reduce 2 way
lmall_no2way = glmer(Shorter ~ Explabel*fOnsetE*rLength-Explabel:fOnsetE  + (1 + fOnsetE*rLength|sub_id),data= alldata_cleanGLM,family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
summary(lmall_no2way) # Use early as the reference
anova(lmall,lmall_no2way)

# reduce 3 way
lmall_no3way = glmer(Shorter ~ Explabel*fOnsetE*rLength-Explabel:fOnsetE:rLength  + (1 + fOnsetE*rLength|sub_id),data= alldata_cleanGLM,family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
summary(lmall_no3way) # Use early as the reference
anova(lmall,lmall_no3way)

# submodels
lmall_speech = glmer(Shorter ~ fOnsetE*rLength  + (1 + fOnsetE*rLength|sub_id),data= filter(alldata_cleanGLM,exp=="EXP8a"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
lmall_speech_noOnset = glmer(Shorter ~ fOnsetE*rLength - fOnsetE  + (1 + fOnsetE*rLength|sub_id),data= filter(alldata_cleanGLM,exp=="EXP8a"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
lmall_tone = glmer(Shorter ~ fOnsetE*rLength  + (1 + fOnsetE*rLength|sub_id),data= filter(alldata_cleanGLM,exp=="EXP8b"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
lmall_tone_noOnset = glmer(Shorter ~ fOnsetE*rLength - fOnsetE + (1 + fOnsetE*rLength|sub_id),data= filter(alldata_cleanGLM,exp=="EXP8b"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
lmall_toneasspeech = glmer(Shorter ~ fOnsetE*rLength  + (1 + fOnsetE*rLength|sub_id),data= filter(alldata_cleanGLM,exp=="EXP8c"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
lmall_toneasspeech_noOnset = glmer(Shorter ~ fOnsetE*rLength - fOnsetE  + (1 + fOnsetE*rLength|sub_id),data= filter(alldata_cleanGLM,exp=="EXP8c"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  

summary(lmall_speech) # Use early as the reference
summary(lmall_tone) # Use early as the reference
summary(lmall_toneasspeech) # Use early as the reference
anova(lmall_speech,lmall_speech_noOnset)
anova(lmall_tone,lmall_tone_noOnset)
anova(lmall_toneasspeech,lmall_toneasspeech_noOnset)

## Tests to compare the variance between Speech and Tone conditions
aovmeans_clean2_vartest = select(aovmeans_clean2,sub_id,Explabel,fOnsetR,Shorter) # aovmeans_clean2 is the data average across Length
aovmeans_clean2_vartest = aovmeans_clean2_vartest %>%
  pivot_wider(names_from = fOnsetR, values_from = Shorter) %>% 
  mutate(diff = early-late)

aovmeans_clean2_vartest$Explabel = factor(aovmeans_clean2_vartest$Explabel, levels = c("EXP8a","EXP8b"))

ggplot(aovmeans_clean2_vartest, aes(x = Explabel, y = diff)) +
  geom_boxplot(outlier.size = 0) + 
  geom_point(position = position_jitter()) +
  theme_bw()

bartlett.test(diff ~ Explabel, data = aovmeans_clean2_vartest)
levene_test(diff ~ Explabel, data = aovmeans_clean2_vartest)