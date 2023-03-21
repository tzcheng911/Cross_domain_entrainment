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
EXPtone = read.csv("results/EXP4c_clean_n71.csv") 
EXPspeech = read.csv("results/EXP6_clean_n79.csv")

## flag the overlapping subjects between EXP4 and EXP6
overlapsubj = intersect(EXPtone$sub_id,EXPspeech$sub_id)
length(overlapsubj)
EXPtone$overlap = ifelse(Reduce(`|`, lapply(overlapsubj, `==`, EXPtone$sub_id)),1,0)
EXPspeech$overlap = ifelse(Reduce(`|`, lapply(overlapsubj, `==`, EXPspeech$sub_id)),1,0)

alldata=rbind(select(EXPtone,participant_id,sub_id,exp,Onset,Length,Shorter,Correct,overlap),select(EXPspeech,participant_id,sub_id,exp,Onset,Length,Shorter,Correct,overlap))
alldata = alldata %>%
  filter(alldata$overlap == 0) %>%
  droplevels


## Rescale and mutate new factors
alldata = alldata %>%
  mutate(rLength = scale(Length, center = TRUE, scale = TRUE)) # scale the steps ## 4c: scale Length; 6: scale comparison
alldata = alldata %>%
  mutate(fOnset = as.factor(Onset))
alldata = alldata %>%
  mutate(Explabel = as.factor(exp))

## Onset coding and reference 
## SC: this shouldn't be necessary for ANOVAs, right? Just LMERs?
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
aovmeans_clean1 = filter(aovmeans, !(sub_id %in% unique(outliers_slope_subj$sub_id)))
length(unique(outliers_slope_subj$sub_id)) # ?? subjects dropped
length(unique(outliers_slope_subj$sub_id))/length(unique(aovmeans_clean1$sub_id)) #...

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
max(dim(aovmeans_clean2))/max(dim(aovmeans_clean1)) # 84.7% data retained--SCC
length(unique(outliers_subj_50$sub_id)) # ?? subjects dropped
length(unique(outliers_subj_50$sub_id))/length(unique(aovmeans_clean1$sub_id)) # further ??% dropped here


# Descriptive stats
aovmeans_clean2 %>%
  group_by(Explabel,fOnsetR) %>%
  summarize(mean(fifty),mean(intercept),mean(Shorter))

# plot overall results
aovdata_clean = filter(aovdata, sub_id %in% unique(aovmeans_clean2$sub_id)) 
aovdata_outlier_slope = filter(aovdata, sub_id %in% unique(outliers_slope_subj$sub_id)) 
aovdata_outlier_50 = filter(aovdata, sub_id %in% unique(outliers_subj_50$sub_id)) 
aovdata_clean$fOnsetR = factor(aovdata_clean$fOnsetR, levels = c("early","ontime","late"))
aovdata_outlier_slope$fOnsetR = factor(aovdata_outlier_slope$fOnsetR, levels = c("early","ontime","late"))
aovdata_outlier_50$fOnsetR = factor(aovdata_outlier_50$fOnsetR, levels = c("early","ontime","late"))

aovdata_clean_plot = aovdata_clean %>% group_by(rLength,fOnsetR,Explabel) %>% summarise(mShorter=mean(Shorter),SD=sd(Shorter),Nsubs=n_distinct(sub_id))
aovdata_clean_plot$fOnsetR = factor(aovdata_clean_plot$fOnsetR, levels = c("early","ontime","late"))
aovdata_clean_plot$Explabel = factor(aovdata_clean_plot$Explabel, levels = c("EXP4c","EXP6"))

ggplot(aovdata_clean_plot,aes(x=rLength,y=mShorter,color=fOnsetR,group=interaction(fOnsetR,Explabel)))+
  geom_point()+
  scale_x_continuous(breaks = seq(1, 8, by = 1))+
  geom_line()+
  geom_errorbar(aes(ymin=mShorter-SD/sqrt(Nsubs),ymax=mShorter+SD/sqrt(Nsubs)),width=0)+
  facet_grid(Explabel~.)+
  theme_minimal()

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
  facet_wrap(Explabel~sub_id) # SCC added Explabel to see who's in what cond

## relabel Speech to EXP8a and Tone to EXP8b and 
aovmeans_clean2$Explabel = ifelse(aovmeans_clean2$Explabel == "EXP6","Speech","Tones")
aovmeans_clean2$fOnsetR = factor(aovmeans_clean2$fOnsetR, levels = c("early","ontime","late"))
aovmeans_clean2$Explabel = factor(aovmeans_clean2$Explabel, levels = c("Speech","Tones"))

ggplot(aovmeans_clean2, aes(x = Explabel, y = fifty, fill = fOnsetR)) +
  geom_boxplot(outlier.size = 0) + 
  geom_point(position = position_jitterdodge(jitter.width = 0.1)) +
  labs(x = "Onset")
ggplot(aovmeans_clean2, aes(x = Explabel, y = Shorter, fill = fOnsetR)) +
  geom_boxplot(outlier.size = 0) + 
  geom_point(position = position_jitterdodge(jitter.width = 0.1)) +
  labs(x = "Onset")

## Final sample size
exp8a = filter(aovmeans_clean2,Explabel == "Speech")
exp8b = filter(aovmeans_clean2,Explabel == "Tones")
length(unique(exp8a$sub_id))
length(unique(exp8b$sub_id))

## ANOVA on proportion short
m = summary(aov(Shorter~fOnsetR*Explabel+Error(sub_id/fOnsetR),data=aovmeans_clean2)) 
# calculate partial generalized eta sq https://urldefense.com/v3/__https://www.aggieerin.com/shiny-server/tests/gesmixss.html__;!!Mih3wA!GIruiklcnAtbDdh8RrFHDsNw9ZBmCPmEtvlvelVk8J7iSlh5lPOnaepdVA3Zxa8KQYQuahfqgn7UcGWE$  and based on Olejnik & Algina (2003)
m$'Error: sub_id'[[1]]$`Sum Sq`[1]/(m$'Error: sub_id'[[1]]$`Sum Sq`[1]+m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]) # Target
m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[1]/(m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[1]+m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]) # Onset
m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[2]/(m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[2]+m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]+m$'Error: sub_id'[[1]]$`Sum Sq`[1]) # Onset*Target

m = summary(aov(Shorter~fOnsetR+Error(sub_id/fOnsetR),data=filter(aovmeans_clean2,Explabel=="Speech"))) 
m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[1]/(sum(m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`)+m$'Error: sub_id'[[1]]$`Sum Sq`[1]) # calculate partial generalized eta sq
m = summary(aov(Shorter~fOnsetR+Error(sub_id/fOnsetR),data=filter(aovmeans_clean2,Explabel=="Tones"))) 
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

p = t.test(filter(aovmeans_clean2,Explabel=="Speech" & fOnsetR=="late")$Shorter,filter(aovmeans_clean2,Explabel=="Speech" & fOnsetR=="ontime")$Shorter,paired=T)
p
cohen.d(filter(aovmeans_clean2,Explabel=="Speech" & fOnsetR=="late")$Shorter,filter(aovmeans_clean2,Explabel=="Speech" & fOnsetR=="ontime")$Shorter,paired=T)
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

p = t.test(filter(aovmeans_clean2,Explabel=="Tones" & fOnsetR=="late")$Shorter,filter(aovmeans_clean2,Explabel=="Tones" & fOnsetR=="ontime")$Shorter,paired=T)
p
cohen.d(filter(aovmeans_clean2,Explabel=="Tones" & fOnsetR=="late")$Shorter,filter(aovmeans_clean2,Explabel=="Tones" & fOnsetR=="ontime")$Shorter,paired=T)
p.adjust(p[["p.value"]], method = "bonferroni", n = 3)

## ANOVA on 50% point
m = summary(aov(fifty~fOnsetR*Explabel+Error(sub_id/fOnsetR),data=aovmeans_clean2)) 
# calculate partial generalized eta sq https://urldefense.com/v3/__https://www.aggieerin.com/shiny-server/tests/gesmixss.html__;!!Mih3wA!GIruiklcnAtbDdh8RrFHDsNw9ZBmCPmEtvlvelVk8J7iSlh5lPOnaepdVA3Zxa8KQYQuahfqgn7UcGWE$  and based on Olejnik & Algina (2003)
m$'Error: sub_id'[[1]]$`Sum Sq`[1]/(m$'Error: sub_id'[[1]]$`Sum Sq`[1]+m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]) # Target
m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[1]/(m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[1]+m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]) # Onset
m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[2]/(m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[2]+m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]+m$'Error: sub_id'[[1]]$`Sum Sq`[1]) # Onset*Target

m = summary(aov(fifty~fOnsetR+Error(sub_id/fOnsetR),data=filter(aovmeans_clean2,Explabel=="Speech"))) 
m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[1]/(sum(m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`)+m$'Error: sub_id'[[1]]$`Sum Sq`[1]) # calculate partial generalized eta sq
m = summary(aov(fifty~fOnsetR+Error(sub_id/fOnsetR),data=filter(aovmeans_clean2,Explabel=="Tones"))) 
m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`[1]/(sum(m$'Error: sub_id:fOnsetR'[[1]]$`Sum Sq`)+m$'Error: sub_id'[[1]]$`Sum Sq`[1]) # calculate partial generalized eta sq

# Speech
p = t.test(filter(aovmeans_clean2,Explabel=="Speech" & fOnsetR=="early")$fifty,filter(aovmeans_clean2,Explabel=="Speech" & fOnsetR=="ontime")$fifty,paired=T)
p
cohen.d(filter(aovmeans_clean2,Explabel=="Speech" & fOnsetR=="early")$fifty,filter(aovmeans_clean2,Explabel=="Speech" & fOnsetR=="ontime")$fifty,paired=T)
p.adjust(p[["p.value"]], method = "bonferroni", n = 3)

p = t.test(filter(aovmeans_clean2,Explabel=="Speech" & fOnsetR=="early")$fifty,filter(aovmeans_clean2,Explabel=="Speech" & fOnsetR=="late")$fifty,paired=T)
p
cohen.d(filter(aovmeans_clean2,Explabel=="Speech" & fOnsetR=="early")$fifty,filter(aovmeans_clean2,Explabel=="Speech" & fOnsetR=="late")$fifty,paired=T)
p.adjust(p[["p.value"]], method = "bonferroni", n = 3)

p = t.test(filter(aovmeans_clean2,Explabel=="Speech" & fOnsetR=="late")$fifty,filter(aovmeans_clean2,Explabel=="Speech" & fOnsetR=="ontime")$fifty,paired=T)
p
cohen.d(filter(aovmeans_clean2,Explabel=="Speech" & fOnsetR=="late")$fifty,filter(aovmeans_clean2,Explabel=="Speech" & fOnsetR=="ontime")$fifty,paired=T)
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

p = t.test(filter(aovmeans_clean2,Explabel=="Tones" & fOnsetR=="late")$fifty,filter(aovmeans_clean2,Explabel=="Tones" & fOnsetR=="ontime")$fifty,paired=T)
p
cohen.d(filter(aovmeans_clean2,Explabel=="Tones" & fOnsetR=="late")$fifty,filter(aovmeans_clean2,Explabel=="Tones" & fOnsetR=="ontime")$fifty,paired=T)
p.adjust(p[["p.value"]], method = "bonferroni", n = 3)
