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
## Load the data
EXPspeech = read.csv("EXP1a_clean_n80.csv")
EXPtone = read.csv("EXP1b_clean_n84.csv") 
EXPtoneasspeech = read.csv("EXP1c_clean_n88.csv") ## all subjects in the Tone-as-Speech condition
# EXPtoneasspeech = read.csv("EXP1c-s_clean_n20.csv") ## subjects reported hearing tone as speech in the Tone-as-Speech condition
# EXPtoneasspeech = read.csv("EXP1c-s_clean_n34.csv") ## additional subjects reported hearing tone as speech in the Tone-as-Speech condition

## Combine the 3 conditions
alldata=rbind(select(EXPtone,participant_id,sub_id,exp,Onset,Length,Shorter,Correct),select(EXPspeech,participant_id,sub_id,exp,Onset,Length,Shorter,Correct),
              select(EXPtoneasspeech,participant_id,sub_id,exp,Onset,Length,Shorter,Correct))

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
  group_by(fOnsetE,Explabel,Length,sub_id) %>% summarise(Shorter=mean(Shorter)) # Important!! Use Length for plotting (Length 1-8, instead of the scaled Length -1.52 - 1.52 for easier visualization), use rLength for fitting model & ANOVA test

## Run logistic fit on each subject and condition
aovmeans=aovdata %>% 
  group_by(sub_id,fOnsetE,Explabel) %>% 
  do(glmfit = glm(Shorter ~ Length,data =.,family=binomial())) # Important!! Use Length for plotting (Length 1-8, instead of the scaled Length -1.52 - 1.52 for easier visualization), use rLength for fitting model & ANOVA test

## Get the coefficients 
aovmeans = aovmeans %>%
  mutate(intercept = coef(glmfit)[1], slope = coef(glmfit)[2], fifty = -coef(glmfit)[1]/coef(glmfit)[2], deviance = glmfit$aic)

## Mutate columes of pps
pps = aovdata %>% group_by(sub_id,fOnsetE,Explabel) %>% summarise(ShorterM=mean(Shorter),ShorterSD=sd(Shorter),Nsubs=n_distinct(sub_id),SE=ShorterSD/sqrt(Nsubs))
aovmeans = cbind(pps$ShorterM,aovmeans)
colnames(aovmeans)[1] = 'Shorter'

## Flag outliers based on slope
# who have reverse slopes, flat lines
# 'af90a' press the same button across all experiment 
aovmeans$outliers_slope = ifelse(aovmeans$slope>= 0 | aovmeans$sub_id == 'af90a',1,0)
outliers_slope_subj = filter(aovmeans,outliers_slope==1)
aovmeans_clean1 = filter(aovmeans, !(sub_id %in% unique(outliers_slope_subj$sub_id)))
length(unique(outliers_slope_subj$sub_id))

## Flag outliers based on 50% point based on 1.5*IQR criteria
q1 = quantile(aovmeans_clean1$fifty,.25)
q3 = quantile(aovmeans_clean1$fifty,.75)
iqr = IQR(aovmeans_clean1$fifty)
aovmeans_clean1$outliers_50 = ifelse(aovmeans_clean1$fifty < (q1 - 1.5*iqr) | aovmeans_clean1$fifty > (q3 + 1.5*iqr),1,0)
outliers_subj_50 = filter(aovmeans_clean1,outliers_50==1)
aovmeans_clean2 = filter(aovmeans_clean1, !(sub_id %in% unique(outliers_subj_50$sub_id)))
hist(aovmeans_clean2$fifty,200)

## Get descriptive stats and plot ready
aovmeans_clean2 %>%
  group_by(Explabel,fOnsetE) %>%
  summarize(mean(fifty),mean(intercept),mean(Shorter))
summary_aovmeans_clean2 = aovmeans_clean2 %>%
  group_by(Explabel,fOnsetE) %>%
  summarize(mfifty = mean(fifty), mShorter = mean(Shorter), Nsubs=n_distinct(sub_id), sefifty = sd(fifty)/sqrt(Nsubs), seShorter = sd(Shorter)/sqrt(Nsubs),sdShorter = sd(Shorter))
aovdata_clean = filter(aovdata, sub_id %in% unique(aovmeans_clean2$sub_id)) 
aovdata_outlier_slope = filter(aovdata, sub_id %in% unique(outliers_slope_subj$sub_id)) 
aovdata_outlier_50 = filter(aovdata, sub_id %in% unique(outliers_subj_50$sub_id)) 
aovdata_clean$fOnsetE = factor(aovdata_clean$fOnsetE, levels = c("early","ontime","late"))
aovdata_outlier_slope$fOnsetE = factor(aovdata_outlier_slope$fOnsetE, levels = c("early","ontime","late"))
aovdata_outlier_50$fOnsetE = factor(aovdata_outlier_50$fOnsetE, levels = c("early","ontime","late"))
aovdata_clean$Explabel = factor(aovdata_clean$Explabel, levels = c("EXP8a","EXP8b","EXP8c"))
aovdata_outlier_slope$Explabel = factor(aovdata_outlier_slope$Explabel, levels = c("EXP8a","EXP8b","EXP8c"))
aovdata_outlier_50$Explabel = factor(aovdata_outlier_50$Explabel, levels = c("EXP8a","EXP8b","EXP8c"))
aovdata_clean_plot = aovdata_clean %>% group_by(Length,fOnsetE,Explabel) %>% summarise(mShorter=mean(Shorter),SD=sd(Shorter),Nsubs=n_distinct(sub_id)) # Important!! Use Length for plotting (Length 1-8, instead of the scaled Length -1.52 - 1.52 for easier visualization), use rLength for fitting model & ANOVA test
aovdata_clean_plot$fOnsetE = factor(aovdata_clean_plot$fOnsetE, levels = c("early","ontime","late"))
aovdata_clean_plot$Explabel = factor(aovdata_clean_plot$Explabel, levels = c("EXP8a","EXP8b","EXP8c"))

# Plot averaged curves of the clean data
ggplot(aovdata_clean_plot,aes(x=Length,y=mShorter,color=fOnsetE,linetype=Explabel,group=interaction(fOnsetE,Explabel)))+
  geom_point()+
  scale_x_continuous(breaks = seq(1, 8, by = 1))+
  geom_line()+
  geom_errorbar(aes(ymin=mShorter-SD/sqrt(Nsubs),ymax=mShorter+SD/sqrt(Nsubs)),width=0)+
  facet_grid(Explabel~.)+
  theme_bw()

# Plot individual curves of the clean data
ggplot(aovdata_clean,aes(x=Length,y=Shorter,color=fOnsetE,shape=Explabel))+
  scale_color_manual(values=c("red","green","blue"))+
  geom_point()+
  # geom_line()+
  # geom_smooth(method="lm",formula=y ~ exp(x)/(1+exp(x)),se=FALSE)+
   geom_smooth(method="lm",se=FALSE) +
  # geom_smooth(method="glm",method.args = list(family = "binomial"),se=FALSE) +
  facet_wrap(sub_id~.) +
  theme(strip.text.x = element_blank())

## Relabel Exp8abc to Speech, Tone and ToneasSpeech
aovmeans_clean2$Explabel = ifelse(aovmeans_clean2$Explabel == "EXP8a","Speech",ifelse(aovmeans_clean2$Explabel == "EXP8b","Tones","ToneasSpeech"))
aovmeans_clean2$Explabel = factor(aovmeans_clean2$Explabel, levels = c("Speech","Tones","ToneasSpeech"))
aovmeans_clean2$fOnsetE = factor(aovmeans_clean2$fOnsetE, levels = c("early","ontime","late"))

## Plot the bar plot: use the Length instead of rLength so the 50% point is more interpretable (Line 36, 41, 85)
ggplot(aovmeans_clean2, aes(x = Explabel, y = Shorter, fill = fOnsetE)) +
  geom_bar(stat="summary", fun.y = "mean", position='dodge') +
  stat_summary(fun.data=mean_se, geom="errorbar", position = position_dodge(width = 0.9), width=.1,color="grey") +
  ylim(0,0.8) + 
  geom_point(position = position_jitterdodge(jitter.width = 0.3,dodge.width = 0.9), color="black")+
  theme_bw()

ggplot(aovmeans_clean2, aes(x = Explabel, y = fifty, fill = fOnsetE)) +
  geom_bar(stat="summary", fun.y = "mean", position='dodge') +
  stat_summary(fun.data=mean_se, geom="errorbar", position = position_dodge(width = 0.9), width=.1,color="grey") +
  geom_point(position = position_jitterdodge(jitter.width = 0.3,dodge.width = 0.9), color="black")+
  theme_bw()

## Get final sample size after excluding the outliers
exp8a = filter(aovmeans_clean2,Explabel == "Speech")
exp8b = filter(aovmeans_clean2,Explabel == "Tones")
exp8c = filter(aovmeans_clean2,Explabel == "ToneasSpeech")
length(unique(exp8a$sub_id))
length(unique(exp8b$sub_id))
length(unique(exp8c$sub_id))

######################################################## Statistical test ######################################################## 

#############################################################################################################################
############################################## Run logistic mixed-effect model ##############################################
#############################################################################################################################
alldata_clean_allEXPlabel = filter(alldata, sub_id %in% unique(aovmeans_clean2$sub_id)) ## change to aovmeans_clean1 to include 50% point outliers 

## Do paired comparison of Tone vs. Speech; Tone vs. ToneasSpeech; Speech vs. ToneasSpeech
## Run the Full and Reduced 2-way models on alldata_cleanGLM to see the interaction
alldata_cleanGLM = filter(alldata_clean_allEXPlabel, Explabel!= "EXP8c") # EXP8a (compare EXP8b vs c), EXP8b (compare EXP8a vs c) or EXP8c (compare EXP8a vs b)
alldata_cleanGLM$Explabel = ifelse(alldata_cleanGLM$exp=="EXP8a",-0.5,0.5) # sum coding for the two conditions being compared, could be EXP8a, EXP8b or EXP8c

## Coding the contrast
# Contrast the 3 Auditory Targets (speech, tone, tone-as-speech) by Helmert coding: comparing speech vs other conditions, and tone vs toneasspeech
# Contrast the 3 Onset Times (early ontime late) by simple coding: comparing early vs ontime, early vs late, but centered (-1/3,2/3,-1/3; -1/3,-1/3, 2/3)
alldata_clean_allEXPlabel = alldata_clean_allEXPlabel %>%
  mutate(earlyVSontime=ifelse(fOnsetE=="ontime",2/3,-1/3),
         earlyVSlate=ifelse(fOnsetE=="late",2/3,-1/3), # note that early is always -1/3, so it's reference level
         speechVSothers=ifelse(exp=="EXP8a",2/3,-1/3), # 8a = speech
         toneVStoneasspeech=ifelse(exp=="EXP8b",.5,
                                   ifelse(exp=="EXP8c",-.5,0))) # 8b = tone, 8c = tas
## Check the coding
alldata_clean_allEXPlabel %>% group_by(speechVSothers,toneVStoneasspeech,
                                       earlyVSontime,earlyVSlate) %>% summarise(n())

## Use BUILDMER package to figure out which random fx to drop https://cran.r-project.org/web/packages/buildmer/vignettes/buildmer.html
library(buildmer)
fmla=Shorter ~ (speechVSothers+toneVStoneasspeech)*(earlyVSontime+earlyVSlate)*rLength  + 
  (1 + (earlyVSontime+earlyVSlate)*rLength|sub_id)
m <- buildmer(fmla,data=alldata_clean_allEXPlabel,
              family="binomial",
              buildmerControl=buildmerControl(direction='order',
                                              args=list(control=glmerControl(optimizer='bobyqa'))))
summary(m) # random fx retained:     (1 + rLength + earlyVSlate + earlyVSontime | sub_id)

## Full model
lm = glmer(Shorter ~ (speechVSothers+toneVStoneasspeech)*(earlyVSontime+earlyVSlate)*rLength  + 
                   (1 + earlyVSontime+earlyVSlate+rLength|sub_id),
                 data= alldata_clean_allEXPlabel,family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
summary(lm)

## Test the omnibus Auditory Targets x Onset Times 2-way interaction 2-way interaction  
lm_nointeraction = glmer(Shorter ~ (speechVSothers+toneVStoneasspeech)*(earlyVSontime+earlyVSlate)*rLength  -
                                 (speechVSothers+toneVStoneasspeech):(earlyVSontime+earlyVSlate) + 
                                 (1 + rLength + earlyVSlate + earlyVSontime | sub_id),
                               data= alldata_clean_allEXPlabel,family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
anova(lm,lm_nointeraction)      

## Test the omnibus Auditory Targets x Onset Times x Target Durations 3-way interaction  
lm_no3wayinteraction = glmer(Shorter ~ speechVSothers*(earlyVSontime+earlyVSlate)*rLength  -
                                     (speechVSothers+toneVStoneasspeech):(earlyVSontime+earlyVSlate):rLength + 
                                     (1 + rLength + earlyVSlate + earlyVSontime | sub_id),
                                   data= alldata_clean_allEXPlabel,family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
anova(lm,lm_no3wayinteraction) 

## Test the Auditory Targets (speech vs. others) x Onset Times (early vs. late) 2-way interaction  
lm_noSimpleinteraction = glmer(Shorter ~ (speechVSothers+toneVStoneasspeech)*(earlyVSontime+earlyVSlate)*rLength  -
                                       (speechVSothers:earlyVSlate) + 
                                       (1 + (earlyVSontime+earlyVSlate+rLength)|sub_id),
                                     data= alldata_clean_allEXPlabel,family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
anova(lm,lm_noSimpleinteraction)      

## Test the Auditory Targets (speech vs. others) x Onset Times (both early vs. late and early vs. ontime) 2-way interaction  
lm_noSpeechTerminteraction = glmer(Shorter ~ (speechVSothers+toneVStoneasspeech)*(earlyVSontime+earlyVSlate)*rLength  -
                                           (speechVSothers:(earlyVSlate + earlyVSontime)) + # i.e., the targetType x delay interaction 
                                           (1 + (earlyVSontime+earlyVSlate+rLength)|sub_id),
                                         data= alldata_clean_allEXPlabel,family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
anova(lm,lm_noSpeechTerminteraction)      

## Test the Target Duration
lm_noLength = glmer(Shorter ~ (speechVSothers+toneVStoneasspeech)*(earlyVSontime+earlyVSlate)*rLength  -
                          rLength + (1 + (earlyVSontime+earlyVSlate+rLength)|sub_id),
                        data= alldata_clean_allEXPlabel,family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
anova(lm,lm_noLength)

## FOR SIMPLICITY THE SUBMODELS RETAIN THE RANDOM EFFECTS STRUCTURE OF THE MAIN MODEL
alldata_clean_allEXPlabel=alldata_clean_allEXPlabel %>%
  mutate(speechIsHigher=ifelse(exp=="EXP8a",.5,-.5), # works for both speech vs x comparisons
         toneIsHigher=ifelse(exp=="EXP8b",.5,-.5)) # tone vs toneasspeech

## Test the interaction just in Speech vs Tone (no Tone-as-Speech) condition
lmSpTn=glmer(Shorter ~ speechIsHigher*(earlyVSontime+earlyVSlate)*rLength  + 
               (1 + earlyVSlate+rLength|sub_id),
             data= filter(alldata_clean_allEXPlabel,exp!="EXP8c"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
summary(lmSpTn)
lmSpTnNoInteraction=glmer(Shorter ~ speechIsHigher*(earlyVSontime+earlyVSlate)*rLength  -
                            speechIsHigher:(earlyVSontime+earlyVSlate) +
                            (1 + earlyVSlate+rLength|sub_id),
                          data= filter(alldata_clean_allEXPlabel,exp!="EXP8c"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
summary(lmSpTnNoInteraction)
anova(lmSpTn,lmSpTnNoInteraction)

## Test the interaction just in Speech vs Tone-as-Speech (no Tone) condition
lmSpTas=glmer(Shorter ~ speechIsHigher*(earlyVSontime+earlyVSlate)*rLength  + 
                (1 + earlyVSontime+earlyVSlate+rLength|sub_id),
              data= filter(alldata_clean_allEXPlabel,exp!="EXP8b"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
summary(lmSpTas)
lmSpTasNoInteraction=glmer(Shorter ~ speechIsHigher*(earlyVSontime+earlyVSlate)*rLength  -
                             speechIsHigher:(earlyVSontime+earlyVSlate) +
                             (1 + earlyVSontime+earlyVSlate+rLength|sub_id),
                           data= filter(alldata_clean_allEXPlabel,exp!="EXP8b"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
anova(lmSpTas,lmSpTasNoInteraction)

## Test the interaction just in Tone vs Tone-as-Speech (no Speech) condition
lmTnTas=glmer(Shorter ~ toneIsHigher*(earlyVSontime+earlyVSlate)*rLength  + 
                (1 + earlyVSontime+earlyVSlate+rLength|sub_id),
              data= filter(alldata_clean_allEXPlabel,exp!="EXP8a"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
summary(lmTnTas)
lmTnTasNoInteraction=glmer(Shorter ~ toneIsHigher*(earlyVSontime+earlyVSlate)*rLength  -
                             toneIsHigher:(earlyVSontime+earlyVSlate) +
                             (1 + earlyVSontime+earlyVSlate+rLength|sub_id),
                           data= filter(alldata_clean_allEXPlabel,exp!="EXP8a"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
anova(lmTnTas,lmTnTasNoInteraction)

## Test the Onset Times effect in Speech condition
lmSp=glmer(Shorter ~ (earlyVSontime+earlyVSlate)*rLength  + 
             (1 + earlyVSontime+earlyVSlate+rLength|sub_id),
           data= filter(alldata_clean_allEXPlabel,exp=="EXP8a"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
summary(lmSp)
lmSpNoOT=glmer(Shorter ~ (earlyVSontime+earlyVSlate)*rLength  -
                 (earlyVSontime+earlyVSlate) + 
                 (1 + earlyVSontime+earlyVSlate+rLength|sub_id),
               data= filter(alldata_clean_allEXPlabel,exp=="EXP8a"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
anova(lmSp,lmSpNoOT)

## Test the Onset Times effect in Tone condition
lmTn=glmer(Shorter ~ (earlyVSontime+earlyVSlate)*rLength  + 
             (1 + earlyVSontime+earlyVSlate+rLength|sub_id),
           data= filter(alldata_clean_allEXPlabel,exp=="EXP8b"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
summary(lmTn)
lmTnNoOT=glmer(Shorter ~ (earlyVSontime+earlyVSlate)*rLength  -
                 (earlyVSontime+earlyVSlate) + 
                 (1 + earlyVSontime+earlyVSlate+rLength|sub_id),
               data= filter(alldata_clean_allEXPlabel,exp=="EXP8b"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
anova(lmTn,lmTnNoOT)

## Test the Onset Times effect in Tone-as-Speech condition
lmTas=glmer(Shorter ~ (earlyVSontime+earlyVSlate)*rLength  + 
              (1 + earlyVSontime+earlyVSlate+rLength|sub_id),
            data= filter(alldata_clean_allEXPlabel,exp=="EXP8c"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
summary(lmTas)
lmTasNoOT=glmer(Shorter ~ (earlyVSontime+earlyVSlate)*rLength  -
                  (earlyVSontime+earlyVSlate) + 
                  (1 + earlyVSontime+earlyVSlate+rLength|sub_id),
                data= filter(alldata_clean_allEXPlabel,exp=="EXP8c"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
anova(lmTas,lmTasNoOT)

#############################################################################################################################
############################ Implement ANOVA on proportion short: Onset Times x Auditory Targets ############################
#############################################################################################################################

m = summary(aov(Shorter~fOnsetE*Explabel+Error(sub_id/fOnsetE),data=aovmeans_clean2)) 
# calculate partial generalized eta sq https://www.aggieerin.com/shiny-server/tests/gesmixss.html and based on Olejnik & Algina (2003)
m$'Error: sub_id'[[1]]$`Sum Sq`[1]/(m$'Error: sub_id'[[1]]$`Sum Sq`[1]+m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]) # Target
m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[1]/(m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[1]+m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]) # Onset
m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[2]/(m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[2]+m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]+m$'Error: sub_id'[[1]]$`Sum Sq`[1]) # Onset*Target

m = summary(aov(Shorter~fOnsetE*Explabel+Error(sub_id/fOnsetE),data=filter(aovmeans_clean2,Explabel!="ToneasSpeech"))) 
m$'Error: sub_id'[[1]]$`Sum Sq`[1]/(m$'Error: sub_id'[[1]]$`Sum Sq`[1]+m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]) # Target
m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[1]/(m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[1]+m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]) # Onset
m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[2]/(m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[2]+m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]+m$'Error: sub_id'[[1]]$`Sum Sq`[1]) # Onset*Target

m = summary(aov(Shorter~fOnsetE*Explabel+Error(sub_id/fOnsetE),data=filter(aovmeans_clean2,Explabel!="Tones")))
m$'Error: sub_id'[[1]]$`Sum Sq`[1]/(m$'Error: sub_id'[[1]]$`Sum Sq`[1]+m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]) # Target
m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[1]/(m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[1]+m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]) # Onset
m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[2]/(m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[2]+m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]+m$'Error: sub_id'[[1]]$`Sum Sq`[1]) # Onset*Target

m = summary(aov(Shorter~fOnsetE*Explabel+Error(sub_id/fOnsetE),data=filter(aovmeans_clean2,Explabel!="Speech"))) 
m$'Error: sub_id'[[1]]$`Sum Sq`[1]/(m$'Error: sub_id'[[1]]$`Sum Sq`[1]+m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]) # Target
m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[1]/(m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[1]+m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]) # Onset
m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[2]/(m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[2]+m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]+m$'Error: sub_id'[[1]]$`Sum Sq`[1]) # Onset*Target

m = summary(aov(Shorter~fOnsetE+Error(sub_id/fOnsetE),data=filter(aovmeans_clean2,Explabel=="Speech"))) 
m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[1]/(sum(m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`)+m$'Error: sub_id'[[1]]$`Sum Sq`[1]) # calculate partial generalized eta sq
m = summary(aov(Shorter~fOnsetE+Error(sub_id/fOnsetE),data=filter(aovmeans_clean2,Explabel=="Tones"))) 
m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[1]/(sum(m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`)+m$'Error: sub_id'[[1]]$`Sum Sq`[1]) # calculate partial generalized eta sq
m = summary(aov(Shorter~fOnsetE+Error(sub_id/fOnsetE),data=filter(aovmeans_clean2,Explabel=="ToneasSpeech"))) 
m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[1]/(sum(m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`)+m$'Error: sub_id'[[1]]$`Sum Sq`[1]) # calculate partial generalized eta sq

## Implement pairwise ttests between Onset Times (Early vs. Ontime, Early vs. Late, Ontime vs. Late) on each of the Auditory Targets (Speech, Tone, ToneasSpeech)
# Speech
p = t.test(filter(aovmeans_clean2,Explabel=="Speech" & fOnsetE=="early")$Shorter,filter(aovmeans_clean2,Explabel=="Speech" & fOnsetE=="ontime")$Shorter,paired=T)
p
cohen.d(filter(aovmeans_clean2,Explabel=="Speech" & fOnsetE=="early")$Shorter,filter(aovmeans_clean2,Explabel=="Speech" & fOnsetE=="ontime")$Shorter,paired=T)
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

# ToneasSpeech
p = t.test(filter(aovmeans_clean2,Explabel=="ToneasSpeech" & fOnsetE=="early")$Shorter,filter(aovmeans_clean2,Explabel=="ToneasSpeech" & fOnsetE=="ontime")$Shorter,paired=T)
p
cohen.d(filter(aovmeans_clean2,Explabel=="ToneasSpeech" & fOnsetE=="early")$Shorter,filter(aovmeans_clean2,Explabel=="ToneasSpeech" & fOnsetE=="ontime")$Shorter,paired=T)
p.adjust(p[["p.value"]], method = "bonferroni", n = 3)

p = t.test(filter(aovmeans_clean2,Explabel=="ToneasSpeech" & fOnsetE=="early")$Shorter,filter(aovmeans_clean2,Explabel=="ToneasSpeech" & fOnsetE=="late")$Shorter,paired=T)
p
cohen.d(filter(aovmeans_clean2,Explabel=="ToneasSpeech" & fOnsetE=="early")$Shorter,filter(aovmeans_clean2,Explabel=="ToneasSpeech" & fOnsetE=="late")$Shorter,paired=T)
p.adjust(p[["p.value"]], method = "bonferroni", n = 3)

p = t.test(filter(aovmeans_clean2,Explabel=="ToneasSpeech" & fOnsetE=="ontime")$Shorter,filter(aovmeans_clean2,Explabel=="ToneasSpeech" & fOnsetE=="late")$Shorter,paired=T)
p
cohen.d(filter(aovmeans_clean2,Explabel=="ToneasSpeech" & fOnsetE=="ontime")$Shorter,filter(aovmeans_clean2,Explabel=="ToneasSpeech" & fOnsetE=="late")$Shorter,paired=T)
p.adjust(p[["p.value"]], method = "bonferroni", n = 3)

#############################################################################################################################
############################ Implement ANOVA on 50% point: Onset Times x Auditory Targets ###################################
#############################################################################################################################
m = summary(aov(fifty~fOnsetE*Explabel+Error(sub_id/fOnsetE),data=aovmeans_clean2)) 
m$'Error: sub_id'[[1]]$`Sum Sq`[1]/(m$'Error: sub_id'[[1]]$`Sum Sq`[1]+m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]) # Target
m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[1]/(m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[1]+m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]) # Onset
m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[2]/(m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[2]+m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]+m$'Error: sub_id'[[1]]$`Sum Sq`[1]) # Onset*Target

m = summary(aov(fifty~fOnsetE*Explabel+Error(sub_id/fOnsetE),data=filter(aovmeans_clean2,Explabel!="ToneasSpeech"))) 
m$'Error: sub_id'[[1]]$`Sum Sq`[1]/(m$'Error: sub_id'[[1]]$`Sum Sq`[1]+m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]) # Target
m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[1]/(m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[1]+m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]) # Onset
m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[2]/(m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[2]+m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]+m$'Error: sub_id'[[1]]$`Sum Sq`[1]) # Onset*Target

m = summary(aov(fifty~fOnsetE*Explabel+Error(sub_id/fOnsetE),data=filter(aovmeans_clean2,Explabel!="Speech"))) 
m$'Error: sub_id'[[1]]$`Sum Sq`[1]/(m$'Error: sub_id'[[1]]$`Sum Sq`[1]+m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]) # Target
m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[1]/(m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[1]+m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]) # Onset
m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[2]/(m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[2]+m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]+m$'Error: sub_id'[[1]]$`Sum Sq`[1]) # Onset*Target

m = summary(aov(fifty~fOnsetE+Error(sub_id/fOnsetE),data=filter(aovmeans_clean2,Explabel=="Speech"))) 
m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[1]/(sum(m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`)+m$'Error: sub_id'[[1]]$`Sum Sq`[1]) # calculate partial generalized eta sq
m = summary(aov(fifty~fOnsetE+Error(sub_id/fOnsetE),data=filter(aovmeans_clean2,Explabel=="Tones"))) 
m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[1]/(sum(m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`)+m$'Error: sub_id'[[1]]$`Sum Sq`[1]) # calculate partial generalized eta sq
m = summary(aov(fifty~fOnsetE+Error(sub_id/fOnsetE),data=filter(aovmeans_clean2,Explabel=="ToneasSpeech"))) 
m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[1]/(sum(m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`)+m$'Error: sub_id'[[1]]$`Sum Sq`[1]) # calculate partial generalized eta sq

m = summary(aov(fifty~fOnsetE*Explabel+Error(sub_id/fOnsetE),data=filter(aovmeans_clean2,Explabel!="Tones")))
m$'Error: sub_id'[[1]]$`Sum Sq`[1]/(m$'Error: sub_id'[[1]]$`Sum Sq`[1]+m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]) # Target
m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[1]/(m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[1]+m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]) # Onset
m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[2]/(m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[2]+m$'Error: sub_id:fOnsetE'[[1]]$`Sum Sq`[3]+m$'Error: sub_id'[[1]]$`Sum Sq`[2]+m$'Error: sub_id'[[1]]$`Sum Sq`[1]) # Onset*Target

## Implement pairwise ttests between Onset Times (Early vs. Ontime, Early vs. Late, Ontime vs. Late) on each of the Auditory Targets (Speech, Tone, ToneasSpeech)
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

# ToneasSpeech
p = t.test(filter(aovmeans_clean2,Explabel=="ToneasSpeech" & fOnsetE=="early")$fifty,filter(aovmeans_clean2,Explabel=="ToneasSpeech" & fOnsetE=="ontime")$fifty,paired=T)
p
cohen.d(filter(aovmeans_clean2,Explabel=="ToneasSpeech" & fOnsetE=="early")$fifty,filter(aovmeans_clean2,Explabel=="ToneasSpeech" & fOnsetE=="ontime")$fifty,paired=T)
p.adjust(p[["p.value"]], method = "bonferroni", n = 3)

p = t.test(filter(aovmeans_clean2,Explabel=="ToneasSpeech" & fOnsetE=="early")$fifty,filter(aovmeans_clean2,Explabel=="ToneasSpeech" & fOnsetE=="late")$fifty,paired=T)
p
cohen.d(filter(aovmeans_clean2,Explabel=="ToneasSpeech" & fOnsetE=="early")$fifty,filter(aovmeans_clean2,Explabel=="ToneasSpeech" & fOnsetE=="late")$fifty,paired=T)
p.adjust(p[["p.value"]], method = "bonferroni", n = 3)

p = t.test(filter(aovmeans_clean2,Explabel=="ToneasSpeech" & fOnsetE=="ontime")$fifty,filter(aovmeans_clean2,Explabel=="ToneasSpeech" & fOnsetE=="late")$fifty,paired=T)
p
cohen.d(filter(aovmeans_clean2,Explabel=="ToneasSpeech" & fOnsetE=="ontime")$fifty,filter(aovmeans_clean2,Explabel=="ToneasSpeech" & fOnsetE=="late")$fifty,paired=T)
p.adjust(p[["p.value"]], method = "bonferroni", n = 3)
