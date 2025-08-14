library(lme4)
library(lmerTest)
library(tidyverse)
library(broom)
library(ggplot2)
library(ggpubr)
library(fastDummies)
library(effsize)
library(lsr)

# SARAH
setwd("~/Documents/current studies/A0 priority/Zoe speech study 2 (FindingFive version)/Zoe code 7-15-2025")
# /SARAH

######################################################## Preprocessing ######################################################## 
## Load the data
EXPspeech = read.csv("EXP1a_clean_n80.csv")
EXPtone = read.csv("EXP1b_clean_n84.csv") 
EXPtoneasspeech = read.csv("EXP1c_clean_n88.csv") 

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
  group_by(fOnsetE,Explabel,rLength,sub_id) %>% summarise(Shorter=mean(Shorter)) # change rLength to Length for visualization

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
# 'af90a' press the same button across all experiment 
aovmeans$outliers_slope = ifelse(aovmeans$slope>= 0 | aovmeans$sub_id == 'af90a',1,0)
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
aovdata_clean_plot = aovdata_clean %>% group_by(rLength,fOnsetE,Explabel) %>% summarise(mShorter=mean(Shorter),SD=sd(Shorter),Nsubs=n_distinct(sub_id))
aovdata_clean_plot$fOnsetE = factor(aovdata_clean_plot$fOnsetE, levels = c("early","ontime","late"))
aovdata_clean_plot$Explabel = factor(aovdata_clean_plot$Explabel, levels = c("EXP8a","EXP8b","EXP8c"))

ggplot(aovdata_clean_plot,aes(x=rLength,y=mShorter,color=fOnsetE,linetype=Explabel,group=interaction(fOnsetE,Explabel)))+
  geom_point()+
  scale_x_continuous(breaks = seq(1, 8, by = 1))+
  geom_line()+
  geom_errorbar(aes(ymin=mShorter-SD/sqrt(Nsubs),ymax=mShorter+SD/sqrt(Nsubs)),width=0)+
  facet_grid(Explabel~.)+
  theme_bw()

# Plot individual curves of the clean data
# ggplot(aovdata_clean,aes(x=rLength,y=Shorter,color=fOnsetE,shape=Explabel))+
#   scale_color_manual(values=c("red","green","blue"))+
#   geom_point()+
#   # geom_line()+
#   #  geom_smooth(method="lm",formula=y ~ exp(x)/(1+exp(x)),se=FALSE)+
#    geom_smooth(method="lm",se=FALSE) +
#   # geom_smooth(method="glm",method.args = list(family = "binomial"),se=FALSE) +
#   facet_wrap(sub_id~.) +
#   theme(strip.text.x = element_blank())

## Relabel Exp8abc to Speech, Tone and ToneasSpeech
aovmeans_clean2$Explabel = ifelse(aovmeans_clean2$Explabel == "EXP8a","Speech",ifelse(aovmeans_clean2$Explabel == "EXP8b","Tones","ToneasSpeech"))
aovmeans_clean2$Explabel = factor(aovmeans_clean2$Explabel, levels = c("Speech","Tones","ToneasSpeech"))
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
## Run logistic mixed-effect model 
alldata_clean_allEXPlabel = filter(alldata, sub_id %in% unique(aovmeans_clean2$sub_id)) 

## For contrasting the 3 conditions:
# I used Helmert coding, comparing 
# speech vs other conditions,
# and tone vs toneasspeech
#
## For early ontime late:
# CURRENT: SIMPLE CODING: early vs ontime, early vs late, but centered (-1/3,2/3,-1/3; -1/3,-1/3, 2/3)
# PREVIOUS: HELMERT comparing early to others, and ontime to late
# Other coding options for early ontime late
# FORWARD DIFF CODING: 2 vars to contrast 3 successive levels
# Likely doesn't make much difference but SIMPLE is most similar to Zoe approach BUT centered at grand mean

# SARAH recoding some things
alldata_clean_allEXPlabel = alldata_clean_allEXPlabel %>%
  # mutate(earlyVSothers=ifelse(fOnsetE=="early",2/3,-1/3),
  #        ontimelate=ifelse(fOnsetE=="early",0,
  #                          ifelse(fOnsetE=="ontime",-.5,.5)),
  mutate(earlyVSontime=ifelse(fOnsetE=="ontime",2/3,-1/3),
         earlyVSlate=ifelse(fOnsetE=="late",2/3,-1/3), # note that early is always -1/3, so it's reference level
         speechVSothers=ifelse(exp=="EXP8a",2/3,-1/3), # 8a = speech
           toneVStoneasspeech=ifelse(exp=="EXP8b",.5,
                                     ifelse(exp=="EXP8c",-.5,0))) # 8b = tone, 8c = tas
# check the recoding
alldata_clean_allEXPlabel %>% group_by(speechVSothers,toneVStoneasspeech,
                                       earlyVSontime,earlyVSlate) %>% summarise(n())

        # #### try BUILDMER package to figure out which random fx to drop
        # # specify maximal formula
        # # it's then supposed to find the maximal model that will actually converge
        library(buildmer)
        # # https://cran.r-project.org/web/packages/buildmer/vignettes/buildmer.html
        fmla=Shorter ~ (speechVSothers+toneVStoneasspeech)*(earlyVSontime+earlyVSlate)*rLength  + 
          (1 + (earlyVSontime+earlyVSlate)*rLength|sub_id)
        # 
        # # all the output shows up as red text and looks like errors
        m <- buildmer(fmla,data=alldata_clean_allEXPlabel,
                      family="binomial",
                      buildmerControl=buildmerControl(direction='order',
                                                      args=list(control=glmerControl(optimizer='bobyqa'))))
        # started at 1:05pm....ended 1:33. not terrible.
        summary(m) # random fx retained:     (1 + rLength + earlyVSlate + earlyVSontime | sub_id)

# buildmer says drop all but the single rand fx slopes (no ranfx interactions)
lm_Sarah = glmer(Shorter ~ (speechVSothers+toneVStoneasspeech)*(earlyVSontime+earlyVSlate)*rLength  + 
                   (1 + earlyVSontime+earlyVSlate+rLength|sub_id),
                 data= alldata_clean_allEXPlabel,family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
summary(lm_Sarah)
# Number of obs: 55008, groups:  sub_id, 191
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                              -0.711083   0.050213 -14.161  < 2e-16 ***
# speechVSothers                           -0.928733   0.107765  -8.618  < 2e-16 ***
# toneVStoneasspeech                        0.109545   0.120848   0.906  0.36469    
# earlyVSontime                            -0.159196   0.034213  -4.653 3.27e-06 ***
# earlyVSlate                              -0.237814   0.035422  -6.714 1.90e-11 ***
# rLength                                  -1.798385   0.057427 -31.316  < 2e-16 ***
# speechVSothers:earlyVSontime              0.113048   0.075818   1.491  0.13595    
# speechVSothers:earlyVSlate                0.230825   0.078178   2.953  0.00315 ** <---speech vs others: speech shows different OnsetTime pattern
# toneVStoneasspeech:earlyVSontime         -0.031254   0.075075  -0.416  0.67719    
# toneVStoneasspeech:earlyVSlate           -0.002546   0.078565  -0.032  0.97415    
# speechVSothers:rLength                   -0.724762   0.123185  -5.884 4.02e-09 *** 
# toneVStoneasspeech:rLength               -0.100077   0.137662  -0.727  0.46724    
# earlyVSontime:rLength                     0.050282   0.034239   1.469  0.14195    
# earlyVSlate:rLength                       0.002696   0.034511   0.078  0.93773    
# speechVSothers:earlyVSontime:rLength      0.071388   0.077214   0.925  0.35520    
# speechVSothers:earlyVSlate:rLength        0.116329   0.077627   1.499  0.13399    
# toneVStoneasspeech:earlyVSontime:rLength -0.017795   0.073033  -0.244  0.80749    
# toneVStoneasspeech:earlyVSlate:rLength    0.075009   0.074130   1.012  0.31161    

lm_Sarah_nointeraction = glmer(Shorter ~ (speechVSothers+toneVStoneasspeech)*(earlyVSontime+earlyVSlate)*rLength  -
                                 (speechVSothers+toneVStoneasspeech):(earlyVSontime+earlyVSlate) + # i.e., the targetType x delay interaction 
                                 (1 + rLength + earlyVSlate + earlyVSontime | sub_id),
                               data= alldata_clean_allEXPlabel,family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
anova(lm_Sarah,lm_Sarah_nointeraction)      
# npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)  
# lm_Sarah_nointeraction   24 51150 51364 -25551    51102                       
# lm_Sarah                 28 51149 51399 -25547    51093 8.7708  4    0.06709 .   <-- marginal 'omnibus' 2-way

lm_Sarah_noSimpleinteraction = glmer(Shorter ~ (speechVSothers+toneVStoneasspeech)*(earlyVSontime+earlyVSlate)*rLength  -
                                 (speechVSothers:earlyVSlate) + # i.e., the targetType x delay interaction 
                                 (1 + (earlyVSontime+earlyVSlate+rLength)|sub_id),
                               data= alldata_clean_allEXPlabel,family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
anova(lm_Sarah,lm_Sarah_noSimpleinteraction)      
# npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)   
# lm_Sarah_noSimpleinteraction   27 51156 51396 -25551    51102                        
# lm_Sarah                       28 51149 51399 -25547    51093 8.4443  1   0.003662 ** <-- significant earlyVSlate for speech/others

# just the speech vs. others interaction, but with both onset time contrasts
lm_Sarah_noSpeechTerminteraction = glmer(Shorter ~ (speechVSothers+toneVStoneasspeech)*(earlyVSontime+earlyVSlate)*rLength  -
                                       (speechVSothers:(earlyVSlate + earlyVSontime)) + # i.e., the targetType x delay interaction 
                                       (1 + (earlyVSontime+earlyVSlate+rLength)|sub_id),
                                     data= alldata_clean_allEXPlabel,family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
anova(lm_Sarah,lm_Sarah_noSpeechTerminteraction)      
# npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)  
# lm_Sarah_noSpeechTerminteraction   26 51154 51386 -25551    51102                       
# lm_Sarah                           28 51149 51399 -25547    51093 8.5066  2    0.01422 * <-- full version: speech vs other models, Delay differences


# FOR SIMPLICITY THE SUBMODELS RETAIN THE RANDOM EFFECTS STRUCTURE OF THE MAIN MODEL

alldata_clean_allEXPlabel=alldata_clean_allEXPlabel %>%
  mutate(speechIsHigher=ifelse(exp=="EXP8a",.5,-.5), # works for both speech vs x comparisons
         toneIsHigher=ifelse(exp=="EXP8b",.5,-.5)) # tone vs toneasspeech

# speech vs tone
# GRAH additionally drop earlyVSontime which is smallest variance in holdout model
lmSpTn=glmer(Shorter ~ speechIsHigher*(earlyVSontime+earlyVSlate)*rLength  + 
        (1 + earlyVSlate+rLength|sub_id),
      data= filter(alldata_clean_allEXPlabel,exp!="EXP8c"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
summary(lmSpTn)
# speechIsHigher:earlyVSontime          0.15049    0.07155   2.103 0.035446 *  
# speechIsHigher:earlyVSlate            0.23675    0.07722   3.066 0.002168 ** 
lmSpTnNoInteraction=glmer(Shorter ~ speechIsHigher*(earlyVSontime+earlyVSlate)*rLength  -
               speechIsHigher:(earlyVSontime+earlyVSlate) +
               (1 + earlyVSlate+rLength|sub_id),
             data= filter(alldata_clean_allEXPlabel,exp!="EXP8c"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
summary(lmSpTnNoInteraction)
anova(lmSpTn,lmSpTnNoInteraction)
# npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)   
# lmSpTnNoInteraction   16 34473 34610 -17220    34441                        
# lmSpTn                18 34467 34621 -17215    34431 9.8285  2   0.007341 **

# speech vs toneasspeech
lmSpTas=glmer(Shorter ~ speechIsHigher*(earlyVSontime+earlyVSlate)*rLength  + 
               (1 + earlyVSontime+earlyVSlate+rLength|sub_id),
             data= filter(alldata_clean_allEXPlabel,exp!="EXP8b"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
summary(lmSpTas)
# speechIsHigher:earlyVSontime          0.09251    0.08408   1.100  0.27119    
# speechIsHigher:earlyVSlate            0.23741    0.08288   2.864  0.00418 ** 

lmSpTasNoInteraction=glmer(Shorter ~ speechIsHigher*(earlyVSontime+earlyVSlate)*rLength  -
                            speechIsHigher:(earlyVSontime+earlyVSlate) +
                            (1 + earlyVSontime+earlyVSlate+rLength|sub_id),
                          data= filter(alldata_clean_allEXPlabel,exp!="EXP8b"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
anova(lmSpTas,lmSpTasNoInteraction)
# lmSpTasNoInteraction   20 30585 30754 -15272    30545                       
# lmSpTas                22 30580 30766 -15268    30536 8.1244  2    0.01721 *


# toneasspeech vs tone
lmTnTas=glmer(Shorter ~ toneIsHigher*(earlyVSontime+earlyVSlate)*rLength  + 
                (1 + earlyVSontime+earlyVSlate+rLength|sub_id),
              data= filter(alldata_clean_allEXPlabel,exp!="EXP8a"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
summary(lmTnTas)
# toneIsHigher:earlyVSontime         -0.031138   0.077610  -0.401    0.688    
# toneIsHigher:earlyVSlate           -0.005425   0.084235  -0.064    0.949   

lmTnTasNoInteraction=glmer(Shorter ~ toneIsHigher*(earlyVSontime+earlyVSlate)*rLength  -
                            toneIsHigher:(earlyVSontime+earlyVSlate) +
                            (1 + earlyVSontime+earlyVSlate+rLength|sub_id),
                          data= filter(alldata_clean_allEXPlabel,exp!="EXP8a"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
anova(lmTnTas,lmTnTasNoInteraction)
# lmTnTasNoInteraction   20 37244 37414 -18602    37204                     
# lmTnTas                22 37248 37435 -18602    37204 0.2027  2     0.9036


# speech only
lmSp=glmer(Shorter ~ (earlyVSontime+earlyVSlate)*rLength  + 
               (1 + earlyVSontime+earlyVSlate+rLength|sub_id),
             data= filter(alldata_clean_allEXPlabel,exp=="EXP8a"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
summary(lmSp)
# earlyVSontime         -0.10614    0.06486  -1.636    0.102    
# earlyVSlate           -0.07786    0.06368  -1.223    0.221    
# rLength               -2.33563    0.13093 -17.839   <2e-16 ***

lmSpNoOT=glmer(Shorter ~ (earlyVSontime+earlyVSlate)*rLength  -
                 (earlyVSontime+earlyVSlate) + 
             (1 + earlyVSontime+earlyVSlate+rLength|sub_id),
           data= filter(alldata_clean_allEXPlabel,exp=="EXP8a"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
anova(lmSp,lmSpNoOT)
#          npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)
# lmSpNoOT   14 13875 13984 -6923.6    13847                     
# lmSp       16 13876 14001 -6922.2    13844 2.8528  2     0.2402

lmTn=glmer(Shorter ~ (earlyVSontime+earlyVSlate)*rLength  + 
             (1 + earlyVSontime+earlyVSlate+rLength|sub_id),
           data= filter(alldata_clean_allEXPlabel,exp=="EXP8b"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
summary(lmTn)
# earlyVSontime         -0.202570   0.054027  -3.749 0.000177 ***
# earlyVSlate           -0.324832   0.059479  -5.461 4.73e-08 ***

lmTnNoOT=glmer(Shorter ~ (earlyVSontime+earlyVSlate)*rLength  -
                 (earlyVSontime+earlyVSlate) + 
             (1 + earlyVSontime+earlyVSlate+rLength|sub_id),
           data= filter(alldata_clean_allEXPlabel,exp=="EXP8b"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
anova(lmTn,lmTnNoOT)
# lmTnNoOT   14 20582 20693 -10277    20554                         
# lmTn       16 20561 20688 -10264    20529 25.534  2  2.854e-06 ***

lmTas=glmer(Shorter ~ (earlyVSontime+earlyVSlate)*rLength  + 
             (1 + earlyVSontime+earlyVSlate+rLength|sub_id),
           data= filter(alldata_clean_allEXPlabel,exp=="EXP8c"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
summary(lmTas)
# earlyVSontime         -0.17919    0.05711  -3.138   0.0017 ** 
# earlyVSlate           -0.30776    0.06015  -5.117 3.11e-07 ***
lmTasNoOT=glmer(Shorter ~ (earlyVSontime+earlyVSlate)*rLength  -
                  (earlyVSontime+earlyVSlate) + 
              (1 + earlyVSontime+earlyVSlate+rLength|sub_id),
            data= filter(alldata_clean_allEXPlabel,exp=="EXP8c"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
anova(lmTas,lmTasNoOT)
# lmTasNoOT   14 16719 16827 -8345.4    16691                         
# lmTas       16 16701 16824 -8334.3    16669 22.204  2  1.508e-05 ***


# /SARAH





################################################################################
################################################################################
#################      This is the end of the Sarah stuff      #################
################################################################################
################################################################################
















# Do paired comparison of Tone vs. Speech; Tone vs. ToneasSpeech; Speech vs. ToneasSpeech
# Run the Full and Reduced 2-way models on alldata_cleanGLM to see the interaction
alldata_cleanGLM = filter(alldata_clean_allEXPlabel, Explabel!= "EXP8c") # EXP8a (compare EXP8b vs c), EXP8b (compare EXP8a vs c) or EXP8c (compare EXP8a vs b)
alldata_cleanGLM$Explabel = ifelse(alldata_cleanGLM$exp=="EXP8a",-0.5,0.5) # sum coding for the two conditions being compared, could be EXP8a, EXP8b or EXP8c

# Full model
lmall = glmer(Shorter ~ Explabel*fOnsetE*rLength  + (1 + fOnsetE*rLength|sub_id),data= alldata_cleanGLM,family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
summary(lmall) # Use early as the reference

# Reduce Target Duration (rLength)
lmall_norLength = glmer(Shorter ~ Explabel*fOnsetE*rLength-rLength  + (1 + fOnsetE*rLength|sub_id),data= alldata_clean_allEXPlabel,family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
summary(lmall_norLength) 
anova(lmall,lmall_norLength)

# Reduce 2 way
lmall_no2way = glmer(Shorter ~ Explabel*fOnsetE*rLength-Explabel:fOnsetE  + (1 + fOnsetE*rLength|sub_id),alldata_cleanGLM,family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
summary(lmall_no2way) 
anova(lmall,lmall_no2way)

# Reduce 3 way
lmall_no3way = glmer(Shorter ~ Explabel*fOnsetE*rLength-Explabel:fOnsetE:rLength  + (1 + fOnsetE*rLength|sub_id),data= alldata_clean_allEXPlabel,family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
summary(lmall_no3way) 
anova(lmall,lmall_no3way)

# Submodels
lmall_speech = glmer(Shorter ~ fOnsetE*rLength  + (1 + fOnsetE*rLength|sub_id),data= filter(alldata_clean_allEXPlabel,exp=="EXP8a"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
lmall_speech_noOnset = glmer(Shorter ~ fOnsetE*rLength - fOnsetE  + (1 + fOnsetE*rLength|sub_id),data= filter(alldata_clean_allEXPlabel,exp=="EXP8a"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
lmall_speech_norLength = glmer(Shorter ~ fOnsetE*rLength - rLength  + (1 + fOnsetE*rLength|sub_id),data= filter(alldata_clean_allEXPlabel,exp=="EXP8a"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
lmall_tone = glmer(Shorter ~ fOnsetE*rLength  + (1 + fOnsetE*rLength|sub_id),data= filter(alldata_clean_allEXPlabel,exp=="EXP8b"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
lmall_tone_noOnset = glmer(Shorter ~ fOnsetE*rLength - fOnsetE + (1 + fOnsetE*rLength|sub_id),data= filter(alldata_clean_allEXPlabel,exp=="EXP8b"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
lmall_tone_norLength = glmer(Shorter ~ fOnsetE*rLength - rLength  + (1 + fOnsetE*rLength|sub_id),data= filter(alldata_clean_allEXPlabel,exp=="EXP8b"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
lmall_toneasspeech = glmer(Shorter ~ fOnsetE*rLength  + (1 + fOnsetE*rLength|sub_id),data= filter(alldata_clean_allEXPlabel,exp=="EXP8c"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
lmall_toneasspeech_noOnset = glmer(Shorter ~ fOnsetE*rLength - fOnsetE  + (1 + fOnsetE*rLength|sub_id),data= filter(alldata_clean_allEXPlabel,exp=="EXP8c"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
lmall_toneasspeech_norLength = glmer(Shorter ~ fOnsetE*rLength - rLength  + (1 + fOnsetE*rLength|sub_id),data= filter(alldata_clean_allEXPlabel,exp=="EXP8c"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  

summary(lmall_speech) 
summary(lmall_tone)
summary(lmall_toneasspeech) 
anova(lmall_speech,lmall_speech_noOnset)
anova(lmall_tone,lmall_tone_noOnset)
anova(lmall_toneasspeech,lmall_toneasspeech_noOnset)
anova(lmall_speech,lmall_speech_norLength)
anova(lmall_tone,lmall_tone_norLength)
anova(lmall_toneasspeech,lmall_toneasspeech_norLength)

# Calculate effects based on the confidence interval wald test
confint(lmall_speech, level = 0.95, method = "Wald")
confint(lmall_tone, level = 0.95, method = "Wald")

## Implement ANOVA on proportion short: Onset Times x Auditory Targets
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

## Implement ANOVA on 50% point: Onset Times x Auditory Targets
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
