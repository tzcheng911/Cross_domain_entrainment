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
  group_by(fOnsetE,Explabel,fOnset,Length,sub_id) %>% summarise(Shorter=mean(Shorter)) # Important!! Use Length for plotting (Length 1-8, instead of the scaled Length -1.52 - 1.52 for easier visualization), use rLength for fitting model & ANOVA test

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
aovdata_clean_plot = aovdata_clean %>% group_by(Length,fOnsetE,Explabel) %>% summarise(mShorter=mean(Shorter),SD=sd(Shorter),Nsubs=n_distinct(sub_id)) # Important!! Use Length for plotting (Length 1-8, instead of the scaled Length -1.52 - 1.52 for easier visualization), use rLength for fitting model & ANOVA test
aovdata_clean_plot$fOnsetE = factor(aovdata_clean_plot$fOnsetE, levels = c("early","ontime","late"))
aovdata_clean$Explabel = factor(aovdata_clean$Explabel, levels = c("EXP10a","EXP10b"))
aovdata_outlier_slope$Explabel = factor(aovdata_outlier_slope$Explabel, levels = c("EXP10a","EXP10b"))
aovdata_outlier_50$Explabel = factor(aovdata_outlier_50$Explabel, levels = c("EXP10a","EXP10b"))
aovdata_clean_plot$Explabel = factor(aovdata_clean_plot$Explabel, levels = c("EXP10a","EXP10b"))

# Plot averaged curves of the clean data
ggplot(aovdata_clean_plot,aes(x=Length,y=mShorter,color=fOnsetE,linetype=fOnsetE,group=interaction(fOnsetE,Explabel)))+
  geom_point(show.legend = FALSE)+
  scale_x_continuous(breaks = seq(1, 8, by = 1))+
  geom_line(linewidth = 1)+
  geom_errorbar(position=position_dodge(width=0.1),linewidth = 2,linetype='solid',aes(ymin=mShorter-SD/sqrt(Nsubs),ymax=mShorter+SD/sqrt(Nsubs)),width=0)+
  scale_linetype_manual(
    values = c("early"="dotted","ontime"="solid","late"="dashed"),
    labels = c("Early", "On-time", "Late"),
    name = "Onset Times",
    guide = "none")+
  scale_color_manual(
    values = c(
      "early"  = "#D55E00",  # red
      "ontime" = "#009E73",  # green
      "late"   = "#0072B2"),   # blue
    labels = c("Early", "On-time", "Late"),
    name = "Onset Times")+
  labs(
    x = "Target duration (step)",
    y = "Proportion of responding short") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2))+
  facet_grid(.~Explabel, labeller = as_labeller(c(
    "EXP10a" = "Speech",
    "EXP10b" = "Tone"
  )))  + 
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.title = element_text(size = 18),    # legend title font size
        legend.text = element_text(size = 18),     # legend item font size
        axis.title = element_text(size = 18),      # axis label font size
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 18)# axis tick label font size
  )

ggsave( 
  "EXP3_curve_11252025.pdf", 
  width = 8, # The desired width of the plot
  height = 6, # The desired height of the plot
  units = "in", # The units for width and height (can be "in", "cm", "mm", or "px")
)

# Plot individual curves of the clean data
ggplot(aovdata_clean,aes(x=Length,y=Shorter,color=fOnsetE,shape=Explabel))+
  scale_color_manual(
    values=c("early"  = "#D55E00",  # red
             "ontime" = "#009E73",  # green
             "late"   = "#0072B2"),   # blue
    labels = c("Early", "On-time", "Late"),
    name = "Onset Times" 
  )+
  geom_point()+
  # geom_line()+
  # geom_smooth(method="lm",formula=y ~ exp(x)/(1+exp(x)),se=FALSE)+
  # geom_smooth(method="lm",se=FALSE) +
  geom_smooth(method="glm",method.args = list(family = "binomial"),se=FALSE) +
  facet_wrap(sub_id~.) +
  theme(strip.text.x = element_blank())+
  theme(legend.position = "bottom")

## Relabel Exp10ab to Speech and Tone
aovmeans_clean2$Explabel = ifelse(aovmeans_clean2$Explabel == "EXP10a","Speech","Tone")
aovmeans_clean2$Explabel = factor(aovmeans_clean2$Explabel, levels = c("Speech","Tone"))
aovmeans_clean2$fOnsetE = factor(aovmeans_clean2$fOnsetE, levels = c("early","ontime","late"))

## Plot the bar plot: use the Length instead of rLength so the 50% point is more interpretable (Line 32, 37, 77)
ggplot(aovmeans_clean2, aes(x = Explabel, y = Shorter, fill = fOnsetE)) +
  geom_bar(stat="summary", fun = "mean", position='dodge') +
  stat_summary(fun.data=mean_se, geom="errorbar", position = position_dodge(width = 0.9), width=.1,linewidth = 2,color="#808080") +
  scale_fill_manual(name = "Onset Times",
                    values = c(
                      "early"  = "#D55E00",  # red
                      "ontime" = "#009E73",  # green
                      "late"   = "#0072B2"),   # blue
                    labels = c("Early", "On-time", "Late"))+
  ylim(0,0.8) + 
  geom_point(position = position_jitterdodge(jitter.width = 0.3,dodge.width = 0.9), color="black")+
  labs(
    x = "Auditory target",      
    y = "Proportion Responding Short" 
  ) +
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.title = element_text(size = 18),    # legend title font size
        legend.text = element_text(size = 18),     # legend item font size
        axis.title = element_text(size = 18),      # axis label font size
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 18)# axis tick label font size
  )
ggsave( 
  "EXP3_barpps_11282025.pdf", 
  width = 8, # The desired width of the plot
  height = 6, # The desired height of the plot
  units = "in", # The units for width and height (can be "in", "cm", "mm", or "px")
)

ggplot(aovmeans_clean2, aes(x = Explabel, y = fifty, fill = fOnsetE)) +
  geom_bar(stat="summary", fun = "mean", position='dodge') +
  stat_summary(fun.data=mean_se, geom="errorbar", position = position_dodge(width = 0.9), width=.1,linewidth = 2,color="#808080") +
  scale_fill_manual(name = "Onset Timing",
                    values = c(
                      "early"  = "#D55E00",  # red
                      "ontime" = "#009E73",  # green
                      "late"   = "#0072B2"),   # blue
                    labels = c("Early", "On-time", "Late"))+
  scale_fill_manual(name = "Onset Times",
                    values = c(
                      "early"  = "#D55E00",  # red
                      "ontime" = "#009E73",  # green
                      "late"   = "#0072B2"),   # blue
                    labels = c("Early", "On-time", "Late"))+
  ylim(0,8) + 
  geom_point(position = position_jitterdodge(jitter.width = 0.3,dodge.width = 0.9), color="black")+
  labs(
    x = "Auditory target",      
    y = "50% point" 
  ) +
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.title = element_text(size = 18),    # legend title font size
        legend.text = element_text(size = 18),     # legend item font size
        axis.title = element_text(size = 18),      # axis label font size
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 18)# axis tick label font size
  )
ggsave( 
  "EXP3_bar50_11282025.pdf", 
  width = 8, # The desired width of the plot
  height = 6, # The desired height of the plot
  units = "in", # The units for width and height (can be "in", "cm", "mm", or "px")
)

## Get final sample size after excluding the outliers
EXP10a = filter(aovmeans_clean2,Explabel == "Speech")
EXP10b = filter(aovmeans_clean2,Explabel == "Tones")
length(unique(EXP10a$sub_id))
length(unique(EXP10b$sub_id))

######################################################## Statistical test ######################################################## 

#############################################################################################################################
############################################## Run logistic mixed-effect model ##############################################
#############################################################################################################################
alldata_clean = filter(alldata, sub_id %in% unique(aovmeans_clean2$sub_id)) # change to aovmeans_clean1 to include 50% point outliers 

## Coding the contrast
# Contrast the 3 Onset Times (early ontime late) by simple coding: comparing early vs ontime, early vs late, but centered (-1/3,2/3,-1/3; -1/3,-1/3, 2/3)
alldata_clean = alldata_clean %>%
  mutate(earlyVSontime=ifelse(fOnsetE=="ontime",2/3,-1/3),
         earlyVSlate=ifelse(fOnsetE=="late",2/3,-1/3), # note that early is always -1/3, so it's reference level
         speechVStone=ifelse(exp=="EXP10a",-0.5,0.5), # 10a = speech
  ) 
## Check the coding
alldata_clean %>% group_by(speechVStone,earlyVSontime,earlyVSlate) %>% summarise(n())

## Use BUILDMER package to figure out which random fx to drop https://cran.r-project.org/web/packages/buildmer/vignettes/buildmer.html
library(buildmer)
fmla=Shorter ~ (speechVStone)*(earlyVSontime+earlyVSlate)*rLength  + 
  (1 + (earlyVSontime+earlyVSlate)*rLength|sub_id)
m <- buildmer(fmla,data=alldata_clean,
              family="binomial",
              buildmerControl=buildmerControl(direction='order',
                                              args=list(control=glmerControl(optimizer='bobyqa'))))
summary(m) # random fx retained:      (1 + rLength + earlyVSlate | sub_id)

## Full model
lm = glmer(Shorter ~ speechVStone*(earlyVSontime+earlyVSlate)*rLength  + 
                   (1 + earlyVSlate + rLength|sub_id),
                 data= alldata_clean,family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
summary(lm)

## Test the omnibus Auditory Targets x Onset Times 2-way interaction 2-way interaction  
lm_nointeraction = glmer(Shorter ~ speechVStone*(earlyVSontime+earlyVSlate)*rLength  -
                                 speechVStone:(earlyVSontime+earlyVSlate) + 
                                 (1 + earlyVSlate + rLength|sub_id),
                               data= alldata_clean,family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  

anova(lm,lm_nointeraction)      

## Test the omnibus Auditory Targets x Onset Times x Target Durations 3-way interaction  
lm_no3wayinteraction = glmer(Shorter ~ speechVStone*(earlyVSontime+earlyVSlate)*rLength  -
                                     speechVStone:(earlyVSontime+earlyVSlate):rLength + # 3 way interaction 
                                     (1 + earlyVSlate + rLength | sub_id),
                                   data= alldata_clean,family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
anova(lm,lm_no3wayinteraction) 

## Test the Target Duration
lm_noLength = glmer(Shorter ~ speechVStone*(earlyVSontime+earlyVSlate)*rLength  -
                          rLength + 
                          (1 + earlyVSlate + rLength|sub_id),
                        data= alldata_clean,family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
anova(lm,lm_noLength)

## Test the Onset Times effect in Speech condition
lmSp=glmer(Shorter ~ (earlyVSontime+earlyVSlate)*rLength  + 
             (1 + earlyVSlate + rLength|sub_id), # note that Sarah used addition instead of interaction in the original model
           data= filter(alldata_clean,exp=="EXP10a"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
summary(lmSp)
lmSpNoOT=glmer(Shorter ~ (earlyVSontime+earlyVSlate)*rLength  -
                 (earlyVSontime+earlyVSlate) + 
                 (1 + earlyVSlate + rLength|sub_id),
               data= filter(alldata_clean,exp=="EXP10a"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
anova(lmSp,lmSpNoOT)

## Test the Onset Times effect in Tone condition
lmTn=glmer(Shorter ~ (earlyVSontime+earlyVSlate)*rLength  + 
             (1 + earlyVSlate + rLength|sub_id),
           data= filter(alldata_clean,exp=="EXP10b"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
summary(lmTn)
lmTnNoOT=glmer(Shorter ~ (earlyVSontime+earlyVSlate)*rLength  -
                 (earlyVSontime+earlyVSlate) + 
                 (1 + earlyVSlate + rLength|sub_id),
               data= filter(alldata_clean,exp=="EXP10b"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
anova(lmTn,lmTnNoOT)

#############################################################################################################################
############################ Implement ANOVA on proportion short: Onset Times x Auditory Targets ############################
#############################################################################################################################
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

#############################################################################################################################
############################ Implement ANOVA on 50% point: Onset Times x Auditory Targets ###################################
#############################################################################################################################
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
