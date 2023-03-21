library(lme4)
library(lmerTest)
library(tidyverse)
library(broom)
library(effsize)
library('fastDummies')

## Load the data

#### Zoe notes to Zoe 
# Sarah's codes are as follows
# glueing datasets together from EXP4c and EXP6
# lm
# plotting 
# aov 

#### Sarah notes to Zoe
# need to resolve singular fit problems
# some convergence issues might be participants with poor fits
# maybe better to code fixed effects of onset time as Helmert (early vs other, ontime vs late??)
# I tried early as reference level since it is the most different/should be
# most of effects in speech are the really long ones, and LATE vs. ontime condition, which is weird
# would be nice to talk to an expert in these models
# I've tried looking at the random effects and random effect SDs, not super helpful?

# mydata = read.csv("/Users/t.z.cheng/Google_Drive/Research/Delaydoesmatter/real_exp/exp2/data_analysis/Rdata/exp2_PPS_n48.csv") # exp2
# mydata = read.csv("/Users/t.z.cheng/Google_Drive/Research/Delaydoesmatter/real_exp/exp3_20CR11/results/EXP3_clean_n34.csv") # exp3
# mydata = read.csv("/Users/t.z.cheng/Google_Drive/Research/Delaydoesmatter/real_exp/exp4_20CR12/4ab/results_shortdelay_2020/EXP4a_clean_n53.csv") # exp4a
# mydata = read.csv("/Users/t.z.cheng/Google_Drive/Research/Delaydoesmatter/real_exp/exp4_20CR12/4ab/results_shortlongdelay_2021/EXP4b_clean_n67.csv") # exp4b
mydata = read.csv("/Users/t.z.cheng/Google_Drive/Research/cross_domain_entrainment/Delaydoesmatter/real_exp/exp4_20CR12/4c/results/EXP4c_clean_n59.csv") # exp4c
# mydata = read.csv("/Users/t.z.cheng/Google_Drive/Research/Delaydoesmatter/real_exp/exp5_21CR01/results/EXP5_clean_n24_e.csv") # exp5 empty
# mydata = read.csv("/Users/t.z.cheng/Google_Drive/Research/Delaydoesmatter/real_exp/exp5_21CR01/results/EXP5_clean_n24_f.csv") # exp5 filled
mydata = read.csv("/Users/t.z.cheng/Google_Drive/Research/cross_domain_entrainment/exp6_21CR03_Vowel_length/FF2021/results/EXP6_clean_n64.csv") # exp6 VL


##### SARAH==to glue together both datasets
# mydataExp6=mydata

str(mydata) #data inspection, you should see that all the variables are "int" they need to be centered and scaled

############################ standardize the input to the model ###########################
## may need to change some variable name

# mydata = mydata %>%
#  mutate(Onset =case_when(Onset == 5 ~ "Early", Onset == 8 ~ "Ontime", Onset == 9 ~ "Late"))
# mydata = mydata %>%
  # mutate(nresponse_value =case_when(response_value == "Longer" ~ 0, response_value == "Shorter" ~1))
mydata = mydata %>% #filter(Length<6) %>%
  mutate(nresponse_value = Shorter)

## Rescale and re-reference
mydata = mydata %>%
  mutate(rLength = scale(comparison, center = TRUE, scale = TRUE)) # scale the steps ## 4c: scale Length; 6: scale comparison
mydata = mydata %>%
  mutate(fOnset = as.factor(Onset))

mydata$fOnsetR = relevel(mydata$fOnset, ref="ontime") # make ontime condition the reference 
mydata$fOnsetE = relevel(mydata$fOnset, ref="early") # make ontime condition the reference 
#mydataExp6$fOnsetE = relevel(mydataExp6$fOnset, ref="early") # make ontime condition the reference 
### DO ALL FOR EXP6 THEN BACK TO TOP TO PROCESS EXP4C
mydataEXP4 = mydata
mydataEXP6 = mydata

## flag the overlapping subjects
overlapsubj = intersect(mydataEXP4$sub_id,mydataEXP6$sub_id)
length(overlapsubj)
mydataEXP4$overlap = ifelse(Reduce(`|`, lapply(overlapsubj, `==`, mydataEXP4$sub_id)),1,0)
mydataEXP6$overlap = ifelse(Reduce(`|`, lapply(overlapsubj, `==`, mydataEXP6$sub_id)),1,0)

# apparently mutating removes the original variable
alldata=rbind(select(mydataEXP4,participant_id,sub_id,expt_id,fOnsetR,fOnsetE,rLength,overlap,Shorter),select(mydataEXP6,participant_id,sub_id,expt_id,fOnsetR,fOnsetE,rLength,overlap,Shorter))
alldata$Exptag=ifelse(alldata$expt_id=="620ddeee49655b42f1242551",1,-1)
alldata$Explabel=ifelse(alldata$expt_id=="620ddeee49655b42f1242551","Tones","Speech")
alldata$OnsetNum=ifelse(alldata$fOnsetR=="early",-1,ifelse(alldata$fOnsetR=="ontime",0,1))
alldata = alldata %>%
  filter(alldata$overlap == 0) %>%
  droplevels

############################ glm model with onset coded as Early, ontime, late run for each experiment ###########################

# very flat slopes, maybe too hard to estimate? I eyeballed these
badsubsExp4c=c("6222b","8be33","a4432","bf5f1","ce0e9") # when I throw them out the model converges
badsubsExp6=c("421ca","488cc","5d964","6933d","97e84","c3ee1") # but doesn't help for Exp6
## do subject fit SEs indicate poor fit?
allbadsubs=c(badsubsExp4c,badsubsExp6)

allbadsubs=c("394a9","5a2fc","6222b","6ae89","8be33","a4432","bf5f1","ce0e9","16b53","421ca","488cc","5d964","6933d","97e84","c3ee1") # by Zoe
alldata$badsubs=ifelse(alldata$sub_id %in% allbadsubs,"badsub","ok")

#for Exp4c: removing flat slope Ss gives okay fit apparently -> not realy by Zoe, remove the interaction in random effect fix the singular
lm = glmer(Shorter ~ fOnsetR*rLength + (1 + fOnsetR + rLength + fOnsetR:rLength|sub_id),data= filter(alldata,Explabel=="Tones"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
# for Exp6: removing flat slope Ss doesn't improve fit as far as I can tell, remove the interaction in random effect fix the singular by Zoe
lm = glmer(Shorter ~ fOnsetR*rLength + (1 + fOnsetR + rLength + fOnsetR:rLength|sub_id),data= filter(alldata,Explabel=="Speech"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
summary(lm) # could use estimate to compare "entrainment effect" across experiments

# both studies lm. With Onset coded as numeric, it converges. 3-way interaction, though no Exp * OnsetNum interaction
lmall = glmer(Shorter ~ Exptag*fOnsetE*rLength + (1 + fOnsetE*rLength|sub_id),data= filter(alldata,badsubs =="ok"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
summary(lmall) # Use early as the reference
lmall = glmer(Shorter ~ Exptag*fOnsetR*rLength + (1 + fOnsetR + rLength|sub_id),data= filter(alldata,badsubs =="ok"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
summary(lmall) # Use ontime as the reference

plotty1=as.data.frame(fixef(lmall))
# try to plot random effects

# in the dataframe view, the subjects effects are, I think, centered on zero
plotty2=as.data.frame(ranef(lmall)) ## have to do as data frame to easily access effects
plot(filter(plotty2,term=="(Intercept)")$condval)

summary(plotty2$term)

# # limit to early & ontime so can do binary effect
# mydataEO=mydata %>% filter(fOnsetR!="ontime")
# mydataEO$numericfOnsetR=scale(as.numeric(mydataEO$fOnsetR))
#   EOmodel=glmer(nresponse_value ~ numericfOnsetR*rLength + ( + numericfOnsetR+rLength|participant_id),data= mydataEO,family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
# summary(EOmodel)
  # why the heck aren't any of these converging? makes no sense, tons of data


################################# SARAH doing some plots
library(ggplot2)

mydata2plot1=alldata %>% group_by(rLength,fOnsetR,sub_id,expt_id,Explabel) %>% summarise(Shorter=mean(Shorter))
mydata2plot2 = mydata2plot1 %>%  group_by(rLength,fOnsetR,expt_id,Explabel) %>% summarise(ShorterM=mean(Shorter),SD=sd(Shorter),Nsubs=n_distinct(sub_id))

# Calculate the slope
all_data_slope <- mydata2plot1 %>%  
  group_by(fOnsetR,sub_id,Explabel) %>%   
  do(tidy(glm(Shorter ~ rLength, data = .,family="binomial"))) %>%   
  filter(term == "rLength") %>%    
  select(fOnsetR,sub_id,Explabel, estimate) %>%   
  rename(slope = estimate)

# plots of sigmoids. flipped 'ShorterM' (mean of Shorter) so it increases L to R. % long.
ggplot(filter(mydata2plot2,fOnsetR!="ontime"),aes(x=rLength,y=ShorterM,color=fOnsetR,linetype=Explabel,group=interaction(fOnsetR,Explabel)))+
  theme_classic() +
  labs(y="Proportion shorter")+
  scale_color_manual(values=c("blue","lightblue","darkblue"))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=ShorterM-SD/sqrt(Nsubs),ymax=ShorterM+SD/sqrt(Nsubs)),width=0)#+
  #facet_grid(.~Explabel)
ggsave("speechvstones_earlyvslate.png",width=5,height=4)

# individual ppts
ggplot(mydata2plot1,aes(x=rLength,y=Shorter,color=fOnsetR,linetype=Explabel))+
  scale_color_manual(values=c("red","blue","gray"))+
#  geom_point()+
  # geom_line()+
  #geom_smooth(method="lm",formula=y ~ exp(x)/(1+exp(x)),se=FALSE)+
  #geom_smooth(method="lm",se=FALSE) +
  geom_smooth(method = glm, method.args= list(family="binomial"),se=FALSE) + # lots of warnings
  facet_wrap(sub_id~.)
