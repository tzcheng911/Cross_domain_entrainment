library(lme4)
library(lmerTest)
library(tidyverse)
library(broom)
library(ggplot2)
library(ggpubr)
library('fastDummies')
library(effsize)
library(lsr)
library(stringr)

## Load the data
EXP = read.csv("/Users/t.z.cheng/Documents/GitHub/Cross_domain_entrainment/exp9ab/p-center/results/pilot_pcenter_results.csv") 

## Preprocessing
# Filter the main task
# Coding response to 0: out of time, and 1: in time
# mutate the conditions: stimuli (ad/at, lab/lap, tone/tone1), delay (metronome is 0,30,60,90,120 ms behind the sound), len (short 1, long 8)
mainEXP = filter(EXP, trial_template == "maintaskTrials") # select the main trials after 8 examples
length(unique(EXP$participant_id)) # how many subjects
mainEXP$response_value=ifelse(mainEXP$response_value=="Out of time",0,1)
mainEXP = mainEXP %>%
  mutate(stimuli = str_split(as.character(mainEXP$stimuli_presented),"[0-9]+",simplify = T)[,1])
mainEXP = mainEXP %>%
  mutate(delay = str_split(as.character(mainEXP$stimuli_presented),"_",simplify = T)[,2])
mainEXP = mainEXP %>%
  mutate(len = str_sub(str_split(as.character(mainEXP$stimuli_presented),"_",simplify = T)[,1],-1))
mainEXP$delay = factor(mainEXP$delay, levels = c("0","30","60","90","120"))

## Analysis
mainEXPmeans_subj=mainEXP %>% 
  group_by(stimuli,delay, participant_id) %>%
  summarize(meanResp = mean(response_value))

mainEXPmeans = mainEXPmeans_subj %>%
  group_by(stimuli,delay) %>%
  summarize(mean = mean(meanResp), SD = sd(meanResp))

length(unique(mainEXP$participant_id))

## Visualization
ggplot(mainEXPmeans,aes(x=delay,y=mean,color=stimuli, group=interaction(stimuli)))+
  geom_line(position=position_dodge(width=0.1))+
  geom_point(position=position_dodge(width=0.1))+
  theme_bw() +
  ylim(0,1) + 
  scale_colour_grey()+
  geom_errorbar(aes(ymin=mean-SD/sqrt(12),ymax=mean+SD/sqrt(12)),width=0,position=position_dodge(width=0.1))
                