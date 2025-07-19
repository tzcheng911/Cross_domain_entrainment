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
EXP = read.csv("/Users/tzu-hanzoecheng/Documents/GitHub/Cross_domain_entrainment/exp9ab/p-center/results/pilot_pcenter_results.csv") 
EXP = read.csv("/Users/tzu-hanzoecheng/Documents/GitHub/Cross_domain_entrainment/exp10ab/p-center/results/session-67ef60de6de0902719c5faef-data.csv") 

## Preprocessing
# Filter the main task
# Coding response to 0: out of time, and 1: in time
# mutate the conditions: stimuli (ad/at, tone/tone1), delay (metronome is 0,30,60,90,120 ms behind the sound), len (short 1, long 8)
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

## Visualization for EXP9
## Rename the legend for plotting
mainEXPmeans$stimuli = ifelse(mainEXPmeans$stimuli == "add","at/add",
                              ifelse(mainEXPmeans$stimuli == "lab","lap/lab","short/long tone"))
mainEXPmeans$stimuli = factor(mainEXPmeans$stimuli, levels = c("at/add","lap/lab","short/long tone"))

ggplot(mainEXPmeans,aes(x=delay,y=mean,color=stimuli, linetype = stimuli,group=stimuli))+
  geom_line(position=position_dodge(width=0.1),size = 1.2)+
  geom_point(position=position_dodge(width=0.1),size = 2)+
  labs(x = "Onset Time Difference Between Sounds and Clicks (ms)", y = "Proportion of Responding Beat Aligned")+
  scale_colour_grey(name = "Auditory Targets")+
    scale_linetype_manual(
    name = "Auditory Targets",
    values = c("at/add" = "dashed", "lap/lab" = "dotted","short/long tone" = "solid")
    ) +
  theme_minimal(base_size = 24) +
  ylim(0,1) + 
  geom_errorbar(aes(ymin=mean-SD/sqrt(12),ymax=mean+SD/sqrt(12)),width=0,position=position_dodge(width=0.1))

## Visualization for EXP10
mainEXPmeans$stimuli = ifelse(mainEXPmeans$stimuli == "add","Speech targets",
                              ifelse(mainEXPmeans$stimuli == "tone","Tone targets",
                                     ifelse(mainEXPmeans$stimuli == "addentrainer_","Speech precursors",
                                            "Tone precursors")))
mainEXPmeans$stimuli = factor(mainEXPmeans$stimuli, levels = c("Tone targets","Speech targets","Tone precursors","Speech precursors"))

ggplot(mainEXPmeans,aes(x=delay,y=mean,color=stimuli, linetype = stimuli,group=stimuli))+
  geom_line(position=position_dodge(width=0.1),size = 1.2)+
  geom_point(position=position_dodge(width=0.1),size = 2)+
  labs(x = "Onset Time Difference Between Sounds and Clicks (ms)", y = "Proportion of Responding Beat Aligned")+
  scale_colour_manual(
    name = "Auditory Stimuli",
    values = c("black", "grey","black",  "grey")
  ) +
  scale_linetype_manual(
    name = "Auditory Stimuli",
    values = c("Tone targets" = "solid", "Speech targets" = "solid","Tone precursors" = "dotted", "Speech precursors" = "dotted")
  ) +
  theme_minimal(base_size = 24) +
  ylim(0,1) + 
  geom_errorbar(aes(ymin=mean-SD/sqrt(12),ymax=mean+SD/sqrt(12)),width=0,position=position_dodge(width=0.1))
