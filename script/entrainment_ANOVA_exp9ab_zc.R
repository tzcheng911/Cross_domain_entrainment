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
EXP = read.csv("/Users/t.z.cheng/Documents/GitHub/Cross_domain_entrainment/exp9ab/results/r1_r2_r3_r4.csv") 

## mutate the conditions: stimuli (ad/at, lab/lap, tone/tone1), delay (metronome is 0,30,60,90,120 ms behind the sound), len (short 1, long 8)
EXP = EXP %>%
  mutate(Shorter = ifelse(EXP$response_value=="At" | EXP$response_value=="Shorter",1,0))
EXP = EXP %>%
  mutate(stimuli = str_split(as.character(EXP$trial_template),"[0-9]+",simplify = T)[,2])

## Preprocessing
# Filter the discrimination task
DiscEXP = filter(EXP, trial_template == "EXP9b_PretestTrials_rv")
DiscEXP = DiscEXP %>%
  mutate(len = str_sub(str_split(as.character(DiscEXP$stimuli_presented),"_",simplify = T)[,1],-1))
DiscEXP %>% 
  group_by(stimuli_presented) %>%
  summarize(mean = mean(Shorter), SD = sd(Shorter))

# Filter the main task
# Coding response to 0: out of time, and 1: in time
# mutate the conditions: stimuli (ad/at, lab/lap, tone/tone1), delay (metronome is 0,30,60,90,120 ms behind the sound), len (short 1, long 8)
mainEXP = filter(EXP, trial_template == "EXP9a_maintaskTrials_rv") # select the main trials after 8 examples
length(unique(mainEXP$participant_id)) # how many subjects
mainEXP = mainEXP %>%
  mutate(onset = str_split(as.character(mainEXP$stimuli_presented),"_",simplify = T)[,1])
mainEXP = mainEXP %>%
  mutate(len = str_sub(str_split(as.character(mainEXP$stimuli_presented),"_",simplify = T)[,2],-1))
mainEXP$onset = factor(mainEXP$onset, levels = c("early","ontime","late"))
mainEXP$len = factor(mainEXP$len, levels = c("1","2","3","4","5","6","7","8"))

## Check the stimuli presentation: divide by # of subjects should be the same
# Speech: "DiscriminationTrials_rv","EXP9a_maintaskTrials"; Tones:"EXP9b_PretestTrials_rv", PrescreenTrials_tones_rv"
countEXP = filter(EXP, trial_template == "EXP9a_maintaskTrials") 
counting = countEXP %>%
  group_by(stimuli_presented) %>%
  count()
length(unique(countEXP$participant_id)) # how many subjects

## Analysis
mainEXPmeans=mainEXP %>% 
  group_by(onset,len) %>%
  summarize(mean = mean(Shorter), SD = sd(Shorter))

mainEXP %>% 
  group_by(onset) %>%
  summarize(mean = mean(Shorter), SD = sd(Shorter))
