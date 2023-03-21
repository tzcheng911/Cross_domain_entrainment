#!/usr/bin/env python
# coding: utf-8

## Import packages
import csv
import glob
import os
import numpy as np
import pandas as pd
from collections import defaultdict
import string
from scipy import stats
import argparse

## Create the parser
parser = argparse.ArgumentParser()
# Add an argument
parser.add_argument('--path', type=str, default="/Users/t.z.cheng/Google_Drive/Research/cross_domain_entrainment/exp8/results/old/", help='the path having the data')
parser.add_argument('--exp', type=str, choices=['EXP4c','EXP6','EXP8a', 'EXP8b', 'EXP8c','EXP8c-s','EXP8c-t'], required=True, help='which experiments')
parser.add_argument('--catch_threshold', type=float, default=0.9, help='input catch threshold')
parser.add_argument('--RT_threshold', type=int, default=10000, help='input RT threshold')
# Parse the argument
args = parser.parse_args()

# Print "Hello" + the user input argument
print('path: ', args.path)
print('which_exp: ', args.exp)
print('catch_threshold: ', args.catch_threshold)
print('RT_threshold: ', args.RT_threshold)

## Load data
# path_to_data = "/Users/t.z.cheng/Google_Drive/Research/cross_domain_entrainment/exp8/results/"

# ## Set parameters
# - Accuracy threshold for the easiest trials of the main task 
# - Extreme RT threshold
# - Catch trial accuracy


## Define parameters 
# which_exp = input ("Enter EXP8a, EXP8b or EXP8c:")
# threshold = .55
# catch_threshold = .9
# RT_threshold = 10000
which_exp = args.exp
catch_threshold = args.catch_threshold
RT_threshold = args.RT_threshold # did not use here
path_to_data = args.path


## Deal with the different naming across experiments
if which_exp == "EXP4c":
    df = pd.read_csv("/Users/t.z.cheng/Google_Drive/Research/cross_domain_entrainment/Delaydoesmatter/real_exp/exp4_20CR12/4c/results/session-62159fb8dfe54372fbc126e5-data.csv") 
    df['task'] = df['trial_template'].apply(lambda x: x.split("_")[0])
    df['exp'] = which_exp

elif which_exp == "EXP6":
    df = pd.read_csv("/Users/t.z.cheng/Google_Drive/Research/cross_domain_entrainment/exp6_21CR03_Vowel_length/FF2021/results/session-6215a55807fa7c7666d39d6e-data.csv")
    df['task'] = df['trial_template'].apply(lambda x: x.split("_")[0])
    df['exp'] = which_exp

else:
    ## Tasks & Conditions: Clean up task and subject ID (first five characters)
    os.chdir(path_to_data)
    df = pd.read_csv("combined_csv.csv")
    df['task'] = df['trial_template'].apply(lambda x: x.split("_")[1] if (x.split("_")[0] == which_exp.split("-")[0]) else x.split("_")[0])
    ## for exp8abc
    df['exp'] = df['group_id'].apply(lambda x: x.split("_")[0])
    df = df[(df['exp'] == which_exp.split("-")[0])].reset_index(drop = True)
    print("How many subjects collected across all three exps:", len(df['participant_id'].unique()))

df['sub_id'] = df['participant_id'].apply(lambda x: x.split()[0][0:5])
print("How many subjects collected in this exp:", len(df['participant_id'].unique()))

## Add the accuracy and PPS column to the dataset: transform True and Shorter to 1, False and longer to 0
Correct = [] # only applied for the prescreen
Shorter = []
for i in np.arange(0,len(df)):
    if df['response_correct'][i] == True:
        Correct.append(1)
    else: 
        Correct.append(0)
    if ((df['response_value'][i] == "Lap") or (df['response_value'][i] == "Shorter")): 
        Shorter.append(1)
    else: 
        Shorter.append(0)
df['Correct'] = Correct
df['Shorter'] = Shorter

# ## Data cleaning 
# ***Super important: df_clean is overwritten after each step of data cleaning***
# 
# **Reject subjects**
# - Catch trial
# - Environmental noise & audio device (may not need to use them as criteria)
# - Fail the practice trial on the last 12 practice trial: pass people have done 24 trials, fail people have done 36 trials
# 
# **Reject trials**
# - Task-relevant trials
# - Extreme RT

## exp8 hear as speech or hear as tone 
subj_hear_speech = df[df["response_value"] == "Lab or Lap"]['sub_id'].unique()
subj_hear_tone = df[df["response_value"] == "Musical Tones"]['sub_id'].unique()

if which_exp == "EXP8c-s":
    df = df[df['sub_id'].isin(subj_hear_speech)].reset_index(drop = True)
elif which_exp == "EXP8c-t":
    df = df[df['sub_id'].isin(subj_hear_tone)].reset_index(drop = True)
else:
    df = df

## How many subjects fail the catch trial 
catch_trials = ['Catch_trials'] ## for exp8abc
df_catch = df[(df['trial_template'].isin(catch_trials))].reset_index(drop = True) # reset index from 1
catch_acc = df_catch.groupby('sub_id')['response_correct'].sum()/df_catch.groupby('sub_id')['response_correct'].count()
fail_catch = catch_acc[catch_acc < catch_threshold].reset_index()
print("How many subjects failed catch trials:", len(fail_catch))
print(fail_catch)

## remove subjects who failed the catch trial based on the threshold
df_clean = df[~df['sub_id'].isin(fail_catch['sub_id'])].reset_index(drop = True)

## How many subjects had a bad environmental noise and device
df_noise = df[(df['response_name'] == 'survey_noise')].reset_index(drop = True)
noise = df_noise.groupby('sub_id')['response_value'].sum()
too_noisy = noise[noise == '4'].reset_index()
print("How many subjects failed noisy environment:",len(too_noisy))
print(too_noisy)

## remove subjects who completed the task in noisy environment
df_clean = df_clean[~df_clean['sub_id'].isin(too_noisy['sub_id'])].reset_index(drop = True)

# may remove subjects who completed the task using low-quality audio device
# df_device = df[(df['response_name'] == 'survey_headphone1')].reset_index()
# len(df_device[['sub_id','response_value']] == 'Yes')

## After cleaning how many subjects left
## Number of subjects, trials & conditions for each subjects
n_subj = len(df_clean['participant_id'].unique())
n_trial = len(df_clean)//n_subj # how many trials per subject
n_conds = len(df_clean['stimuli_presented'].unique())
print('Participant_number:', n_subj,'Trial number:', n_trial,'Condition number:', n_conds, sep='\n')

# ### Task relevant trials
# #### Select the trials to analyze
# - Pretest trails (n = 60): single tone 1 or 8
# - Practice trials (n = 24 or 36): 6 context tones + 1 ontime single tone 1 or 8 with correct/incorrect feedback
# - Main task (n = 576): 6 context tones + 1 single tone (early, ontime, late; 1 to 8 steps) without feedback

# ### Select the pretest trials 
df_pre = df_clean[(df_clean['task'] == "PretestTrials")].reset_index()
df_pre.groupby(['sub_id','stimuli_presented'])['Correct'].mean()
pre_acc = df_pre.groupby(['sub_id'])['Correct'].mean()
conds = df_pre['stimuli_presented'].unique()
print(df_pre.groupby('stimuli_presented')['Shorter'].mean())

# ### Select the practice trials
df_prac = df_clean[(df_clean['task'] == "practiceTrials")].reset_index()
conds = df_prac['stimuli_presented'].unique()
print(df_prac.groupby('stimuli_presented')['Shorter'].mean())

# ### Select the main trials
df_clean = df_clean[(df_clean['task'] == "maintaskTrials")].reset_index(drop = True)
conds = df_clean['stimuli_presented'].unique()
print(conds) # check the conditions

## Create new cols for onset and length
onset = []
length = []
df_clean = df_clean.set_index(pd.Index(np.arange(0,len(df_clean)))) # change the index for the for loop

for i in np.arange(0,len(df_clean)):
    stimuli_presented = df_clean['stimuli_presented'][i].split('_')
    if which_exp == "EXP8a" or which_exp == 'EXP6':
    # if which_exp in method1_set:
        onset.append(stimuli_presented[0])
        length.append(stimuli_presented[-1][-1])
    else:
        onset.append(stimuli_presented[2]) 
        length.append(stimuli_presented[-1])
        
df_clean['Onset'] = onset
df_clean['Length'] = length

# Proportion short/lap
print("Proportion Short")
print(df_clean.groupby(['Onset'])['Shorter'].mean())

pps = df_clean.groupby(['Onset'])['Shorter'].mean()
pps_all = df_clean.groupby(['stimuli_presented','Onset'])['Shorter'].mean()

# ### Statistic tests
PPS = df_clean.groupby(['sub_id','Onset'])['Shorter'].mean()
PPS_early = PPS[::3]
PPS_ontime = PPS[2::3]
PPS_late = PPS[1::3]

print(stats.ttest_rel(PPS_early, PPS_late))
print(stats.ttest_rel(PPS_early, PPS_ontime))
print(stats.ttest_rel(PPS_late, PPS_ontime))

## save df to csv
df_clean.to_csv(path_to_data+which_exp+"_clean_n" + str(n_subj) +".csv", header=True)
print('Cleaned data saved!')