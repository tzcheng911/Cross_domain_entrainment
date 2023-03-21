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
import seaborn as sns
import matplotlib.pyplot as plt
from scipy import stats
from sklearn import linear_model
from sklearn import metrics # confusion matrix, MSE etc.


## Load data
path_to_data = "/Users/t.z.cheng/Google_Drive/Research/cross_domain_entrainment/exp8/results/"
os.chdir(path_to_data)
df = pd.read_csv("combined_csv.csv")

# ## Set parameters
# - Accuracy threshold for the easiest trials of the main task 
# - Extreme RT threshold
# - Catch trial accuracy


## Define parameters 
threshold = .55
catch_threshold = .7
RT_threshold = 10000

## Tasks & Conditions: Clean up task and subject ID (first five characters)
df['task'] = df['trial_template'].apply(lambda x: x.split("_")[1] if (x.split("_")[0] == "EXP8c") else x.split("_")[0])
df['sub_id'] = df['participant_id'].apply(lambda x: x.split()[0][0:5])
## for exp8abc
df['exp'] = df['group_id'].apply(lambda x: x.split("_")[0])

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

## How many subjects miss the catch trial 
catch_trials = ['Catch_trials'] ## for exp8abc

df_catch = df[(df['trial_template'].isin(catch_trials))].reset_index(drop = True) # reset index from 1
catch_acc = df_catch.groupby('sub_id')['response_correct'].sum()/df_catch.groupby('sub_id')['response_correct'].count()
fail_catch = catch_acc[catch_acc < catch_threshold].reset_index()
print(len(fail_catch))
print(fail_catch)

## remove subjects who failed the catch trial based on the threshold
df_clean = df[~df['sub_id'].isin(fail_catch['sub_id'])].reset_index(drop = True)

## How many subjects had a bad environmental noise and device
df_noise = df[(df['response_name'] == 'survey_noise')].reset_index(drop = True)
noise = df_noise.groupby('sub_id')['response_value'].sum()
too_noisy = noise[noise == '4'].reset_index()
print(len(too_noisy))
print(too_noisy)

## remove subjects who completed the task in noisy environment
df_clean = df_clean[~df_clean['sub_id'].isin(too_noisy['sub_id'])].reset_index(drop = True)

# may remove subjects who completed the task using low-quality audio device
df_device = df[(df['response_name'] == 'survey_headphone1')].reset_index()
len(df_device[['sub_id','response_value']] == 'Yes')


## After cleaning how many subjects left
## Number of subjects, trials & conditions for each subjects
n_subj = len(df_clean['participant_id'].unique())
n_trial = len(df_clean)//n_subj # how many trials per subject
n_conds = len(df_clean['stimuli_presented'].unique())
print('Participant_number:', n_subj,'Trial number:', n_trial,'Condition number:', n_conds, sep='\n')

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
    exp = df_clean['exp'][i]
    if exp != "EXP8a":
        onset.append(stimuli_presented[2])
        length.append(stimuli_presented[-1])
    else:
        onset.append(stimuli_presented[0])
        length.append(stimuli_presented[-1][-1])
df_clean['Onset'] = onset
df_clean['Length'] = length

## save df to csv
df_clean.to_csv(path_to_data+"_clean_n" + str(n_subj) +".csv", header=True)
print('Cleaned data saved!')

