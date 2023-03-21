## Reshape my data
library(effsize)
library(tidyr)
library(dplyr)
library(lme4)
library(lmerTest)

mydata = read.csv("/Users/t.z.cheng/Google_Drive/Research/Delaydoesmatter/real_exp/exp4_20CR12/results_shortlongdelay_2021/v2_20CR12_clean_n67_cleaned.csv") 

## Accuracy
all_conds_mean_acc <- mydata %>% 
  group_by(sub_id,onset,delay,comparison,order,key) %>% 
  summarise(mean(Correct))
all_conds_mean_acc$sub_id = as.factor(all_conds_mean_acc$sub_id)
all_conds_mean_acc$onset = as.factor(all_conds_mean_acc$onset)
all_conds_mean_acc$delay = as.factor(all_conds_mean_acc$delay)
all_conds_mean_acc$comparison = scale(all_conds_mean_acc$comparison, center = TRUE, scale = TRUE) # mean centered
all_conds_mean_acc$order = as.factor(all_conds_mean_acc$order)
all_conds_mean_acc$key = as.factor(all_conds_mean_acc$key)

Accuracy_result = summary(aov(`mean(Correct)` ~ onset * delay * comparison * order * key + Error(sub_id/onset * delay * comparison), data = all_conds_mean_acc))
Accuracy_result

## Proportion short
all_conds_mean_acc <- mydata %>% 
  group_by(sub_id,onset,delay,comparison,order,key) %>% 
  summarise(mean(Shorter))
all_conds_mean_acc$sub_id = as.factor(all_conds_mean_acc$sub_id)
all_conds_mean_acc$onset = as.factor(all_conds_mean_acc$onset)
all_conds_mean_acc$delay = as.factor(all_conds_mean_acc$delay)
all_conds_mean_acc$comparison = scale(all_conds_mean_acc$comparison, center = TRUE, scale = TRUE) # mean centered
all_conds_mean_acc$order = as.factor(all_conds_mean_acc$order)
all_conds_mean_acc$key = as.factor(all_conds_mean_acc$key)

Propotionshort_result = summary(aov(`mean(Shorter)` ~ onset * delay * comparison * order * key + Error(sub_id/onset * delay * comparison), data = all_conds_mean_acc))
Propotionshort_result

