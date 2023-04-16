library(lme4)
library(lmerTest)
library(tidyverse)
library(broom)
library(effsize)
library('fastDummies')

## Load the data
# mydata = read.csv("/Users/t.z.cheng/Google_Drive/Research/Delaydoesmatter/real_exp/exp2/data_analysis/Rdata/exp2_PPS_n48.csv") # exp2
mydata = read.csv("/Users/t.z.cheng/Google_Drive/Research/cross_domain_entrainment/Delaydoesmatter/real_exp/exp3_20CR11/results/EXP3_clean_n34.csv") # exp3
mydata = read.csv("/Users/t.z.cheng/Google_Drive/Research/cross_domain_entrainment/Delaydoesmatter/real_exp/exp4_20CR12/4ab/results_shortdelay_2020/EXP4a_clean_n53.csv") # exp4a
mydata = read.csv("/Users/t.z.cheng/Google_Drive/Research/cross_domain_entrainment/Delaydoesmatter/real_exp/exp4_20CR12/4ab/results_shortlongdelay_2021/EXP4b_clean_n67.csv") # exp4b
mydata = read.csv("/Users/t.z.cheng/Google_Drive/Research/cross_domain_entrainment/Delaydoesmatter/real_exp/exp4_20CR12/4c/results/EXP4c_clean_n59.csv") # exp4c
mydata = read.csv("/Users/t.z.cheng/Google_Drive/Research/cross_domain_entrainment/Delaydoesmatter/real_exp/exp5_21CR01/results/EXP5_clean_n24_e.csv") # exp5 empty
mydata = read.csv("/Users/t.z.cheng/Google_Drive/Research/cross_domain_entrainment/Delaydoesmatter/real_exp/exp5_21CR01/results/EXP5_clean_n24_f.csv") # exp5 filled
mydata = read.csv("/Users/t.z.cheng/Google_Drive/Research/cross_domain_entrainment/exp6_21CR03_Vowel_length/FF2021/results/EXP6_clean_n64.csv") # exp6 VL

str(mydata) #data inspection, you should see that all the variables are "int" they need to be centered and scaled

############################ standardize the input to the model ###########################
## may need to change some variable name

# mydata = mydata %>%
#  mutate(Onset =case_when(Onset == 5 ~ "Early", Onset == 8 ~ "Ontime", Onset == 9 ~ "Late"))
# mydata = mydata %>%
  # mutate(nresponse_value =case_when(response_value == "Longer" ~ 0, response_value == "Shorter" ~1))
mydata = mydata %>%
  mutate(nresponse_value = Shorter)
## Rescale and re-reference
mydata = mydata %>%
  mutate(rLength = scale(Length, center = TRUE, scale = TRUE)) # scale the steps 
mydata = mydata %>%
  mutate(fOnset = as.factor(Onset))
mydata$fOnsetR = relevel(mydata$fOnset, ref="ontime") # make ontime condition the reference 

############################ glm model with onset coded as Early, ontime, late run for each experiment ###########################
lm = glmer(nresponse_value ~ fOnsetR + rLength + fOnsetR:rLength + (1 + fOnsetR + rLength + fOnsetR:rLength|participant_id),data= mydata,family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
summary(lm) # could use estimate to compare "entrainment effect" across experiments
ranef(lm)

###################################################### RM ANOVA on PPS ###################################################
all_conds_mean_pps <- mydata %>% 
  group_by(sub_id,onset,delay,comparison,order,key) %>% 
  summarise(PPS = mean(Shorter))
PPS <- all_conds_mean_pps %>% 
  group_by(onset) %>% 
  summarise(PPS = mean(PPS))
PPS

all_conds_mean_pps$sub_id = as.factor(all_conds_mean_pps$sub_id)
all_conds_mean_pps$onset = as.factor(all_conds_mean_pps$onset)
all_conds_mean_pps$delay = as.factor(all_conds_mean_pps$delay)
all_conds_mean_pps$comparison = scale(all_conds_mean_pps$comparison, center = TRUE, scale = TRUE) # mean centered
all_conds_mean_pps$order = as.factor(all_conds_mean_pps$order)
all_conds_mean_pps$key = as.factor(all_conds_mean_pps$key)

# EXP3 4a and 5: only short delay 
Propotionshort_result = summary(aov(PPS ~ onset + Error(sub_id/(onset)), data = all_conds_mean_pps))
Propotionshort_result
Propotionshort_result$'Error: sub_id:onset'[[1]]$`Sum Sq`[1]/sum(Propotionshort_result$'Error: sub_id:onset'[[1]]$`Sum Sq`)

## Pairwise comparison for interaction effects
Proportiondata <- all_conds_mean_pps %>% 
  group_by(sub_id,onset) %>% 
  summarise(PPS = mean(PPS))

early_short = Proportiondata$`PPS`[Proportiondata$onset==5]
ontime_short = Proportiondata$`PPS`[Proportiondata$onset==8]
late_short = Proportiondata$`PPS`[Proportiondata$onset==9]

# Short condition 
p1 = t.test(early_short,ontime_short, paired = TRUE) # Early vs Ontime in short condition 
cohen.d(early_short,ontime_short)
p2 = t.test(early_short,late_short, paired = TRUE) # Early vs Late in short condition
cohen.d(early_short,late_short)
p3 = t.test(ontime_short,late_short, paired = TRUE) # Late vs Ontime in short condition
cohen.d(ontime_short,late_short)
p.adjust(p1[["p.value"]], method = "bonferroni", n = 3) # Early vs Ontime in short condition 
p.adjust(p2[["p.value"]], method = "bonferroni", n = 3) # Early vs Late in short condition 
p.adjust(p3[["p.value"]], method = "bonferroni", n = 3) # Late vs Ontime in short condition 

# EXP4b: short and long delay
Propotionshort_result = summary(aov(PPS ~ onset * delay + Error(sub_id/(onset * delay)), data = all_conds_mean_pps))
Propotionshort_result
Propotionshort_result$'Error: sub_id:onset'[[1]]$`Sum Sq`[1]/sum(Propotionshort_result$'Error: sub_id:onset'[[1]]$`Sum Sq`)
Propotionshort_result$'Error: sub_id:delay'[[1]]$`Sum Sq`[1]/sum(Propotionshort_result$'Error: sub_id:delay'[[1]]$`Sum Sq`)
Propotionshort_result$'Error: sub_id:onset:delay'[[1]]$`Sum Sq`[1]/sum(Propotionshort_result$'Error: sub_id:onset:delay'[[1]]$`Sum Sq`)

## Pairwise comparison for interaction effects
Proportiondata <- all_conds_mean_pps %>% 
  group_by(sub_id,onset,delay) %>% 
  summarise(PPS = mean(PPS))

early_short = Proportiondata$`PPS`[Proportiondata$onset==5 & Proportiondata$delay==2]
ontime_short = Proportiondata$`PPS`[Proportiondata$onset==8 & Proportiondata$delay==2]
late_short = Proportiondata$`PPS`[Proportiondata$onset==9 & Proportiondata$delay==2]

early_long = Proportiondata$`PPS`[Proportiondata$onset==5 & Proportiondata$delay==4]
ontime_long = Proportiondata$`PPS`[Proportiondata$onset==8 & Proportiondata$delay==4]
late_long = Proportiondata$`PPS`[Proportiondata$onset==9 & Proportiondata$delay==4]

# Short condition 
p1 = t.test(early_short,ontime_short, paired = TRUE) # Early vs Ontime in short condition 
cohen.d(early_short,ontime_short)
p2 = t.test(early_short,late_short, paired = TRUE) # Early vs Late in short condition
cohen.d(early_short,late_short)
p3 = t.test(ontime_short,late_short, paired = TRUE) # Late vs Ontime in short condition
cohen.d(ontime_short,late_short)
p.adjust(p1[["p.value"]], method = "bonferroni", n = 3) # Early vs Ontime in short condition 
p.adjust(p2[["p.value"]], method = "bonferroni", n = 3) # Early vs Late in short condition 
p.adjust(p3[["p.value"]], method = "bonferroni", n = 3) # Late vs Ontime in short condition 

# long condition 
p1 = t.test(early_long,ontime_long, paired = TRUE) # Early vs Ontime in short condition 
cohen.d(early_long,ontime_long)
p2 = t.test(early_long,late_long, paired = TRUE) # Early vs Late in short condition
cohen.d(early_long,late_long)
p3 = t.test(ontime_long,late_long, paired = TRUE) # Late vs Ontime in short condition
cohen.d(ontime_long,late_long)
p.adjust(p1[["p.value"]], method = "bonferroni", n = 3) # Early vs Ontime in short condition 
p.adjust(p2[["p.value"]], method = "bonferroni", n = 3) # Early vs Late in short condition 
p.adjust(p3[["p.value"]], method = "bonferroni", n = 3) # Late vs Ontime in short condition 



############### Other: coded the onset as continuous variables
lm = glmer(nresponse_value ~ rOnset + rLength + rOnset:rLength + (1 + Onset + rLength + Onset:rLength|participant_id),data= mydata,family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
summary(lm)
ranef(lm)

mydata %>%
  group_by(participant_id, Onset) %>%
  summarise(prop_short = mean(response_value))

df_by_subject %>%
  ggplot(aes(x = Onset,
             y = prop_short,
             color = participant_id)) +
  geom_point()

df_by_subject %>%
  ggplot(aes(x = Onset,
             y = prop_short,
             color = participant_id)) +
  geom_point() +
  geom_line() +
  theme(legend.position = "none")

## group the subjects
df_by_subject = two_filled_data %>%
  mutate(response_numeric = as.numeric(response_value)-1) %>%
  group_by(participant_id, Onset) %>%
  summarise(prop_short = mean(response_numeric))

