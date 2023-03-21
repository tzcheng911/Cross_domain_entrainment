## Reshape my data
library(effsize)
library(tidyr)
library(dplyr)

setwd('/Users/t.z.cheng/Google_Drive/Research/Delaydoesmatter/real_exp/exp2/data_analysis/Rdata')

##### Accuracy ####
Accdata = read.csv('/Users/t.z.cheng/Google_Drive/Research/Delaydoesmatter/real_exp/exp2/data_analysis/Rdata/Accuracy.csv')
long_Accdata <- gather(Accdata,BD,Accuracy,Early.Shortdelay:Late.Longdelay)
separate_Accdata <- separate(long_Accdata, BD, c("Comparison_onsets", "Delay_length"))

## Run repeated measure ANOVA
# main effect & interaction
Acc_F = summary(aov(Accuracy ~ Comparison_onsets * Delay_length + Error(subject/(Comparison_onsets * Delay_length)), data = separate_Accdata))
Acc_F$'Error: subject:Comparison_onsets'[[1]]$`Sum Sq`[1]/sum(Acc_F$'Error: subject:Comparison_onsets'[[1]]$`Sum Sq`)
Acc_F$'Error: subject:Delay_length'[[1]]$`Sum Sq`[1]/sum(Acc_F$'Error: subject:Delay_length'[[1]]$`Sum Sq`)
Acc_F$'Error: subject:Comparison_onsets:Delay_length'[[1]]$`Sum Sq`[1]/sum(Acc_F$'Error: subject:Comparison_onsets:Delay_length'[[1]]$`Sum Sq`)

# Simple main effect
Acc_Short = separate_Accdata %>%
  filter(Delay_length =='Shortdelay')
p1 = summary(aov(Accuracy ~ Comparison_onsets + Error(subject/(Comparison_onsets)), data = Acc_Short))
p.adjust(p1$'Error: subject:Comparison_onsets'[[1]]$'Pr(>F)'[1], method = "bonferroni", n = 3)
Acc_Medium = separate_Accdata %>%
  filter(Delay_length =='Mediumdelay')
p2 = summary(aov(Accuracy ~ Comparison_onsets + Error(subject/(Comparison_onsets)), data = Acc_Medium))
p.adjust(p2$'Error: subject:Comparison_onsets'[[1]]$'Pr(>F)'[1], method = "bonferroni", n = 3)
Acc_Long = separate_Accdata %>%
  filter(Delay_length =='Longdelay')
p3 = summary(aov(Accuracy ~ Comparison_onsets + Error(subject/(Comparison_onsets)), data = Acc_Long))
p.adjust(p3$'Error: subject:Comparison_onsets'[[1]]$'Pr(>F)'[1], method = "bonferroni", n = 3)
p1$'Error: subject:Comparison_onsets'[[1]]$`Sum Sq`[1]/sum(p1$'Error: subject:Comparison_onsets'[[1]]$`Sum Sq`)
p2$'Error: subject:Comparison_onsets'[[1]]$`Sum Sq`[1]/sum(p2$'Error: subject:Comparison_onsets'[[1]]$`Sum Sq`)
p3$'Error: subject:Comparison_onsets'[[1]]$`Sum Sq`[1]/sum(p3$'Error: subject:Comparison_onsets'[[1]]$`Sum Sq`)

## ttest in comparison onset (early, ontime, late)
p1 = t.test(Accdata$Early,Accdata$Ontime, paired = TRUE)
cohen.d(Accdata$Early,Accdata$Ontime)
p2 = t.test(Accdata$Early,Accdata$Late, paired = TRUE)
cohen.d(Accdata$Early,Accdata$Late)
p3 = t.test(Accdata$Late,Accdata$Ontime, paired = TRUE)
cohen.d(Accdata$Late,Accdata$Ontime)
p.adjust(p1[["p.value"]], method = "bonferroni", n = 3)
p.adjust(p2[["p.value"]], method = "bonferroni", n = 3)
p.adjust(p3[["p.value"]], method = "bonferroni", n = 3)

## ttest in delay length (short, medium, long)
p1 = t.test(Accdata$ShortDelay,Accdata$MediumDelay, paired = TRUE)
cohen.d(Accdata$ShortDelay,Accdata$MediumDelay)
p2 = t.test(Accdata$ShortDelay,Accdata$LongDelay, paired = TRUE)
cohen.d(Accdata$ShortDelay,Accdata$LongDelay)
p3 = t.test(Accdata$MediumDelay,Accdata$LongDelay, paired = TRUE)
cohen.d(Accdata$MediumDelay,Accdata$LongDelay)
p.adjust(p1[["p.value"]], method = "bonferroni", n = 3)
p.adjust(p2[["p.value"]], method = "bonferroni", n = 3)
p.adjust(p3[["p.value"]], method = "bonferroni", n = 3)

## Pairwise comparison for interaction effects
# Accuracy
# Short condition 
p1 = t.test(Accdata$Early.Shortdelay,Accdata$Ontime.Shortdelay, paired = TRUE) # Early vs Ontime in short condition 
cohen.d(Accdata$Early.Shortdelay,Accdata$Ontime.Shortdelay)
p2 = t.test(Accdata$Early.Shortdelay,Accdata$Late.Shortdelay, paired = TRUE) # Early vs Late in short condition
cohen.d(Accdata$Early.Shortdelay,Accdata$Late.Shortdelay)
p3 = t.test(Accdata$Ontime.Shortdelay,Accdata$Late.Shortdelay, paired = TRUE) # Late vs Ontime in short condition
cohen.d(Accdata$Ontime.Shortdelay,Accdata$Late.Shortdelay)

# Medium condition 
p4 = t.test(Accdata$Early.Mediumdelay,Accdata$Ontime.Mediumdelay, paired = TRUE) # Early vs Ontime in short condition 
cohen.d(Accdata$Early.Mediumdelay,Accdata$Ontime.Mediumdelay)
p5 = t.test(Accdata$Early.Mediumdelay,Accdata$Late.Mediumdelay, paired = TRUE) # Early vs Late in short condition
cohen.d(Accdata$Early.Mediumdelay,Accdata$Late.Mediumdelay)
p6 = t.test(Accdata$Ontime.Mediumdelay,Accdata$Late.Mediumdelay, paired = TRUE) # Late vs Ontime in short condition
cohen.d(Accdata$Ontime.Mediumdelay,Accdata$Late.Mediumdelay)

# Long condition
p7 = t.test(Accdata$Early.Longdelay,Accdata$Ontime.Longdelay, paired = TRUE) # Early vs Ontime in long condition
p8 = t.test(Accdata$Early.Longdelay,Accdata$Late.Longdelay, paired = TRUE) # Early vs Late in long condition
p9 = t.test(Accdata$Ontime.Longdelay,Accdata$Late.Longdelay, paired = TRUE) # Late vs Ontime in long condition
p.adjust(p1[["p.value"]], method = "bonferroni", n = 3) # Early vs Ontime in short condition 
p.adjust(p2[["p.value"]], method = "bonferroni", n = 3) # Early vs Late in short condition 
p.adjust(p3[["p.value"]], method = "bonferroni", n = 3) # Late vs Ontime in short condition 
p.adjust(p4[["p.value"]], method = "bonferroni", n = 3) # Early vs Ontime in long condition 
p.adjust(p5[["p.value"]], method = "bonferroni", n = 3) # Early vs Late in long condition 
p.adjust(p6[["p.value"]], method = "bonferroni", n = 3) # Late vs Ontime in long condition 
p.adjust(p7[["p.value"]], method = "bonferroni", n = 3) # Early vs Ontime in long condition 
p.adjust(p8[["p.value"]], method = "bonferroni", n = 3) # Early vs Late in long condition 
p.adjust(p9[["p.value"]], method = "bonferroni", n = 3) # Late vs Ontime in long condition 

##### Proportion Short ####
Proportiondata = read.csv('/Users/t.z.cheng/Google_Drive/Research/Delaydoesmatter/real_exp/exp2/data_analysis/Rdata/Proportion_short.csv')
long_Proportiondata <- gather(Proportiondata,BD,Proportion_short,Early.Shortdelay:Late.Longdelay)
separate_Proportiondata <- separate(long_Proportiondata, BD, c("Comparison_onsets", "Delay_length"))

## Run repeated measure ANOVA
# main effect & interaction
PPS_F = summary(aov(Proportion_short ~ Comparison_onsets * Delay_length + Error(subject/(Comparison_onsets * Delay_length)), data = separate_Proportiondata))
PPS_F$'Error: subject:Comparison_onsets'[[1]]$`Sum Sq`[1]/sum(PPS_F$'Error: subject:Comparison_onsets'[[1]]$`Sum Sq`)
PPS_F$'Error: subject:Delay_length'[[1]]$`Sum Sq`[1]/sum(PPS_F$'Error: subject:Delay_length'[[1]]$`Sum Sq`)
PPS_F$'Error: subject:Comparison_onsets:Delay_length'[[1]]$`Sum Sq`[1]/sum(PPS_F$'Error: subject:Comparison_onsets:Delay_length'[[1]]$`Sum Sq`)

# Simple main effect
PPS_Short = separate_Proportiondata %>%
  filter(Delay_length =='Shortdelay')
p1 = summary(aov(Proportion_short ~ Comparison_onsets + Error(subject/(Comparison_onsets)), data = PPS_Short))
p.adjust(p1$'Error: subject:Comparison_onsets'[[1]]$'Pr(>F)'[1], method = "bonferroni", n = 3)
PPS_Medium = separate_Proportiondata %>%
  filter(Delay_length =='Mediumdelay')
p2 = summary(aov(Proportion_short ~ Comparison_onsets + Error(subject/(Comparison_onsets)), data = PPS_Medium))
p.adjust(p2$'Error: subject:Comparison_onsets'[[1]]$'Pr(>F)'[1], method = "bonferroni", n = 3)
PPS_Long = separate_Proportiondata %>%
  filter(Delay_length =='Longdelay')
p3 = summary(aov(Proportion_short ~ Comparison_onsets + Error(subject/(Comparison_onsets)), data = PPS_Long))
p.adjust(p3$'Error: subject:Comparison_onsets'[[1]]$'Pr(>F)'[1], method = "bonferroni", n = 3)
p1$'Error: subject:Comparison_onsets'[[1]]$`Sum Sq`[1]/sum(p1$'Error: subject:Comparison_onsets'[[1]]$`Sum Sq`)
p2$'Error: subject:Comparison_onsets'[[1]]$`Sum Sq`[1]/sum(p2$'Error: subject:Comparison_onsets'[[1]]$`Sum Sq`)
p3$'Error: subject:Comparison_onsets'[[1]]$`Sum Sq`[1]/sum(p3$'Error: subject:Comparison_onsets'[[1]]$`Sum Sq`)

## ttest in comparison onset (early, ontime, late)
p1 = t.test(Proportiondata$Early,Proportiondata$Ontime, paired = TRUE)
cohen.d(Proportiondata$Early,Proportiondata$Ontime)
p2 = t.test(Proportiondata$Early,Proportiondata$Late, paired = TRUE)
cohen.d(Proportiondata$Early,Proportiondata$Late)
p3 = t.test(Proportiondata$Late,Proportiondata$Ontime, paired = TRUE)
cohen.d(Proportiondata$Late,Proportiondata$Ontime)
p.adjust(p1[["p.value"]], method = "bonferroni", n = 3)
p.adjust(p2[["p.value"]], method = "bonferroni", n = 3)
p.adjust(p3[["p.value"]], method = "bonferroni", n = 3)

## ttest in delay length (short, medium, long)
p1 = t.test(Proportiondata$ShortDelay,Proportiondata$MediumDelay, paired = TRUE)
cohen.d(Proportiondata$ShortDelay,Proportiondata$MediumDelay)
p2 = t.test(Proportiondata$ShortDelay,Proportiondata$LongDelay, paired = TRUE)
cohen.d(Proportiondata$ShortDelay,Proportiondata$LongDelay)
p3 = t.test(Proportiondata$MediumDelay,Proportiondata$LongDelay, paired = TRUE)
cohen.d(Proportiondata$MediumDelay,Proportiondata$LongDelay)
p.adjust(p1[["p.value"]], method = "bonferroni", n = 3)
p.adjust(p2[["p.value"]], method = "bonferroni", n = 3)
p.adjust(p3[["p.value"]], method = "bonferroni", n = 3)

## Pairwise comparison for interaction effects
# Proportion_short
# Short condition 
p1 = t.test(Proportiondata$Early.Shortdelay,Proportiondata$Ontime.Shortdelay, paired = TRUE) # Early vs Ontime in short condition 
cohen.d(Proportiondata$Early.Shortdelay,Proportiondata$Ontime.Shortdelay)
p2 = t.test(Proportiondata$Early.Shortdelay,Proportiondata$Late.Shortdelay, paired = TRUE) # Early vs Late in short condition
cohen.d(Proportiondata$Early.Shortdelay,Proportiondata$Late.Shortdelay)
p3 = t.test(Proportiondata$Ontime.Shortdelay,Proportiondata$Late.Shortdelay, paired = TRUE) # Late vs Ontime in short condition
cohen.d(Proportiondata$Ontime.Shortdelay,Proportiondata$Late.Shortdelay)

# Medium condition 
p4 = t.test(Proportiondata$Early.Mediumdelay,Proportiondata$Ontime.Mediumdelay, paired = TRUE) # Early vs Ontime in short condition 
cohen.d(Proportiondata$Early.Mediumdelay,Proportiondata$Ontime.Mediumdelay)
p5 = t.test(Proportiondata$Early.Mediumdelay,Proportiondata$Late.Mediumdelay, paired = TRUE) # Early vs Late in short condition
cohen.d(Proportiondata$Early.Mediumdelay,Proportiondata$Late.Mediumdelay)
p6 = t.test(Proportiondata$Ontime.Mediumdelay,Proportiondata$Late.Mediumdelay, paired = TRUE) # Late vs Ontime in short condition
cohen.d(Proportiondata$Ontime.Mediumdelay,Proportiondata$Late.Mediumdelay)

# Long condition
p7 = t.test(Proportiondata$Early.Longdelay,Proportiondata$Ontime.Longdelay, paired = TRUE) # Early vs Ontime in long condition
p8 = t.test(Proportiondata$Early.Longdelay,Proportiondata$Late.Longdelay, paired = TRUE) # Early vs Late in long condition
p9 = t.test(Proportiondata$Ontime.Longdelay,Proportiondata$Late.Longdelay, paired = TRUE) # Late vs Ontime in long condition

p.adjust(p1[["p.value"]], method = "bonferroni", n = 3) # Early vs Ontime in short condition 
p.adjust(p2[["p.value"]], method = "bonferroni", n = 3) # Early vs Late in short condition 
p.adjust(p3[["p.value"]], method = "bonferroni", n = 3) # Late vs Ontime in short condition 
p.adjust(p4[["p.value"]], method = "bonferroni", n = 3) # Early vs Ontime in long condition 
p.adjust(p5[["p.value"]], method = "bonferroni", n = 3) # Early vs Late in long condition 
p.adjust(p6[["p.value"]], method = "bonferroni", n = 3) # Late vs Ontime in long condition 
p.adjust(p7[["p.value"]], method = "bonferroni", n = 3) # Early vs Ontime in long condition 
p.adjust(p8[["p.value"]], method = "bonferroni", n = 3) # Early vs Late in long condition 
p.adjust(p9[["p.value"]], method = "bonferroni", n = 3) # Late vs Ontime in long condition 

##### PSE ####
PSEdata = read.csv('/Users/t.z.cheng/Google_Drive/Research/Delaydoesmatter/real_exp/exp2/data_analysis/Rdata/PSE.csv')
long_PSEdata <- gather(PSEdata,BD,PSE,Early.Shortdelay:Late.Longdelay)
separate_PSEdata <- separate(long_PSEdata, BD, c("Comparison_onsets", "Delay_length"))

## Run repeated measure ANOVA
# main effect & interaction
PSE_F = summary(aov(PSE ~ Comparison_onsets * Delay_length + Error(subject/(Comparison_onsets * Delay_length)), data = separate_PSEdata))
PSE_F$'Error: subject:Comparison_onsets'[[1]]$`Sum Sq`[1]/sum(PSE_F$'Error: subject:Comparison_onsets'[[1]]$`Sum Sq`)
PSE_F$'Error: subject:Delay_length'[[1]]$`Sum Sq`[1]/sum(PSE_F$'Error: subject:Delay_length'[[1]]$`Sum Sq`)
PSE_F$'Error: subject:Comparison_onsets:Delay_length'[[1]]$`Sum Sq`[1]/sum(PSE_F$'Error: subject:Comparison_onsets:Delay_length'[[1]]$`Sum Sq`)

# Simple main effect
PSE_Short = separate_PSEdata %>%
  filter(Delay_length =='Shortdelay')
p1 = summary(aov(PSE ~ Comparison_onsets + Error(subject/(Comparison_onsets)), data = PSE_Short))
p.adjust(p1$'Error: subject:Comparison_onsets'[[1]]$'Pr(>F)'[1], method = "bonferroni", n = 3)
PSE_Medium = separate_PSEdata %>%
  filter(Delay_length =='Mediumdelay')
p2 = summary(aov(PSE ~ Comparison_onsets + Error(subject/(Comparison_onsets)), data = PSE_Medium))
p.adjust(p2$'Error: subject:Comparison_onsets'[[1]]$'Pr(>F)'[1], method = "bonferroni", n = 3)
PSE_Long = separate_PSEdata %>%
  filter(Delay_length =='Longdelay')
p3 = summary(aov(PSE ~ Comparison_onsets + Error(subject/(Comparison_onsets)), data = PSE_Long))
p.adjust(p3$'Error: subject:Comparison_onsets'[[1]]$'Pr(>F)'[1], method = "bonferroni", n = 3)
p1$'Error: subject:Comparison_onsets'[[1]]$`Sum Sq`[1]/sum(p1$'Error: subject:Comparison_onsets'[[1]]$`Sum Sq`)
p2$'Error: subject:Comparison_onsets'[[1]]$`Sum Sq`[1]/sum(p2$'Error: subject:Comparison_onsets'[[1]]$`Sum Sq`)
p3$'Error: subject:Comparison_onsets'[[1]]$`Sum Sq`[1]/sum(p3$'Error: subject:Comparison_onsets'[[1]]$`Sum Sq`)

## ttest in comparison onset (early, ontime, late)
p1 = t.test(PSEdata$Early,PSEdata$Ontime, paired = TRUE)
cohen.d(PSEdata$Early,PSEdata$Ontime)
p2 = t.test(PSEdata$Early,PSEdata$Late, paired = TRUE)
cohen.d(PSEdata$Early,PSEdata$Late)
p3 = t.test(PSEdata$Late,PSEdata$Ontime, paired = TRUE)
cohen.d(PSEdata$Late,PSEdata$Ontime)
p.adjust(p1[["p.value"]], method = "bonferroni", n = 3)
p.adjust(p2[["p.value"]], method = "bonferroni", n = 3)
p.adjust(p3[["p.value"]], method = "bonferroni", n = 3)

## ttest in delay length (short, medium, long)
p1 = t.test(PSEdata$ShortDelay,PSEdata$MediumDelay, paired = TRUE)
cohen.d(PSEdata$ShortDelay,PSEdata$MediumDelay)
p2 = t.test(PSEdata$ShortDelay,PSEdata$LongDelay, paired = TRUE)
cohen.d(PSEdata$ShortDelay,PSEdata$LongDelay)
p3 = t.test(PSEdata$MediumDelay,PSEdata$LongDelay, paired = TRUE)
cohen.d(PSEdata$MediumDelay,PSEdata$LongDelay)
p.adjust(p1[["p.value"]], method = "bonferroni", n = 3)
p.adjust(p2[["p.value"]], method = "bonferroni", n = 3)
p.adjust(p3[["p.value"]], method = "bonferroni", n = 3)

## Pairwise comparison for interaction effects
# PSE
# Short condition 
p1 = t.test(PSEdata$Early.Shortdelay,PSEdata$Ontime.Shortdelay, paired = TRUE) # Early vs Ontime in short condition 
cohen.d(PSEdata$Early.Shortdelay,PSEdata$Ontime.Shortdelay)
p2 = t.test(PSEdata$Early.Shortdelay,PSEdata$Late.Shortdelay, paired = TRUE) # Early vs Late in short condition
cohen.d(PSEdata$Early.Shortdelay,PSEdata$Late.Shortdelay)
p3 = t.test(PSEdata$Ontime.Shortdelay,PSEdata$Late.Shortdelay, paired = TRUE) # Late vs Ontime in short condition
cohen.d(PSEdata$Ontime.Shortdelay,PSEdata$Late.Shortdelay)

# Medium condition 
p4 = t.test(PSEdata$Early.Mediumdelay,PSEdata$Ontime.Mediumdelay, paired = TRUE) # Early vs Ontime in short condition 
cohen.d(PSEdata$Early.Mediumdelay,PSEdata$Ontime.Mediumdelay)
p5 = t.test(PSEdata$Early.Mediumdelay,PSEdata$Late.Mediumdelay, paired = TRUE) # Early vs Late in short condition
cohen.d(PSEdata$Early.Mediumdelay,PSEdata$Late.Mediumdelay)
p6 = t.test(PSEdata$Ontime.Mediumdelay,PSEdata$Late.Mediumdelay, paired = TRUE) # Late vs Ontime in short condition
cohen.d(PSEdata$Ontime.Mediumdelay,PSEdata$Late.Mediumdelay)

# Long condition
p7 = t.test(PSEdata$Early.Longdelay,PSEdata$Ontime.Longdelay, paired = TRUE) # Early vs Ontime in long condition
p8 = t.test(PSEdata$Early.Longdelay,PSEdata$Late.Longdelay, paired = TRUE) # Early vs Late in long condition
p9 = t.test(PSEdata$Ontime.Longdelay,PSEdata$Late.Longdelay, paired = TRUE) # Late vs Ontime in long condition

p.adjust(p1[["p.value"]], method = "bonferroni", n = 3) # Early vs Ontime in short condition 
p.adjust(p2[["p.value"]], method = "bonferroni", n = 3) # Early vs Late in short condition 
p.adjust(p3[["p.value"]], method = "bonferroni", n = 3) # Late vs Ontime in short condition 
p.adjust(p4[["p.value"]], method = "bonferroni", n = 3) # Early vs Ontime in long condition 
p.adjust(p5[["p.value"]], method = "bonferroni", n = 3) # Early vs Late in long condition 
p.adjust(p6[["p.value"]], method = "bonferroni", n = 3) # Late vs Ontime in long condition 
p.adjust(p7[["p.value"]], method = "bonferroni", n = 3) # Early vs Ontime in long condition 
p.adjust(p8[["p.value"]], method = "bonferroni", n = 3) # Early vs Late in long condition 
p.adjust(p9[["p.value"]], method = "bonferroni", n = 3) # Late vs Ontime in long condition 

# one sample ttest to test if the responses are different from 300
p1 = t.test(PSEdata$Early.Shortdelay,mu = 300) 
(mean(PSEdata$Early.Shortdelay)-300)/sd(PSEdata$Early.Shortdelay) # cohen's d
p.adjust(p1[["p.value"]], method = "bonferroni", n = 3) # Early vs Ontime in short condition 
p2 = t.test(PSEdata$Ontime.Shortdelay,mu = 300) 
(mean(PSEdata$Ontime.Shortdelay)-300)/sd(PSEdata$Ontime.Shortdelay)
p.adjust(p2[["p.value"]], method = "bonferroni", n = 3) # Early vs Ontime in short condition 
p3 = t.test(PSEdata$Late.Shortdelay,mu = 300)  
(mean(PSEdata$Late.Shortdelay)-300)/sd(PSEdata$Late.Shortdelay)
p.adjust(p3[["p.value"]], method = "bonferroni", n = 3) # Early vs Ontime in short condition 
p4 = t.test(PSEdata$Early.Mediumdelay,mu = 300) 
(mean(PSEdata$Early.Mediumdelay)-300)/sd(PSEdata$Early.Mediumdelay) # cohen's d
p.adjust(p4[["p.value"]], method = "bonferroni", n = 3) # Early vs Ontime in short condition 
p5 = t.test(PSEdata$Ontime.Mediumdelay,mu = 300) 
(mean(PSEdata$Ontime.Mediumdelay)-300)/sd(PSEdata$Ontime.Mediumdelay)
p.adjust(p5[["p.value"]], method = "bonferroni", n = 3) # Early vs Ontime in short condition 
p6 = t.test(PSEdata$Late.Mediumdelay,mu = 300)  
(mean(PSEdata$Late.Mediumdelay)-300)/sd(PSEdata$Late.Mediumdelay)
p.adjust(p6[["p.value"]], method = "bonferroni", n = 3) # Early vs Ontime in short condition 
p7 = t.test(PSEdata$Early.Longdelay,mu = 300) 
(mean(PSEdata$Early.Longdelay)-300)/sd(PSEdata$Early.Longdelay)
p.adjust(p7[["p.value"]], method = "bonferroni", n = 3) # Early vs Ontime in short condition 
p8 = t.test(PSEdata$Ontime.Longdelay,mu = 300) 
p.adjust(p8[["p.value"]], method = "bonferroni", n = 3) # Early vs Ontime in short condition 
p9 = t.test(PSEdata$Late.Longdelay,mu = 300) 
p.adjust(p9[["p.value"]], method = "bonferroni", n = 3) # Early vs Ontime in short condition 


