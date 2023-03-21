## Reshape my data
library(effsize)
library(tidyr)
library(dplyr)
setwd('/Users/t.z.cheng/Google_Drive/Research/Delaydoesmatter/real_exp/exp1/data_analysis/Rdata')

##### Accuracy ####
Accdata = read.csv('/Users/t.z.cheng/Google_Drive/Research/cross_domain_entrainment/Delaydoesmatter/real_exp/exp1/data_analysis/Rdata/Accuracy.csv')
long_Accdata <- gather(Accdata,BD,Accuracy,Early.Shortdelay:Late.Longdelay)
separate_Accdata <- separate(long_Accdata, BD, c("Comparison_onsets", "Delay_length"))

## Run repeated measure ANOVA
# main effect & interaction
Acc_F = summary(aov(Accuracy ~ Comparison_onsets * Delay_length + Error(subject/(Comparison_onsets * Delay_length)), data = separate_Accdata))

# Simple main effect
Acc_Long = separate_Accdata %>%
  filter(Delay_length =='Longdelay')
p1 = summary(aov(Accuracy ~ Comparison_onsets + Error(subject/(Comparison_onsets)), data = Acc_Long))
p.adjust(p1$'Error: subject:Comparison_onsets'[[1]]$'Pr(>F)'[1], method = "bonferroni", n = 2)
Acc_Short = separate_Accdata %>%
  filter(Delay_length =='Shortdelay')
p2 = summary(aov(Accuracy ~ Comparison_onsets + Error(subject/(Comparison_onsets)), data = Acc_Short))
p.adjust(p2$'Error: subject:Comparison_onsets'[[1]]$'Pr(>F)'[1], method = "bonferroni", n = 2)

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

## ttest in delay length (short, long)
p1 = t.test(Accdata$ShortDelay,Accdata$LongDelay, paired = TRUE)
p.adjust(p1[["p.value"]], method = "bonferroni", n = 2)

## Pairwise comparison for interaction effects
# Accuracy
# Short condition 
p1 = t.test(Accdata$Early.Shortdelay,Accdata$Ontime.Shortdelay, paired = TRUE) # Early vs Ontime in short condition 
cohen.d(Accdata$Early.Shortdelay,Accdata$Ontime.Shortdelay)
p2 = t.test(Accdata$Early.Shortdelay,Accdata$Late.Shortdelay, paired = TRUE) # Early vs Late in short condition
cohen.d(Accdata$Early.Shortdelay,Accdata$Late.Shortdelay)
p3 = t.test(Accdata$Ontime.Shortdelay,Accdata$Late.Shortdelay, paired = TRUE) # Late vs Ontime in short condition
cohen.d(Accdata$Ontime.Shortdelay,Accdata$Late.Shortdelay)

# Long condition
p4 = t.test(Accdata$Early.Longdelay,Accdata$Ontime.Longdelay, paired = TRUE) # Early vs Ontime in long condition
p5 = t.test(Accdata$Early.Longdelay,Accdata$Late.Longdelay, paired = TRUE) # Early vs Late in long condition
p6 = t.test(Accdata$Ontime.Longdelay,Accdata$Late.Longdelay, paired = TRUE) # Late vs Ontime in long condition
p.adjust(p1[["p.value"]], method = "bonferroni", n = 3) # Early vs Ontime in short condition 
p.adjust(p2[["p.value"]], method = "bonferroni", n = 3) # Early vs Late in short condition 
p.adjust(p3[["p.value"]], method = "bonferroni", n = 3) # Late vs Ontime in short condition 
p.adjust(p4[["p.value"]], method = "bonferroni", n = 3) # Early vs Ontime in long condition 
p.adjust(p5[["p.value"]], method = "bonferroni", n = 3) # Early vs Late in long condition 
p.adjust(p6[["p.value"]], method = "bonferroni", n = 3) # Late vs Ontime in long condition 

##### Proportion Short ####
Proportiondata = read.csv('/Users/t.z.cheng/Google_Drive/Research/Delaydoesmatter/real_exp/exp1/data_analysis/Rdata/Proportion_short.csv')
long_Proportiondata <- gather(Proportiondata,BD,Proportion_short,Early.Shortdelay:Late.Longdelay)
separate_Proportiondata <- separate(long_Proportiondata, BD, c("Comparison_onsets", "Delay_length"))

## Run repeated measure ANOVA
# main effect & interaction
Proportion_F = summary(aov(Proportion_short ~ Comparison_onsets * Delay_length + Error(subject/(Comparison_onsets * Delay_length)), data = separate_Proportiondata))

# Simple main effect
Prop_Long = separate_Proportiondata %>%
  filter(Delay_length =='Longdelay')
p1 = summary(aov(Proportion_short ~ Comparison_onsets + Error(subject/(Comparison_onsets)), data = Prop_Long))
p.adjust(p1$'Error: subject:Comparison_onsets'[[1]]$'Pr(>F)'[1], method = "bonferroni", n = 2)
Prop_Short = separate_Proportiondata %>%
  filter(Delay_length =='Shortdelay')
p2 = summary(aov(Proportion_short ~ Comparison_onsets + Error(subject/(Comparison_onsets)), data = Prop_Short))
p.adjust(p2$'Error: subject:Comparison_onsets'[[1]]$'Pr(>F)'[1], method = "bonferroni", n = 2)

## ttest in comparison onset (early, ontime, late)
p1 = t.test(Proportiondata$Early,Proportiondata$Ontime, paired = TRUE)
cohen.d(Proportiondata$Early,Proportiondata$Ontime)
p2 = t.test(Proportiondata$Early,Proportiondata$Late, paired = TRUE)
cohen.d(Proportiondata$Early,Proportiondata$Late)
p3 = t.test(Proportiondata$Late,Proportiondata$Ontime, paired = TRUE)
cohen.d(Proportiondata$Ontime,Proportiondata$Late)
p.adjust(p1[["p.value"]], method = "bonferroni", n = 3)
p.adjust(p2[["p.value"]], method = "bonferroni", n = 3)
p.adjust(p3[["p.value"]], method = "bonferroni", n = 3)

## ttest in delay length (short, long)
p1 = t.test(Proportiondata$ShortDelay,Proportiondata$LongDelay, paired = TRUE)
p.adjust(p1[["p.value"]], method = "bonferroni", n = 2)

## Pairwise comparison for interaction effects
# Short condition 
p1 = t.test(Proportiondata$Early.Shortdelay,Proportiondata$Ontime.Shortdelay, paired = TRUE) # Early vs Ontime in short condition 
cohen.d(Proportiondata$Early.Shortdelay,Proportiondata$Ontime.Shortdelay)
p2 = t.test(Proportiondata$Early.Shortdelay,Proportiondata$Late.Shortdelay, paired = TRUE) # Early vs Late in short condition
cohen.d(Proportiondata$Early.Shortdelay,Proportiondata$Late.Shortdelay)
p3 = t.test(Proportiondata$Ontime.Shortdelay,Proportiondata$Late.Shortdelay, paired = TRUE) # Late vs Ontime in short condition
cohen.d(Proportiondata$Ontime.Shortdelay,Proportiondata$Late.Shortdelay)
# Long condition
p4 = t.test(Proportiondata$Early.Longdelay,Proportiondata$Ontime.Longdelay, paired = TRUE) # Early vs Ontime in short condition
p5 = t.test(Proportiondata$Early.Longdelay,Proportiondata$Late.Longdelay, paired = TRUE) # Early vs Late in short condition
p6 = t.test(Proportiondata$Ontime.Longdelay,Proportiondata$Late.Longdelay, paired = TRUE) # Late vs Ontime in short condition
p.adjust(p1[["p.value"]], method = "bonferroni", n = 3) # Early vs Ontime in short condition 
p.adjust(p2[["p.value"]], method = "bonferroni", n = 3) # Early vs Late in short condition 
p.adjust(p3[["p.value"]], method = "bonferroni", n = 3) # Late vs Ontime in short condition 
p.adjust(p4[["p.value"]], method = "bonferroni", n = 3) # Early vs Ontime in long condition 
p.adjust(p5[["p.value"]], method = "bonferroni", n = 3) # Early vs Late in long condition 
p.adjust(p6[["p.value"]], method = "bonferroni", n = 3) # Late vs Ontime in long condition 

##### PSE ####
PSEdata = read.csv('/Users/t.z.cheng/Google_Drive/Research/Delaydoesmatter/real_exp/exp1/data_analysis/Rdata/PSE.csv')
long_PSEdata <- gather(PSEdata,BD,PSE,Early.Shortdelay:Late.Longdelay)
separate_PSEdata <- separate(long_PSEdata, BD, c("Comparison_onsets", "Delay_length"))

## Run repeated measure ANOVA
# main effect & interaction
PSE_F = summary(aov(PSE ~ Comparison_onsets * Delay_length + Error(subject/(Comparison_onsets * Delay_length)), data = separate_PSEdata))

# Simple main effect
PSE_Long = separate_PSEdata %>%
  filter(Delay_length =='Longdelay')
p1 = summary(aov(PSE ~ Comparison_onsets + Error(subject/(Comparison_onsets)), data = PSE_Long))
p.adjust(p1$'Error: subject:Comparison_onsets'[[1]]$'Pr(>F)'[1], method = "bonferroni", n = 2)
PSE_Short = separate_PSEdata %>%
  filter(Delay_length =='Shortdelay')
p2 = summary(aov(PSE ~ Comparison_onsets + Error(subject/(Comparison_onsets)), data = PSE_Short))
p.adjust(p2$'Error: subject:Comparison_onsets'[[1]]$'Pr(>F)'[1], method = "bonferroni", n = 2)

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

## ttest in delay length (short, long)
p1 = t.test(PSEdata$ShortDelay,PSEdata$LongDelay, paired = TRUE)
p.adjust(p1[["p.value"]], method = "bonferroni", n = 2)

## Pairwise comparison for interaction effects
# Short condition 
p1 = t.test(PSEdata$Early.Shortdelay,PSEdata$Ontime.Shortdelay, paired = TRUE) # Early vs Ontime in short condition 
cohen.d(PSEdata$Early.Shortdelay,PSEdata$Ontime.Shortdelay)
p2 = t.test(PSEdata$Early.Shortdelay,PSEdata$Late.Shortdelay, paired = TRUE) # Early vs Late in short condition
cohen.d(PSEdata$Early.Shortdelay,PSEdata$Late.Shortdelay)
p3 = t.test(PSEdata$Ontime.Shortdelay,PSEdata$Late.Shortdelay, paired = TRUE) # Late vs Ontime in short condition
cohen.d(PSEdata$Ontime.Shortdelay,PSEdata$Late.Shortdelay)
# Long condition
p4 = t.test(PSEdata$Early.Longdelay,PSEdata$Ontime.Longdelay, paired = TRUE) # Early vs Ontime in short condition
cohen.d(PSEdata$Early.Longdelay,PSEdata$Ontime.Longdelay)
p5 = t.test(PSEdata$Early.Longdelay,PSEdata$Late.Longdelay, paired = TRUE) # Early vs Late in short condition
p6 = t.test(PSEdata$Ontime.Longdelay,PSEdata$Late.Longdelay, paired = TRUE) # Late vs Ontime in short condition
p.adjust(p1[["p.value"]], method = "bonferroni", n = 3) # Early vs Ontime in short condition 
p.adjust(p2[["p.value"]], method = "bonferroni", n = 3) # Early vs Late in short condition 
p.adjust(p3[["p.value"]], method = "bonferroni", n = 3) # Late vs Ontime in short condition 
p.adjust(p4[["p.value"]], method = "bonferroni", n = 3) # Early vs Ontime in long condition 
p.adjust(p5[["p.value"]], method = "bonferroni", n = 3) # Early vs Late in long condition 
p.adjust(p6[["p.value"]], method = "bonferroni", n = 3) # Late vs Ontime in long condition 

# one sample ttest to test if the responses are different from 600
p1 = t.test(PSEdata$Early.Shortdelay,mu = 600) 
(mean(PSEdata$Early.Shortdelay)-600)/sd(PSEdata$Early.Shortdelay) # cohen's d
p.adjust(p1[["p.value"]], method = "bonferroni", n = 3) # Early vs Ontime in short condition 
p2 = t.test(PSEdata$Ontime.Shortdelay,mu = 600) 
(mean(PSEdata$Ontime.Shortdelay)-600)/sd(PSEdata$Ontime.Shortdelay)
p.adjust(p2[["p.value"]], method = "bonferroni", n = 3) # Early vs Ontime in short condition 
p3 = t.test(PSEdata$Late.Shortdelay,mu = 600)  
(mean(PSEdata$Late.Shortdelay)-600)/sd(PSEdata$Late.Shortdelay)
p.adjust(p3[["p.value"]], method = "bonferroni", n = 3) # Early vs Ontime in short condition 
p4 = t.test(PSEdata$Early.Longdelay,mu = 600) 
(mean(PSEdata$Early.Longdelay)-600)/sd(PSEdata$Early.Longdelay)
p.adjust(p4[["p.value"]], method = "bonferroni", n = 3) # Early vs Ontime in short condition 
p5 = t.test(PSEdata$Ontime.Longdelay,mu = 600) 
p.adjust(p5[["p.value"]], method = "bonferroni", n = 3) # Early vs Ontime in short condition 
p6 = t.test(PSEdata$Late.Longdelay,mu = 600) 
p.adjust(p6[["p.value"]], method = "bonferroni", n = 3) # Early vs Ontime in short condition 

