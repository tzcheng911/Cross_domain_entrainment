## Reshape my data
library(tidyr)
library(dplyr)
setwd('/Users/tzu-hancheng/Google_Drive/Research/entrainedVOT/pilot2018/results/outlierexclud_csv')

##### AUC of/pa/ response ####
pctpa = read.csv('/Users/tzu-hancheng/Google_Drive/Research/entrainedVOT/pilot2018/results/outlierexclud_csv/pctpa.csv')

## Run repeated measure ANOVA
pctpa_F = summary(aov(AUC ~ Onset + Error(subj/(Onset)) , data = pctpa))

## ttest in comparison onset (early, ontime, late)
pctpa_early = pctpa %>%
  filter(Onset == 'Early')
pctpa_ontime = pctpa %>%
  filter(Onset == 'Ontime')
pctpa_late = pctpa %>%
  filter(Onset == 'Late')

p1 = t.test(pctpa_early$AUC,pctpa_ontime$AUC, paired = TRUE)
p2 = t.test(pctpa_early$AUC,pctpa_late$AUC, paired = TRUE) #*
p3 = t.test(pctpa_ontime$AUC,pctpa_late$AUC, paired = TRUE)
p.adjust(p1[["p.value"]], method = "bonferroni", n = 3)
p.adjust(p2[["p.value"]], method = "bonferroni", n = 3)
p.adjust(p3[["p.value"]], method = "bonferroni", n = 3)

##### Lower bound ####
lowerbound = read.csv('/Users/tzu-hancheng/Google_Drive/Research/entrainedVOT/pilot2018/results/outlierexclud_csv/lowerbound.csv')

## Run repeated measure ANOVA
lowerbound_F = summary(aov(Lowerbound ~ Onset + Error(subj/(Onset)) , data = lowerbound))

## ttest in comparison onset (early, ontime, late)
lowerbound_early = lowerbound %>%
  filter(Onset == 'Early')
lowerbound_ontime = lowerbound %>%
  filter(Onset == 'Ontime')
lowerbound_late = lowerbound %>%
  filter(Onset == 'Late')
p1 = t.test(lowerbound_early$Lowerbound,lowerbound_ontime$Lowerbound, paired = TRUE)
p2 = t.test(lowerbound_early$Lowerbound,lowerbound_late$Lowerbound, paired = TRUE) #*
p3 = t.test(lowerbound_ontime$Lowerbound,lowerbound_late$Lowerbound, paired = TRUE)
p.adjust(p1[["p.value"]], method = "bonferroni", n = 3)
p.adjust(p2[["p.value"]], method = "bonferroni", n = 3)
p.adjust(p3[["p.value"]], method = "bonferroni", n = 3)

##### Upper bound ####
upperbound = read.csv('/Users/tzu-hancheng/Google_Drive/Research/entrainedVOT/pilot2018/results/outlierexclud_csv/upperbound.csv')

## Run repeated measure ANOVA
upperbound_F = summary(aov(Upperbound ~ Onset + Error(subj/(Onset)) , data = upperbound))

## ttest in comparison onset (early, ontime, late)
upperbound_early = upperbound %>%
  filter(Onset == 'Early')
upperbound_ontime = upperbound %>%
  filter(Onset == 'Ontime')
upperbound_late = upperbound %>%
  filter(Onset == 'Late')
p1 = t.test(upperbound_early$Upperbound,upperbound_ontime$Upperbound, paired = TRUE)
p2 = t.test(upperbound_early$Upperbound,upperbound_late$Upperbound, paired = TRUE) #*
p3 = t.test(upperbound_ontime$Upperbound,upperbound_late$Upperbound, paired = TRUE)
p.adjust(p1[["p.value"]], method = "bonferroni", n = 3)
p.adjust(p2[["p.value"]], method = "bonferroni", n = 3)
p.adjust(p3[["p.value"]], method = "bonferroni", n = 3)

##### Reflection point ####
inflectionpoint = read.csv('/Users/tzu-hancheng/Google_Drive/Research/entrainedVOT/pilot2018/results/outlierexclud_csv/inflectionpoint.csv')

## Run repeated measure ANOVA
inflectionpoint_F = summary(aov(EC50 ~ Onset + Error(subj/(Onset)) , data = inflectionpoint))

## ttest in comparison onset (early, ontime, late)
inflectionpoint_early = inflectionpoint %>%
  filter(Onset == 'Early')
inflectionpoint_ontime = inflectionpoint %>%
  filter(Onset == 'Ontime')
inflectionpoint_late = inflectionpoint %>%
  filter(Onset == 'Late')
p1 = t.test(inflectionpoint_early$EC50,inflectionpoint_ontime$EC50, paired = TRUE)
p2 = t.test(inflectionpoint_early$EC50,inflectionpoint_late$EC50, paired = TRUE) #*
p3 = t.test(inflectionpoint_ontime$EC50,inflectionpoint_late$EC50, paired = TRUE)
p.adjust(p1[["p.value"]], method = "bonferroni", n = 3)
p.adjust(p2[["p.value"]], method = "bonferroni", n = 3)
p.adjust(p3[["p.value"]], method = "bonferroni", n = 3)

##### Slope of the reflection point ####
Slope = read.csv('/Users/tzu-hancheng/Google_Drive/Research/entrainedVOT/pilot2018/results/outlierexclud_csv/slope.csv')

## Run repeated measure ANOVA
Slope_F = summary(aov(Slope ~ Onset + Error(subj/(Onset)) , data = Slope))

## ttest in comparison onset (early, ontime, late)
Slope_early = Slope %>%
  filter(Onset == 'Early')
Slope_ontime = Slope %>%
  filter(Onset == 'Ontime')
Slope_late = Slope %>%
  filter(Onset == 'Late')
p1 = t.test(Slope_early$Slope,Slope_ontime$Slope, paired = TRUE)
p2 = t.test(Slope_early$Slope,Slope_late$Slope, paired = TRUE) 
p3 = t.test(Slope_ontime$Slope,Slope_late$Slope, paired = TRUE)
p.adjust(p1[["p.value"]], method = "bonferroni", n = 3)
p.adjust(p2[["p.value"]], method = "bonferroni", n = 3)
p.adjust(p3[["p.value"]], method = "bonferroni", n = 3)
