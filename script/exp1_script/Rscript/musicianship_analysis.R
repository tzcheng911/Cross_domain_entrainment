## Two ways to test if the musicianship influences the accuracy of time perception task
# 1. parametric ANCOVA test
# 2. non-parametric permutation test

## Reshape my data
library(tidyr)
mydata = read.csv('/Users/tzu-hancheng/Google_Drive/Research/Manuscript/Time_perception/results/Rdata/music/Accuracy.csv')
Proportiondata = read.csv('/Users/tzu-hancheng/Google_Drive/Research/Manuscript/Time_perception/results/Rdata/music/Proportion_short.csv')
PSEdata = read.csv('/Users/tzu-hancheng/Google_Drive/Research/Manuscript/Time_perception/results/Rdata/music/PSE.csv')

mydata$Music = (scale(mydata$Music))
long_mydata <- gather(mydata,BD,Accuracy,Early.Shortdelay:Late.Longdelay)
separate_mydata <- separate(long_mydata, BD, c("Comparison_onsets", "Delay_length"))

Proportiondata$Music = (scale(Proportiondata$Music))
long_Proportiondata <- gather(Proportiondata,BD,Proportion_short,Early.Shortdelay:Late.Longdelay)
separate_Proportiondata <- separate(long_Proportiondata, BD, c("Comparison_onsets", "Delay_length"))

PSEdata$Music = (scale(PSEdata$Music))
long_PSEdata <- gather(PSEdata,BD,PSE,Early.Shortdelay:Late.Longdelay)
separate_PSEdata <- separate(long_PSEdata, BD, c("Comparison_onsets", "Delay_length"))


## Run ANCOVA on real data
Accuracy_result = summary(aov(Accuracy ~ Comparison_onsets * Delay_length * Music + Error(subject/(Comparison_onsets * Delay_length)), data = separate_mydata))
fit <- aov(Accuracy ~ Comparison_onsets * Delay_length * Music + Error(subject/(Comparison_onsets * Delay_length)), data = separate_mydata)
eta_sq(fit,partial = TRUE)
Proportion_result = summary(aov(Proportion_short ~ Comparison_onsets * Delay_length * Music + Error(subject/(Comparison_onsets * Delay_length)), data = separate_Proportiondata))
PSE_result = summary(aov(PSE ~ Comparison_onsets * Delay_length * Music + Error(subject/(Comparison_onsets * Delay_length)), data = separate_PSEdata))
# result2 = summary(lm(Accuracy ~ Comparison_onsets * Delay_length * Music, data = separate_mydata))
# real_value <- result_ANCOVA$'Error: subject'[[1]]$'F value'[1]
 
 real_value <- Accuracy_result$'Error: subject'[[1]]$'Pr(>F)'[1]



## Run permutation 
# set up variable to collect 1000 sample test values (p, F, whatever)
teststat=rep(-1,1000) # initialize to -1, an impossible value for p or F, so failures to update this vector are easily spotted
iterations=1000
# for loop, for 1000 sample iterations
for (i in 1:iterations){
	# scramble music experience level across subjects
	# separate_mydata$ScramMusic=sample(scale(separate_mydata$Music),replace=F) # don't replace.
	# analyze the resulting data frame
		# anova function is aov(); it can contain a continuous factor
		# scale() the continuous factor to be extra cautious
		mydata$ScramMusic=sample(scale(mydata$Music),replace=F)
		long_mydata <- gather(mydata,BD,Accuracy,Early.Shortdelay:Late.Longdelay)
		separate_mydata <- separate(long_mydata, BD, c("Comparison_onsets", "Delay_length"))
	# DV ~ IV1 * IV2 * IV3 + Error(Subject/( IV1 * IV2 * IV3 )) --check direction of error term
	  result=summary(aov(Accuracy ~ Comparison_onsets * Delay_length * ScramMusic + Error(subject/(Comparison_onsets * 		  
	  Delay_length)), data = separate_mydata))
	
	
	# extract p or F value of interest: summary(result)[1]F.value[1] 
	 teststat[i] <- result$'Error: subject'[[1]]$'Pr(>F)'[1]
	#teststat[i] <- result$'Error: subject'[[1]]$'F value'[1]
}
	
# Figure out the 95% upper bound of this distribution
lower_bound = sort(teststat)[50]
upper_bound = sort(teststat)[950]

# histogram of teststat
hist(teststat)

# add lines at the 5% lowerbound, 95% upperbound, and our observed value
abline(v = lower_bound, col = "blue", lwd = 1)
abline(v = upper_bound, col = "blue", lwd = 1)
abline(v = real_value, col = "red", lwd = 1)