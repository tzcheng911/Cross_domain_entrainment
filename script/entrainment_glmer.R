## load the data
mydataEXP4 = read.csv("/Users/t.z.cheng/Google_Drive/Research/cross_domain_entrainment/Delaydoesmatter/real_exp/exp4_20CR12/4c/results/EXP4c_clean_n59.csv") # exp4c
mydataEXP6 = read.csv("/Users/t.z.cheng/Google_Drive/Research/cross_domain_entrainment/exp6_21CR03_Vowel_length/FF2021/results/EXP6_clean_n64_v1.csv") # exp6 VL

# mydata = read.csv("/Users/t.z.cheng/Google_Drive/Research/Delaydoesmatter/real_exp/exp2/data_analysis/Rdata/exp2_PPS_n48.csv") # exp2
# mydata = read.csv("/Users/t.z.cheng/Google_Drive/Research/Delaydoesmatter/real_exp/exp3_20CR11/results/EXP3_clean_n34.csv") # exp3
# mydata = read.csv("/Users/t.z.cheng/Google_Drive/Research/Delaydoesmatter/real_exp/exp4_20CR12/4ab/results_shortdelay_2020/EXP4a_clean_n53.csv") # exp4a
# mydata = read.csv("/Users/t.z.cheng/Google_Drive/Research/Delaydoesmatter/real_exp/exp4_20CR12/4ab/results_shortlongdelay_2021/EXP4b_clean_n67.csv") # exp4b
# mydata = read.csv("/Users/t.z.cheng/Google_Drive/Research/cross_domain_entrainment/Delaydoesmatter/real_exp/exp4_20CR12/4c/results/EXP4c_clean_n59.csv") # exp4c
# mydata = read.csv("/Users/t.z.cheng/Google_Drive/Research/Delaydoesmatter/real_exp/exp5_21CR01/results/EXP5_clean_n24_e.csv") # exp5 empty
# mydata = read.csv("/Users/t.z.cheng/Google_Drive/Research/Delaydoesmatter/real_exp/exp5_21CR01/results/EXP5_clean_n24_f.csv") # exp5 filled
# mydata = read.csv("/Users/t.z.cheng/Google_Drive/Research/cross_domain_entrainment/exp6_21CR03_Vowel_length/FF2021/results/EXP6_clean_n64.csv") # exp6 VL

## flag the overlapping subjects
overlapsubj = intersect(mydataEXP4$sub_id,mydataEXP6$sub_id)
length(overlapsubj)
mydataEXP4$overlap = ifelse(Reduce(`|`, lapply(overlapsubj, `==`, mydataEXP4$sub_id)),1,0)
mydataEXP6$overlap = ifelse(Reduce(`|`, lapply(overlapsubj, `==`, mydataEXP6$sub_id)),1,0)

## combine the data
alldata=rbind(select(mydataEXP4,participant_id,sub_id,expt_id,Onset,Length,overlap,Shorter),select(mydataEXP4,participant_id,sub_id,expt_id,Onset,Length,overlap,Shorter))
alldata$Exptag=ifelse(alldata$expt_id=="620ddeee49655b42f1242551",1,-1)
alldata$Explabel=ifelse(alldata$expt_id=="620ddeee49655b42f1242551","Tones","Speech")
alldata = alldata %>%
  filter(alldata$overlap == 0) %>%
  droplevels

## Rescale and mutate new factors
alldata = alldata %>%
  mutate(rLength = scale(Length, center = TRUE, scale = TRUE)) # scale the steps ## 4c: scale Length; 6: scale comparison
alldata = alldata %>%
  mutate(fOnset = as.factor(Onset))

## Onset coding and reference 
alldata$fOnsetR = relevel(alldata$fOnset, ref="ontime") # make ontime condition the reference 
alldata$fOnsetE = relevel(alldata$fOnset, ref="early") # make ontime condition the reference 
alldata$OnsetNum=ifelse(alldata$fOnsetR=="early",-1,ifelse(alldata$fOnsetR=="ontime",0,1))
alldata$OnsetHelm1=ifelse(alldata$fOnsetR=="early",2/3,-1/3)
alldata$OnsetHelm2=ifelse(alldata$fOnsetR=="early",0,ifelse(alldata$fOnsetR=="ontime",1/2,-1/2))

## very flat slopes, maybe too hard to estimate
allbadsubs=c("394a9","5a2fc","6222b","6ae89","8be33","a4432","bf5f1","ce0e9","16b53","421ca","488cc","5d964","6933d","97e84","c3ee1") # by Zoe
alldata$badsubs=ifelse(alldata$sub_id %in% allbadsubs,"badsub","ok")

## glm: focus on the interaction between Onset and Exptag

# use early as the reference
lmall = glmer(Shorter ~ Exptag*fOnsetE*rLength + (1 + fOnsetE + rLength|sub_id),data= filter(alldata,badsubs =="ok"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
summary(lmall) # Use early as the reference
lmall_NoOnset = glmer(Shorter ~ Exptag*fOnsetE*rLength - fOnsetE:Exptag + (1 + fOnsetE + rLength|sub_id),data= filter(alldata,badsubs =="ok"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
summary(lmall) # Use ontime as the reference

# use ontime as the reference
lmallo = glmer(Shorter ~ Exptag*fOnsetR*rLength + (1 + fOnsetR + rLength|sub_id),data= filter(alldata,badsubs =="ok"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
summary(lmall) # Use early as the reference
lmallo_NoOnset = glmer(Shorter ~ Exptag*fOnsetR*rLength - fOnsetR:Exptag  + (1 + fOnsetR + rLength|sub_id),data= filter(alldata,badsubs =="ok"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
summary(lmall) # Use ontime as the reference

# use numerical code and ontime as the reference
lmalln = glmer(Shorter ~ Exptag*OnsetNum*rLength + (1 + OnsetNum + rLength|sub_id),data= filter(alldata,badsubs =="ok"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
summary(lmall) # Use early as the reference
lmalln_NoOnset = glmer(Shorter ~ Exptag*OnsetNum*rLength - Exptag:OnsetNum + (1 + OnsetNum + rLength|sub_id),data= filter(alldata,badsubs =="ok"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
summary(lmall) # Use ontime as the reference
anova(lmalln,lmalln_NoOnset)

# use helmert contrast to code 
lmallh = glmer(Shorter ~ Exptag*OnsetHelm1*rLength + Exptag*OnsetHelm2*rLength - Exptag:OnsetHelm1 - Exptag:OnsetHelm2
              + (1 + OnsetHelm1 + OnsetHelm2 + rLength|sub_id),
              data= filter(alldata,badsubs =="ok"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
summary(lmall) # Use early as the reference
lmallh_NoOnset = glmer(Shorter ~ Exptag*OnsetNum*rLength - Exptag:OnsetNum + (1 + OnsetNum + rLength|sub_id),data= filter(alldata,badsubs =="ok"),family="binomial", control = glmerControl(optimizer="bobyqa"), verbose=2)  
summary(lmall) # Use ontime as the reference

anova(lmall,lmall_NoOnset)




