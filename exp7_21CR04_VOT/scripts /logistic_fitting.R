# Code snippet
library(dr4pl)
library(tidyverse)
#dummy <- data.frame(votstep = c(10^1,10^2,10^3,10^4,10^5,10^6), pctpa = c(0.118055556,0.135416667,0.378472222,0.829861111,0.975694444,0.986111111))
#dummy <- data.frame(votstep = c(exp(1),exp(2),exp(3),exp(4),exp(5),exp(6)), pctpa = c(0.118055556,0.135416667,0.378472222,0.829861111,0.975694444,0.986111111))
pctpa = c(0,0,0,1,1,1)
dummy <- data.frame(votstep = c(1,2,3,4,5,6), pctpa)

attempt<- dr4pl(dose = dummy$votstep,
                response = dummy$pctpa,
                method.init = "logistic")
attempt
plot(attempt)
abcd[n]=attempt$parameters[2]
abcd

########## loop over each subjects and conditions 
library(dr4pl)
library(tidyverse)

setwd("/Users/tzu-hancheng/Google_Drive/Research/cross-domain entrainment/results")
mydata = read.csv('pctpa.csv')
EC <- vector("list",nrow(mydata))

for (n in 1:nrow(mydata)){
a = mydata[n,3:8]
aa = t(a)
dummy <- data.frame(votstep = c(1,2,3,4,5,6), pctpa = aa[,1])

attempt<- dr4pl(dose = dummy$votstep,
                response = dummy$pctpa,
                method.init = "logistic")
EC[[n]] <-attempt$parameters[2] # which of the four parameters do you want
}
write.csv(EC, file = "Reflectionpoint.csv")
EC = as.numeric(unlist(EC))
