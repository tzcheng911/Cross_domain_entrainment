## R plotting practice
# need to do preprocessing in entrainment_glmer or entrainment_logistic_fit

# plot logistic regression curve
ggplot(mtcars, aes(x=hp, y=vs)) + 
  geom_point(alpha=.5) +
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial))

ggplot(mydata, aes(x=Length, y=Shorter)) + 
  geom_point(alpha=.5) +
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial))

# plot logistic regression curve for individual
myplot=mydata %>% group_by(rLength,fOnset,sub_id,expt_id) %>% summarise(mShorter=mean(Shorter))
ggplot(myplot,aes(x=rLength,y=1-mShorter,color=fOnset))+
  scale_color_manual(values=c("red","blue","gray"))+
  geom_point()+
  # geom_line()+
  #geom_smooth(method="lm",formula=y ~ exp(x)/(1+exp(x)),se=FALSE)+
  geom_smooth(method="lm",se=FALSE) +
  facet_wrap(sub_id~.)

# bar plot  