

library(lme4)
library(multcomp)
library(reshape)
library(plyr)
library(ggplot2)
library(lmerTest) 
library(emmeans)
library(sjPlot)
library(sjmisc)




# reading data into R
dataset = read.csv('RTs.data.csv', sep=",", header = T)


head(dataset)
summary(dataset)


# 
# converting categorical variables (i.e. condition) to a factor with two levels
#  1 = Direct Evidential, 2 = Indirect Evidential

dataset$condition <- factor(dataset$condition,
                            levels = c(1,2),
                            labels = c("Direct Evidential", "Indirect Evidential"))



## Assignment 1a: Do treatment/dummy contrast coding!! 

contrasts(dataset$condition) = contr.treatment(2, base = 1)
contrasts(dataset$condition)

## Assignment 1b: Do sum  contrast coding!! 

contrasts(dataset$condition) = contr.sum(2)   
contrasts(dataset$condition)

contrasts(dataset$condition) = -contr.sum(2)/2   
contrasts(dataset$condition)









# a quick visualisation 

hist(dataset$RTs, main="RTs")




# fit your first lmer

model1 = lmer(RTs ~ condition + (1|subject) + (1|item) , data=dataset)
summary(model1)



# interpreting fixed effects 

fixef(model1)


# interpreting random effects (ranef) = ranef returns the estimated deviation from the estimated average RTs
ranef(model1)


# plot actual vs. estimated data by subjects

ggplot() +
  geom_point(aes(x = dataset$subject, y = dataset$RTs),
             colour = 'red')+
  geom_point(aes(x = dataset$subject, y = predict(model1)),
             colour = 'blue') +
  ggtitle('Estimated vs. actual data') +
  xlab('Subject') +
  ylab('RTs')


# normalizing RTs data while running lmer

dataset$logRTs = log(dataset$RTs)
hist(dataset$logRTs)



model2 <- lmer( log(RTs) ~ condition + (1 |subject) + (1|item), data=dataset)
summary(model2)




# adding random slopes 

model3 <- lmer (log(RTs) ~ condition + (1 |subject) + (1 + age|item), data=dataset)

summary(model3)



model4 <- lmer (log(RTs) ~  condition + (1 |subject) + (1 + age |item), data=dataset)

summary(model4)





#compare models with anova 

anova(model1, model4)


#now let's inspect the distribution of residuals 
#This is not normal probably, but normality is not required for logistic regression
qqnorm (resid(model1))
qqline (resid(model1))




#Post-hocs

emmeans(model3, "condition")
pairs(emmeans(model3, "condition"))



model4 <- lmer (log(RTs) ~  condition * age + (1 |subject) + (1 |item), data=dataset)
summary(model4)

emmeans(model4, "condition", by = "age")
pairs(emmeans(model4, "condition", by = "age"))

plot_model(model4, type = "pred", terms = c("age", "condition"))


# optimising the model 

model5 <- lmer (log(RTs) ~  age * condition + (1 |subject) + (1 |item),
                control = lmerControl(optimizer='bobyqa'),
                data=dataset)

summary(model5)

control = lmerControl(optimizer='bobyqa')



# setting contrasts ? does it make a differnce? 

#we set the condition contrasts by 2 because we have two levels
contrasts(dataset$condition) = contr.sum(2)
contrasts(dataset$condition)


model5 <- lmer (log(RTs) ~  age * condition + (1 |subject) + (1 |item),
                control = lmerControl(optimizer='bobyqa'),
                data=dataset)
summary(model5)




