getwd()
setwd("C:/Users/linli/OneDrive - Singapore University of Technology and Design/Term 5/40.016 The Anal Edge/Week2/Class2_LogReg1/orings")
orings <- read.csv("Orings.csv")
str(orings)
summary(orings)
table(orings)
tapply(orings$Field, orings$Flight, sum)
#orings$Field is the variable that I am summing.
#we group by each flight hence orings$FLight
# we sum up the flights
table(tapply(orings$Field, orings$Flight, sum))
plot(orings$Temp[orings$Field==1]) #visualise temperature that orings that failed
plot(orings$Temp[orings$Field==0]) #visualise temperature that orings did not fail

plot(orings$Temp, orings$Field)
# add some random noise to tempature cause each obs values are too close to see the trend

plot(jitter(orings$Temp), orings$Field)
# from the plot, we can already see that linear regression is not a good fit, but we apply it for verification purposes.

m1 <- lm(Field~Temp, data =orings)
summary(m1)

#Field = 0.79 - 0.0104 Temp, if temperature increases, Field tends to 0, wont fail.
# Bu the R squared is very low
abline(m1)

#now we look at other variables, such as pressure

m2 <- lm(Field~Pres, data=orings)
summary(m2)
abline(m2)
#pressure is not statistically significant.

#Build joint model for temp and pressure

m3<- lm(Field~Pres+Temp, data=orings)
summary(m3)
#R squared is a little better but still pretty bad.


#logistic regression model fitting
?glm
m4<- glm(Field~Temp, data=orings, family=binomial) #specify the family=binomial for logistics regression
summary(m4)
#find log likelihood
logLik(m4)


m5<- glm(Field~Pres, data=orings, family=binomial)
summary(m5)


#Joint model

m6 <- glm(Field~Pres+Temp, data=orings, family=binomial)
summary(m6)
logLik(m6)
plot(jitter(orings$Temp, orings$Field))
curve(exp(6.75183-0.13971*x)/(1+exp(6.75183-0.13971*x)),add=T)

predict(m4, newdata=orings[144,])
#not a probability, if we want the probability...
6.75-0.139*(31)
#now we compute the probability of oring failing at temperature =31

predict(m4, newdata=orings[144,], type="response")
# High probability that oring fail at temperature of 31 farenheit. But it still depends on the risk preference. It might not be considered high risk

#Confusion Matrix: to show FN, TN, FP, TP
Pred <- predict(m3,newdata=orings,type="response")
Pred # Why is the confusion matrix all the way to index 7? #How do i read it
table(Pred[1:138] >0.5, orings$Field[1:138])
table(as.integer(Pred[1:138]>0.5), orings$Field[1:138])
table(as.integer(Pred[1:138]>0.25), orings$Field[1:138])
#Lowering Threshold to 0.20
table(as.integer(Pred[1:138]>0.20), orings$Field[1:138])



#ROCR curve package
install.packages("ROCR")
library(ROCR)
# the ROCR package tries the threshold in a continuous way
# So it plot a ROC curve
ROCRpred <- prediction(Pred[1:138],orings$Field[1:138])
ROCRperf <- performance(ROCRpred,x.measure="fpr",measure="tpr")
plot(ROCRperf)

aucperf <- performance(ROCRpred,measure="auc")
str(aucperf)
aucperf@y.values
