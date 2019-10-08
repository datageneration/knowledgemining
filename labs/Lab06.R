# R Programming Lab (Logistic Regression)
# Adapted from ISLR Chapter 4 Lab

# Load ISLR library

require(ISLR)

# Check dataset Smarket
?Smarket
names(Smarket)
summary(Smarket)

# Create a dataframe for data browsing
sm=Smarket

# Plot the data with smaller dots
pairs(Smarket,col=Smarket$Direction,cex=.5)

# Logistic regression
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
            data=Smarket,family=binomial)
summary(glm.fit)
glm.probs=predict(glm.fit,type="response") 
glm.probs[1:5]
glm.pred=ifelse(glm.probs>0.5,"Up","Down")
attach(Smarket)
table(glm.pred,Direction)
mean(glm.pred==Direction)
# Make training and test set
train = Year<2005
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
            data=Smarket,family=binomial, subset=train)
glm.probs=predict(glm.fit,newdata=Smarket[!train,],type="response") 
glm.pred=ifelse(glm.probs >0.5,"Up","Down")
Direction.2005=Smarket$Direction[!train]
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
#Fit smaller model
glm.fit=glm(Direction~Lag1+Lag2,
            data=Smarket,family=binomial, subset=train)
glm.probs=predict(glm.fit,newdata=Smarket[!train,],type="response") 
glm.pred=ifelse(glm.probs >0.5,"Up","Down")
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)

# Check accuracy rate
106/(76+106)





## Load datasets from MASS and ISLR packages

install.packages("arm") # For coefficient plots
library(arm)
library(MASS)
library(ISLR)

attach(Boston)

### Simple linear regression
names(Boston)
# What is the Boston dataset?
?Boston
plot(medv~lstat,Boston)
fit1=lm(medv~lstat,data=Boston)
fit1
summary(fit1)
abline(fit1,col="red")
names(fit1)
confint(fit1)
predict(fit1,data.frame(lstat=c(0,5,10,15)),interval="confidence")
### Multiple linear regression
fit2=lm(medv~lstat+age,data=Boston)
summary(fit2)
fit3=lm(medv~.,Boston)
summary(fit3)
par(mfrow=c(2,2))
plot(fit3)

# Update function to respecify the model, i.e. remove age and indus variables
fit4=update(fit3,~.-age-indus)
summary(fit4)

# set the next plot configuration

par(mfrow=c(2,2))
plot(fit4)

par(mfrow=c(1,1))
coefplot(fit4)
### Nonlinear terms and Interactions
fit5=lm(medv~lstat*age,Boston)
summary(fit5)
## I() identity function for squared term to interpret as-is
## Combine two command lines with semicolon
fit6=lm(medv~lstat +I(lstat^2),Boston); summary(fit6)
attach(Boston)
par(mfrow=c(1,1))
plot(medv~lstat)
points(lstat,fitted(fit6),col="red",pch=20)
fit7=lm(medv~poly(lstat,4))
points(lstat,fitted(fit7),col="blue",pch=20)
## Show all plotting characters (pch) in once chart, double in size (cex=2)
plot(1:20,1:20,pch=1:20,cex=2)
###Qualitative predictors
fix(Carseats)
names(Carseats)
summary(Carseats)
fit1=lm(Sales~.+Income:Advertising+Age:Price,Carseats)
summary(fit1)
contrasts(Carseats$ShelveLoc) # what is contrasts function/
###Writing R functions
## Combine the lm, plot and abline functions to create a one step regression fit plot function
regplot=function(x,y){
  fit=lm(y~x)
  plot(x,y)
  abline(fit,col="red")
}
attach(Carseats)
regplot(Price,Sales)
## Allow extra room for additional arguments/specifications
regplot=function(x,y,...){
  fit=lm(y~x)
  plot(x,y,...)
  abline(fit,col="red")
}
regplot(Price,Sales,xlab="Price",ylab="Sales",col="blue",pch=20)

## Additional note: try out the coefplot2 package to finetune the coefplots
##install.packages("coefplot2", repos="http://www.math.mcmaster.ca/bolker/R", type="source")
## library(coefplot2)

# Exercise 
# Try other combination of interactive terms
# How to interpret interactive terms?
# Read: Brambor, T., Clark, W.R. and Golder, M., 2006. Understanding interaction models: Improving empirical analyses. Political analysis, 14(1), pp.63-82.
# What are qualitative variables?  What class should they be? 




