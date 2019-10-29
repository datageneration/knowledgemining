# R Programming Lab (Model selection)
# Adapted from ISLR Chapter 6 Lab

# Best Subset Selection

library(ISLR)
hitters=Hitters
names(Hitters)
dim(Hitters)
# Check on missing values of the Salary variable
sum(is.na(Hitters$Salary))
# Remove missing values (deletion of cases)
Hitters=na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))
# Package leaps to perform subset selection in regresson
# install.packages("leaps")
library(leaps)

regfit.full=regsubsets(Salary~.,Hitters) # Start with full model
?regsubsets()
# Check on the method used among "exhaustive", "backward", "forward", "seqrep"
# Default is "exhaustive"
regfit.full$method  
regfit.full$nvmax  
names(regfit.full)

# Check on the model
# An asterisk indicates that a given variable is included in the corresponding model.
summary(regfit.full)

# Use the nvmax option fit 19 variables.
regfit.full=regsubsets(Salary~.,data=Hitters,nvmax=19)
reg.summary=summary(regfit.full)
names(reg.summary)
reg.summary$rsq
# Prepare for plots
par(mfrow=c(2,2))
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
which.max(reg.summary$adjr2)
points(11,reg.summary$adjr2[11], col="red",cex=2,pch=20)
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
which.min(reg.summary$cp)
points(10,reg.summary$cp[10],col="red",cex=2,pch=20)
which.min(reg.summary$bic)
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
points(6,reg.summary$bic[6],col="red",cex=2,pch=20)
plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")
coef(regfit.full,6)

# Forward and Backward Stepwise Selection

regfit.fwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="forward")
summary(regfit.fwd)
regfit.bwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="backward")
summary(regfit.bwd)
coef(regfit.full,7) #best seven-variable model
coef(regfit.fwd,7)  #best seven-variable model
coef(regfit.bwd,7)  #best seven-variable model

# Choosing Among Models

# Creating a random vector, train, of elements equal to TRUE if the corresponding observation is in the training set, and FALSE otherwise
# Replacement=TRUE
set.seed(1)
train=sample(c(TRUE,FALSE), nrow(Hitters),rep=TRUE)
# Checking train
head(train)
library(descr)
freq(train)
# The vector test has a TRUE if the observation is in the test set, and a FALSE otherwise.
test=(!train)
freq(test)
regfit.best=regsubsets(Salary~.,data=Hitters[train,],nvmax=19)

# Compute the validation set error for the best model of each model size. 
# First make a model matrix from the test data.
# model.matrix() function is used in many regression packages for building an “X” matrix from data.
test.mat=model.matrix(Salary~.,data=Hitters[test,])
# Create 19 missing values
# Run a loop, and for each size i, we extract the coefficients from regfit.best 
# for the best model of that size, multiply them into the appropriate columns of 
# the test model matrix to form the predictions, and compute the test MSE.


val.errors=rep(NA,19)
for(i in 1:19){
  coefi=coef(regfit.best,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((Hitters$Salary[test]-pred)^2)
}
val.errors
which.min(val.errors)
coef(regfit.best,10)

# There is no predict() method for regsubsets()
# Writing a predict function for cross-validation

predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

# Best subset selection on the full data set
regfit.best=regsubsets(Salary~.,data=Hitters,nvmax=19)
coef(regfit.best,10)

# Create a vector that allocates each observation to one of k = 10 folds
# Thwn create a matrix to store the results.
k=10
set.seed(1)
folds=sample(1:k,nrow(Hitters),replace=TRUE)
cv.errors=matrix(NA,k,19, dimnames=list(NULL, paste(1:19)))

# In the jth fold, the elements of folds that equal j are in the test set, 
# and the remainder are in the training set.
for(j in 1:k){
  best.fit=regsubsets(Salary~.,data=Hitters[folds!=j,],nvmax=19)
  for(i in 1:19){
    pred=predict(best.fit,Hitters[folds==j,],id=i)
    cv.errors[j,i]=mean( (Hitters$Salary[folds==j]-pred)^2)
  }
}
mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors,type='b') # Which is best model?

reg.best=regsubsets(Salary~.,data=Hitters, nvmax=19)
coef(reg.best,10)