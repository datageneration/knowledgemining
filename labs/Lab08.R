# R Programming Lab (Resampling methods)
# Adapted from ISLR Chapter 5 Lab

require(ISLR)
require(boot)
?Auto

attach(Auto)  # continuous in variables on both sides
plot(mpg~horsepower,data=Auto)
plot(mpg, horsepower, pch=16, col="dodgerblue", cex=1,xlab="mpg",ylab="Horsepower")

#CV
set.seed(1)
train=sample(392,196)
NROW(train)

lm.fit=lm(mpg~horsepower, data=Auto,subset=train)
mean((mpg-predict(lm.fit, Auto))[-train]^2) 

## LOOCV
?cv.glm
glm.fit=glm(mpg~horsepower, data=Auto)
cv.glm(Auto,glm.fit)$delta #pretty slow (doesnt use formula (5.2) on page 180)

##Lets write a simple function to use formula (5.2)
loocv=function(fit){
  h=lm.influence(fit)$h
  mean((residuals(fit)/(1-h))^2)
}

## Now we try it out
loocv(glm.fit)

cv.error=rep(0,5)
degree=1:5

# For loop
for(d in degree){
  glm.fit=glm(mpg~poly(horsepower,d), data=Auto)
  cv.error[d]=loocv(glm.fit)
}
plot(degree,cv.error,type="b")

## 10-fold CV

cv.error10=rep(0,5)
for(d in degree){
  glm.fit=glm(mpg~poly(horsepower,d), data=Auto)
  cv.error10[d]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}
lines(degree,cv.error10,type="b",col="red")

# Try different K?

## Bootstrap
## Minimum risk investment - Section 5.2
?Portfolio
pf=Portfolio

attach(Portfolio)
alpha=function(x,y){
  vx=var(x)
  vy=var(y)
  cxy=cov(x,y)
  (vy-cxy)/(vx+vy-2*cxy)
}
alpha(Portfolio$X,Portfolio$Y)

## What is the standard error of alpha?

alpha.fn=function(data, index){
  with(data[index,],alpha(X,Y))
}

alpha.fn(Portfolio,1:100)

set.seed(1)
alpha.fn (Portfolio,sample(1:100,100,replace=TRUE))

boot.out=boot(Portfolio,alpha.fn,R=1000)
boot.out
plot(boot.out)
names(boot.out)
