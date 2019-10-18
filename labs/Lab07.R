# R Programming Lab (Linear Discrimination Analysis)
# Adapted from ISLR Chapter 4 Lab

require(ISLR)
require(MASS)
require(descr)
attach(Smarket)
## Linear Discriminant Analysis
freq(Direction)
lda.fit=lda(Direction~Lag1+Lag2,data=Smarket, subset=Year<2005)
lda.fit
plot(lda.fit, col="dodgerblue")
Smarket.2005=subset(Smarket,Year==2005)
lda.pred=predict(lda.fit,Smarket.2005)
names(lda.pred)
lda.class=lda.pred$class
table(lda.class,Direction.2005) # what is Direction.2005?
data.frame(lda.pred)[1:5,]
table(lda.pred$class,Smarket.2005$Direction)
mean(lda.pred$class==Smarket.2005$Direction)


