# R Programming (base)
# Adapted from ISLR Chapter 2 Lab: Introduction to R

# Basic Commands

# Create object using the assignment operator (<-, =)
x <- c(1,3,2,5)
x
x = c(1,6,2)
x
y = c(1,4,3)

# Using function

length(x)  # What does length() do?
length(y)

# Using +, -, *, /,^ operators
x+y


ls() # List objects in the environment
rm(x,y) # Remove objects
ls()
rm(list=ls()) # Danger! What does this do?

# Matrix operations

?matrix
x=matrix(data=c(1,2,3,4), nrow=2, ncol=2) # Create a 2x2 matrix object
x
x=matrix(c(1,2,3,4),2,2)
matrix(c(1,2,3,4),2,2,byrow=TRUE) # What about byrow=F?

sqrt(x) # What does x look like?

x^2

x=rnorm(50) # Generate a vector of 50 numbers using the rnorm() function

y=x+rnorm(50,mean=50,sd=.1) # What does rnorm(50,mean=50,sd=.1) generate?

cor(x,y) # Correlation of x and y
set.seed(1303) # Set the seed for Random Number Generator (RNG) to generate values that are reproducible.
rnorm(50)
set.seed(3) # Try different seeds?
y=rnorm(100)

# Simple descriptive statistics

mean(y)
var(y)
sqrt(var(y))
sd(y)

# Graphics using R Graphics (without packages)

x=rnorm(100)
y=rnorm(100)
plot(x,y) # Scatterplot for two numeric variables by default
plot(x,y,xlab="this is the x-axis",ylab="this is the y-axis",main="Plot of X vs Y") # Add labels
pdf("Figure.pdf") # Save as pdf, add a path or it will be stored on the project directory
plot(x,y,col="green") # Try different colors?
dev.off() # Close the file using the dev.off function
x=seq(1,10) # Same as x=c(1:10)
x
x=1:10
x
x=seq(-pi,pi,length=50)
y=x

## Fancy graphs: contour, image, persp (3D)
f=outer(x,y,function(x,y)cos(y)/(1+x^2))
contour(x,y,f)
contour(x,y,f,nlevels=45,add=T)
fa=(f-t(f))/2
contour(x,y,fa,nlevels=15)
image(x,y,fa)
persp(x,y,fa)
persp(x,y,fa,theta=30)
persp(x,y,fa,theta=30,phi=20)
persp(x,y,fa,theta=30,phi=70)
persp(x,y,fa,theta=30,phi=40)


