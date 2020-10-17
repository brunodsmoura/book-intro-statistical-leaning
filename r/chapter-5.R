# 5.3.1 The Validation Set Approach
library(ISLR)
set.seed(1)
train <- sample(392, 196)

attach(Auto)

lm.fit <- lm(mpg~horsepower, data=Auto, subset = train)
mean((mpg-predict(lm.fit, Auto))[-train]^2)

lm.fit2 <- lm(mpg~poly(horsepower,2), data=Auto, subset = train)
mean((mpg-predict(lm.fit2, Auto))[-train]^2)

lm.fit3 <- lm(mpg~poly(horsepower,3), data=Auto, subset = train)
mean((mpg-predict(lm.fit3, Auto))[-train]^2)

# Different sample (setting seed)
set.seed(2)
train <- sample(392,196)

lm.fit <- lm(mpg~horsepower, data=Auto, subset = train)
mean((mpg-predict(lm.fit, Auto))[-train]^2)

lm.fit2 <- lm(mpg~poly(horsepower,2), data=Auto, subset = train)
mean((mpg-predict(lm.fit2, Auto))[-train]^2)

lm.fit3 <- lm(mpg~poly(horsepower,3), data=Auto, subset = train)
mean((mpg-predict(lm.fit3, Auto))[-train]^2)

# 5.3.2 Leave-One-Out Cross-Validation
glm.fit <- glm(mpg~horsepower, data=Auto)
coef(glm.fit)

lm.fit <- lm(mpg~horsepower, data=Auto)
coef(lm.fit)

# Library that contains function cv.glm() which performs cross validation
library(boot)
glm.fit <- glm(mpg~horsepower, data=Auto)
cv.err <- cv.glm(Auto, glm.fit)

# Contains cross validation results
# idx 0 - standard k fold cv estimate
# idx 1 bias corrected version
cv.err$delta

cv.error <- rep(0,5)
for(idx in 1:5) {
  glm.fit <- glm(mpg~poly(horsepower, idx), data=Auto)
  cv.error[idx] <- cv.glm(Auto, glm.fit)$delta[1]
}

cv.error

# 5.3.3 k-Fold Cross Validation
set.seed(17)
cv.error.10 <- rep(0,10)

for(idx in 1:10) {
  glm.fit <- glm(mpg~poly(horsepower, idx), data=Auto)
  cv.error.10[idx] <- cv.glm(Auto, glm.fit, K=10)$delta[1]
}

cv.error.10

# 5.3.4 The Bootstrap
alpha.fn <- function(data, idx) {
  X <- data$X[idx]
  Y <- data$Y[idx]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}

alpha.fn(Portfolio, 1:100)

set.seed(1)
alpha.fn(Portfolio, sample(100,100,replace=T))

# applies function alpha.fn and calculates standard deviation for the number R of bootstrap replicates
boot(Portfolio, alpha.fn, R=1000)

boot.fn <- function(data, index) {
  return(coef(lm(mpg~horsepower, data=data, subset=index)))
}
boot.fn(Auto, 1:392)

set.seed(1)
boot.fn(Auto, sample(392,392,replace=T))
boot.fn(Auto, sample(392,392,replace=T))

boot(Auto, boot.fn, 1000)
summary(lm(mpg~horsepower, data=Auto))$coef

boot.fn <- function(data, index) {
  return(coefficients(lm(mpg~horsepower+I(horsepower^2), data=data, subset=index)))
}
set.seed(1)

boot(Auto, boot.fn, 1000)
summary(lm(mpg~horsepower+I(horsepower^2), data=Auto))$coef
