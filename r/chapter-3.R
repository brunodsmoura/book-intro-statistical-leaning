# install.packages("ISLR")

library(MASS);
library(ISLR);

fix(Boston);
names(Boston)

lm.fit = lm(medv~lstat, data=Boston)
attach(Boston)
lm.fit = lm(medv~lstat)

summary(lm.fit)
names(lm.fit)

coef(lm.fit)
predict(lm.fit, data.frame(lstat=c(5,10,15)), interval = "confidence")
predict(lm.fit, data.frame(lstat=c(5,10,15)), interval = "prediction")

plot(lstat, medv)
abline(lm.fit, col="red")

par(mfrow=c(2,2))
plot(lm.fit)

plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))

plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

# --------

lm.fit <- lm(medv~lstat+age)
summary(lm.fit)

lm.fit <- lm(medv~., data=Boston)
summary(lm.fit)

summary(lm.fit)$r.sq
summary(lm.fit)$sigma

# install.packages("car")
library(car)
vif(lm.fit)

lm.fit_minusage <- lm(medv~.-age, data=Boston)
summary(lm.fit_minusage)

summary(lm(medv~lstat*age))

lm.fit=lm(medv~lstat)
lm.fit2=lm(medv~lstat+I(lstat^2))
summary(lm.fit2)

anova(lm.fit, lm.fit2)

par(mfrow=c(2,2))
plot(lm.fit2)

lm.fit5 <- lm(medv~poly(lstat, 5))
summary(lm.fit5)

summary(lm(medv~log(rm), data=Boston))

### Carseats
fix(Carseats)
names(Carseats)
