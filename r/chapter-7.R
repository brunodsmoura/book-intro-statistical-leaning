# 7.8 Non Linear Models
# 7.8.1 Polynomial Regression and Step Functions

library(ISLR)
attach(Wage)

# Different ways to create a polynomial function
fit <- lm(wage~poly(age, 4), data=Wage)
coef(summary(fit))

fit2 <- lm(wage~poly(age, 4, raw=T), data=Wage)
coef(summary(fit2))

fit2a <- lm(wage~age+I(age^2)+I(age^3)+I(age^4), data=Wage)
coef(fit2a)

fit2b <- lm(wage~cbind(age, age^2, age^3, age^4), data=Wage)
coef(fit2b)

agelims <- range(age)
age.grid <- seq(from=agelims[1], to=agelims[2])
preds <- predict(fit, newdata=list(age=age.grid), se=T)
se.bands <- cbind(preds$fit+2*preds$se.fit, preds$fit-2*preds$se.fit)

par(mfrow=c(1,2), mar=c(4.5, 4.5, 1, 1), oma=c(0,0,4,0))
plot(age, wage, xlim=agelims, cex=.5, col="darkgrey")
title("Degree-4 Polynomial", outer=T)
lines(age.grid, preds$fit, lwd=2, col="blue")
matlines(age.grid, se.bands, lwd=1, col="blue", lty=3)

preds2 <- predict(fit2, newdata=list(age=age.grid), se=T)
max(abs(preds$fit-preds2$fit))

fit.1 <- lm(wage~age, data=Wage)
fit.2 <- lm(wage~poly(age, 2), data=Wage)
fit.3 <- lm(wage~poly(age, 3), data=Wage)
fit.4 <- lm(wage~poly(age, 4), data=Wage)
fit.5 <- lm(wage~poly(age, 5), data=Wage)

# ANalysis Of VAriance
anova(fit.1, fit.2, fit.3, fit.4, fit.5)

coef(summary(fit.5))
(-11.893)^2

fit.1 <- lm(wage~education+age, data=Wage)
fit.2 <- lm(wage~education+poly(age,2), data=Wage)
fit.3 <- lm(wage~education+poly(age, 3), data=Wage)

# ANOVA method works whether or not we used orthogonal polynomials
anova(fit.1, fit.2, fit.3)

fit <- glm(I(wage>250)~poly(age, 4), data=Wage, family=binomial)
preds <- predict(fit, newdata=list(age=age.grid), se=T)

pfit <- exp(preds$fit)/(1+exp(preds$fit))
se.bands.logit <- cbind(preds$fit+2*preds$se.fit, preds$fit-2*preds$se.fit)
se.bands <- exp(se.bands.logit)/(1+exp(se.bands.logit))

preds <- predict(fit, newdata=list(age=age.grid), type="response", se=T)

# We have drawn the age values corresponding to the observations with wage
# values above 250 as gray marks on the top of the plot, and those with wage
# values below 250 are shown as gray marks on the bottom of the plot. We
# used the jtter() function to jutter the age values a bit so that observations
# with the same age value do not cover each other up. This is often called a rug plot
plot(age, I(wage>250), xlim=agelims, type="n", ylim=c(0,.2))
points(jitter(age), I((wage>250)/5), cex=.5, pch="|", col="darkgrey")
lines(age.grid, pfit, lwd=2, col="blue")
matlines(age.grid, se.bands, lwd=1, col="blue", lty=3)

# Create step functions with cut function
table(cut(age, 4))
fit <- lm(wage~cut(age,4), data=Wage)
coef(summary(fit))

# 7.8.2 Splines
library(splines)
fit <- lm(wage~bs(age, knots=c(25, 40, 60)), data=Wage)
pred <- predict(fit, newdata=list(age=age.grid), se=T)
plot(age, wage, col="gray")
lines(age.grid, pred$fit, lwd=2)
lines(age.grid, pred$fit+2*pred$se.fit, lty="dashed")
lines(age.grid, pred$fit-2*pred$se.fit, lty="dashed")

# produce a spline with knots at uniform quantiles of the data
dim(bs(age, knots=c(25, 40, 60)))
dim(bs(age, df=6))
attr(bs(age, df=6), "knots")

# producing a natural spline with 4 degrees of freedom
fit2 <- lm(wage~ns(age, df=4), data=Wage)
pred2 <- predict(fit2, newdata=list(age=age.grid), se=T)
lines(age.grid, pred2$fit, col="red", lwd=2)

# fit a smoothing spline
plot(age, wage, xlim=agelims, cex=.5, col="darkgrey")
title("Smoothing Spline")
fit <- smooth.spline(age, wage, df=16)
fit2 <- smooth.spline(age, wage, cv=T)
fit2$df

lines(fit, col="red", lwd=2)
lines(fit2, col="blue", lwd=2)
legend("topright", legend=c("16 DF", "6.8 DF"), col=c("red", "blue"), 
       lty=1, lwd=2, cex=.8)

# performing local regression
plot(age, wage, xlim=agelims, cex=.5, col="darkgrey")
title("Local Regression")
fit <- loess(wage~age, span=.2, data=Wage)
fit2 <- loess(wage~age, span=.5, data=Wage)
lines(age.grid, predict(fit, data.frame=(age=age.grid)), col="red", lwd=2)
lines(age.grid, predict(fit2, data.frame=(age=age.grid)), col="red", lwd=2)

legend("topright", legend=c("Span=0.2","Span=0.5"), col=c("red", "blue"), 
       lty=1, lwd=2, cex=.8)

# 7.8.3 GAMs
gam1 <- lm(wage~ns(year,4)+ns(age,5)+education, data=Wage)

install.packages("gam")
library(gam)

gam.m3 <- gam(wage~s(year,4)+s(age,5)+education, data=Wage)
par(mfrow=c(1,3))
plot(gam.m3, se=T, col="blue")
plot.Gam(gam1, se=T, col="red")

gam.m1 <- gam(wage~s(age, 5)+education, data=Wage)
gam.m2 <- gam(wage~year+s(age, 5)+education, data=Wage)

# We find that there is compelling evidence that a GAM with a linear function of
# year is better than a GAM that does not include year at all (p-value: 0.00014).
# However, there is no evidence that a non-linear function of year is needed
# (p-value=0.349). In other words, based on the results of this ANOVA, m2 is preferred.
anova(gam.m1, gam.m2, gam.m3, test="F")

# The p-values for year and age correspond to a null hypothesis of a linear relation-
# ship versus the alternative of a non-linear relationship. The large p-value for year 
# reinforces our conclusion from the ANOVA test that a linear function is adequate for
# this term. However, there is very clear evidence that a non-linear term is required
# for age.
summary(gam.m3)

preds <- predict(gam.m2, newdata=Wage)
gam.lo <- gam(wage~s(year, df=4)+lo(age, span=0.7)+education, data=Wage)
plot.Gam(gam.lo, se=T, col="green")

# Here we have used local regression for the age term, with a span of 0.7. We can
# also use the lo() function to create interactions before calling the gam() function
gam.lo.i <- gam(wage~lo(year, age, span=0.5)+education, data=Wage)

install.packages("akima")
library(akima)

plot(gam.lo.i)

# In order to fit a logistic regression GAM, we once again use the I() function in
# constructing the binary response variable and set family=binomial.
gam.lr <- gam(I(wage>250)~year+s(age, df=5)+education, family=binomial, data=Wage)
par(mfrow=c(1,3))
plot(gam.lr, se=T, col="green")

# It is easy to see that there are no high earners in the <HS category.
table(education, I(wage>250))

# Hence, we fit a logistic regression GAM using all but this category. This provides
# more sensible results.
gam.lr.s <- gam(I(wage>250)~year+s(age, df=5)+education, family=binomial, data=Wage, subset=(education!="1. < HS Grad"))
plot(gam.lr.s, se=T, col="green")
