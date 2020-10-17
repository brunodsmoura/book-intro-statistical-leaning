# 6.5.1 Best Subset Selection
library(ISLR)
fix(Hitters)
names(Hitters)

dim(Hitters)
# Check missing values for columns
sum(is.na(Hitters))

# Remove rows that contains columns with missing values
Hitters <- na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))

library(leaps)
# Performs best subset selection (quantified using RSS)
regfit.full <- regsubsets(Salary~.,Hitters)
summary(regfit.full)

regfit.full <- regsubsets(Salary~., data = Hitters, nvmax=19)
reg.summary <- summary(regfit.full)

names(reg.summary)

# R^2 statistic increases when more variables are included in the model.
reg.summary$rsq

par(mfrow=c(2,2))
plot(reg.summary$rss, xlab="Number of Variables", ylab="RSS", type="l")
plot(reg.summary$adjr2, xlab="Number of Variables", ylab="RSS", type="l")

max_adjr2 <- which.max(reg.summary$adjr2)
points(max_adjr2, reg.summary$adjr2[max_adjr2], col="red", cex=2, pch=20)

par(mfrow=c(2,2))
plot(reg.summary$cp, xlab="Number of Variables", ylab="Cp", type="l")

min_cp <- which.min(reg.summary$cp)
points(min_cp, reg.summary$cp[min_cp], col="red", cex=2, pch=20)

plot(reg.summary$bic, xlab="Number of Variables", ylab="BIC", type="l")

min_bic <- which.min(reg.summary$bic)
points(min_bic, reg.summary$bic[min_bic], col="red", cex=2, pch=20)

?plot.regsubsets

plot(regfit.full, scale="r2")
plot(regfit.full, scale="adjr2")
plot(regfit.full, scale="Cp")
plot(regfit.full, scale="bic")

# Coefficients estimates associated with the model with lowest BIC
coef(regfit.full, min_bic)

# 6.5.2 Forward and Backward Stepwise Selection
regfit.fwd <- regsubsets(Salary~., data=Hitters, nvmax=10, method="forward")
summary(regfit.fwd)

regfit.bwd <- regsubsets(Salary~., data=Hitters, nvmax=10, method="backward")
summary(regfit.bwd)

# Best seven-variable model for each fit applying subset selection (full, forward and backward)
coef(regfit.full, 7)
coef(regfit.fwd, 7)
coef(regfit.bwd, 7)

# Choosing among models using the Validation Set apporach and Cross-Validation
set.seed(1)
train <- sample(c(TRUE,FALSE), nrow(Hitters), rep=TRUE)
test <- (!train)

# Fit model based only on training samples
regfit.best <- regsubsets(Salary~., data=Hitters[train,], nvmax=19)

test.mat <- model.matrix(Salary~., data=Hitters[test,])

val.errors <- rep(NA, 19)
for(idx in 1:19) {
  coefi <- coef(regfit.best, id=idx)
  pred <- test.mat[, names(coefi)]%*%coefi
  # Calculate MSE
  val.errors[idx] <- mean((Hitters$Salary[test]-pred)^2)
}

# We can conclude that the best model is the one that contains 10 variables
val.errors
which.min(val.errors)

coef(regfit.best, 10)

predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id=id)
  xvars=names(coefi)
  return(mat[,xvars]%*%coefi)
}

regfit.best <- regsubsets(Salary~., data=Hitters, nvmax=19)
# Select the best 10-variable model for this set
coef(regfit.best,10)

k <- 10
set.seed(1)
folds <- sample(1:k, nrow(Hitters), replace=TRUE)
cv.errors <- matrix(NA, k, 19, dimnames=list(NULL, paste(1:19)))

for(j in 1:k) {
  best.fit <- regsubsets(Salary~., data=Hitters[folds!=j,], nvmax=19)
  for(i in 1:19) {
    pred <- predict(best.fit, Hitters[folds==j, ], id=i)
    cv.errors[j,i] <- mean( (Hitters$Salary[folds==j]-pred)^2 )
  }
}

mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors

par(mfrow=c(1,1))
plot(mean.cv.errors, type='b')

# Plot model with the minimum MSE and check how many variables it has
min_variables <- which.min(mean.cv.errors)
points(min_variables, mean.cv.errors[min_variables], col="red", cex=2, pch=20)

reg.best <- regsubsets(Salary~., data=Hitters, nvmax=19)
coef(reg.best, min_variables)

# 6.6 Lab 2: Ridge Regression and the Lasso
x <- model.matrix(Salary~., Hitters)[,-1]
y <- Hitters$Salary

# 6.6.1 Ridge Regression
library(glmnet)
grid <- 10^seq(10, -2, length=100)
# alpha = 0 (trains a ridge regression model)
ridge.mod <- glmnet(x, y, alpha=0, lambda=grid)

dim(coef(ridge.mod))
ridge.mod$lambda[50]

coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))

ridge.mod$lambda[60]
coef(ridge.mod)[,60]

# obtain ridge regression coefficient for a new value of lambda
predict(ridge.mod, s=50, type="coefficients")[1:20,]

# divide dataset into train and test
set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
y.test <- y[test]

# fit ridge regression model and evaluate MSE
ridge.mod <- glmnet(x[train,], y[train], alpha = 0, lambda=grid, thresh=1e-12)
ridge.pred <- predict(ridge.mod, s=4, newx=x[test,])
# Test MSE
mean((ridge.pred-y.test)^2)

mean(((mean(y[train]))-y.test)^2)

# fit ridge regression with a very large lambda
ridge.pred <- predict(ridge.mod, s=1e10, newx=x[test,])
mean((ridge.pred-y.test)^2)

# fit ridge regression with lambda = 0 is equal to least squares
ridge.pred <- predict(ridge.mod, s=0, newx=x[test,])
mean((ridge.pred-y.test)^2)

lm(y~x, subset=train)
predict(ridge.mod, s=0, exact=T, type="coefficients")[1:20,]

set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha=0)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam

ridge.pred <- predict(ridge.mod, s=bestlam, newx=x[test,])
mean((ridge.pred-y.test)^2)

out <- glmnet(x, y, alpha=0)
# None of the coefficients are zero - ridge regression does not perform variable selection!
predict(out, type="coefficients", s=bestlam)[1:20,]

# 6.6.2 The Lasso
lasso.mod <- glmnet(x[train,], y[train], alpha=1, lambda=grid)
# Depending on the choice of the tuning parameter, some of the coefficients will be exactly equal to zero.
plot(lasso.mod)

set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha=1)
plot(cv.out)

bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod, s=bestlam, newx=x[test,])
mean((lasso.pred-y.test)^2)

out <- glmnet(x, y, alpha=1, lambda=grid)
lasso.coef <- predict(out, type="coefficients", s=bestlam)[1:20,]
# Lasso has a substantial advantage over ridge regression in that the resulting coefficient estimates are sparse.
# We see that 12 of 19 coefficient estimates are exactly zero. That means, the lasso model only counts with 7 variables.
lasso.coef

# 6.7 Lab 3: PCR and PLS Regression
# 6.7.1 Principal Components Regression
install.packages("pls")
library(pls)
set.seed(2)

# scale=true standardizes each predictor
# validation=CV applies a ten-fold cross-validation error
pcr.fit <- pcr(Salary~., data=Hitters, scale=T, validation="CV")
# PCR reports the root mean squared error. 
# In order to obtain the usual MSE, we must square this quantity.
summary(pcr.fit)

validationplot(pcr.fit, val.type="MSEP")

set.seed(1)
pcr.fit <- pcr(Salary~., data=Hitters, subset=train, scale=T, validation="CV")
validationplot(pcr.fit, val.type="MSEP")

pcr.pred <- predict(pcr.fit, x[test,], ncomp=7)
# This test set MSE is competitive with the results obtained using ridge regression
# and the lasso. However, as a result of the way PCR is implemented, the final model
# is more difficult to interpret because it does not perform any kind of variable 
# selection.
mean((pcr.pred-y.test)^2)

pcr.fit <- pcr(y~x, scale=T, ncomp=7)
summary(pcr.fit)

# 6.7.2 Partial Least Squares
set.seed(1)
pls.fit <- plsr(Salary~., data=Hitters, subset=train, scale=T, validation="CV")
summary(pls.fit)
validationplot(pls.fit, val.type="MSEP")

# number of components pick is equals to the one with the lowest cross-validation error
pls.pred <- predict(pls.fit, x[test,], ncomp=2)
mean((pls.pred-y.test)^2)

# Perform it on the full dataset
pls.fit <- plsr(Salary~., data=Hitters, scale=T, ncomp=2)
# Notice that the percentage of variance in Salary that the two-component
# PLS fit explains, 46.4%, is almost as much as that explained using the
# final seven-component model PCR fit, 46.69%. This is because PCR only
# attempts to maximize the amount of variance explained in the predictors,
# while PLS searches for directions that explain variance in both the predictors
# and the response.
summary(pls.fit)
