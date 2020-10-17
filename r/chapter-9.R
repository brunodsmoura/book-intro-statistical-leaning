# 9.6 Lab: Support Vector Machines
# 9.6.1 Support Vector Classifier
install.packages("e1071")
library(e1071)

set.seed(1)
x <- matrix(rnorm(20*2), ncol=2)
y <- c(rep(-1,10), rep(1,10))

x[y==1, ] <- x[y==1, ] + 1
plot(x, col=(3-y))

# Note that in order for the svm() function to perform classification (as opposed to
# SVM-based regression), we must encode the response as a factor variable.
dat <- data.frame(x=x, y=as.factor(y))
# A cost argument allows us to specify the cost of a violation to the margin. When
# the cost argument is small, then the margins will be wide and many support vectors
# will be on the margin or will violate the margin. When the cost argument is large,
# then the margins will be narrow and there will be few support vectors on the margin
# or violating the margin.
# The argument scale=F tells the svm() function not to scale each feature to have mean
# zero or standard deviation one.
svm.fit <- svm(y~., data=dat, kernel="linear", cost=10, scale=F)

plot(svm.fit, dat)

# support vectors
svm.fit$index

# The summary tells us, for instance, that a linear kernel was used with cost=10, and
# that there were seven support vectors, four in one class and three in the other.
summary(svm.fit)

svm.fit <- svm(y~., data=dat, kernel="linear", cost=0.1, scale=F)

# Now that a smaller value of the cost parameter is being used, we obtain a larger 
# number of support vectors, because the margin is now wider. Unfortunately, the svm()
# function does not explicitly output the coefficients of the linear decision 
# boundary obtained when the support vector classifier is fit, nor does it output the 
# width of the margin.
plot(svm.fit, dat)
svm.fit$index

set.seed(1)
# The e1071 library includes a built-in function, tune(), to perform cross-validation.
# By default, tune() performs ten-fold cross-validation on a set of models of interest.
tune.out <- tune(svm, y~., data=dat, kernel="linear", 
                 ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)

# We see that cost=0.1 results in the lowest cross-validation error rate. The tune()
# function stores the best model obtained, which can be accessed as follows.
bestmod <- tune.out$best.model
summary(bestmod)

xtest <- matrix(rnorm(20*2), ncol=2)
ytest <- sample(c(-1,1), 20, rep=T)
xtest[ytest==1, ] <- xtest[ytest==1, ] + 1
testdat <- data.frame(x=xtest, y=as.factor(ytest))

# Predict test set based on the best model found through cross-validation.
ypred <- predict(bestmod, testdat)

# With this value of cost, 19 of the test observations are correctly classified.
table(predict=ypred, truth=testdat$y)

svm.fit <- svm(y~., data=dat, kernel="linear", cost=.01, scale=F)
ypred <- predict(svm.fit, testdat)

# Predict using a model with cost .01 has a result of one additional observation being
# misclassified.
table(predict=ypred, truth=testdat$y)

x[y==1, ] <- x[y==1, ] + 0.5
plot(x, col=(y+5)/2, pch=19)

# Now the observations are just barely linearly separable. We fit the support vector
# classifier and plot the resulting hyperplane, using a very large value of cost so
# that no observations are misclassified.
dat <- data.frame(x=x, y=as.factor(y))
svm.fit <- svm(y~., data=dat, kernel="linear", cost=1e5)
summary(svm.fit)

# No training errors were made and only three support vectors were used. However, we
# can see from the figure that the margin is very narrow (because the observations that
# are not support vectors, indicated as circles, are very close to the decision
# boundary). It seems likely that this model will perform poorly on test data. 
plot(svm.fit, dat)

svm.fit <- svm(y~., data=dat, kernel="linear", cost=1)
summary(svm.fit)

# Using cost=1, we misclassify a training observation, but we also obtain a much wider
# margin and make use of seven support vectors. It seems likely that this model will perform 
# better on test data than th model with cost 1e5.
plot(svm.fit, dat)

# 9.6.2 Support Vector Machine
# To fit an SVM with a polynomial kernel we use kernel="polynomial", and to fit an
# SVM with a radial kernel we use kernel="radial". In the former case we also use the
# degree argument to specify a degree for the polynomial kernel, and in the latter 
# case we use gamma to specify a value of gamma for the radial basis kernel.

set.seed(1)
x <- matrix(rnorm(200*2), ncol=2)
x[1:100, ] <- x[1:100, ]+2
x[101:150, ] <- x[101:150]-2
y <- c(rep(1,150), rep(2,50))
dat <- data.frame(x=x, y=as.factor(y))

plot(x, col=y)

train <- sample(200, 100)
svm.fit <- svm(y~., data=dat[train,], kernel="radial", gamma=1, cost=1)
# The plot shows that the resulting SVM has a decidedly non-linear boundary.
plot(svm.fit, dat[train,])
summary(svm.fit)

# We can see from the figure that there are a fair number of training errors in this
# SVM fit. If we increase the value of cost, we can reduce the number of training 
# errors. However, this comes at the price of a more irregular decision boundary that
# seems to be at risk of overfitting the data.
svm.fit <- svm(y~., data=dat[train,], kernel="radial", gamma=1, cost=1e5)
plot(svm.fit, dat[train,])

# We can perform cross-validation using tune() to select the best choice of gamma
# and cost for an SVM with a radial kernel.
set.seed(1)
tune.out <- tune(svm, y~., data=dat[train,], kernel="radial", 
                 ranges=list(cost=c(0.1, 1, 10, 100, 1000)),
                 gamma=c(0.5,1,2,3,4))
summary(tune.out)

# ~10% of test observations are misclassified by this SVM.
table(true=dat[-train, "y"], pred=predict(tune.out$best.model, newdata=dat[-train,]))

# 9.6.3 ROC Curves
install.packages("ROCR")
library(ROCR)

rocplot <- function(pred, truth, ...) {
  predob <- prediction(pred, truth)
  perf <- performance(predob, "tpr", "fpr")
  plot(perf, ...)
}

svm.fit <- svm(y~., data=dat[train,], kernel="radial", gamma=2, cost=1, 
               decision.values=T)
fitted <- attributes(predict(svm.fit, dat[train,], decision.values=T))$decision.values
par(mfrow=c(1,2))
rocplot(fitted, dat[train, "y"], main="Training Data")

svm.fit.flex <- svm(y~., data=dat[train,], kernel="radial", gamma=50, cost=1, 
                    decision.values=T)
fitted <- attributes(predict(svm.fit.flex, dat[train,], decision.values=T))$decision.values
rocplot(fitted, dat[train, "y"], add=T, col="red")

fitted <- attributes(predict(svm.fit, dat[-train,], decision.values=T, values=T))$decision.values
rocplot(fitted, dat[-train,"y"], main="Test Data")
fitted <- attributes(predict(svm.fit.flex, dat[-train,], decision.values=T))$decision.values
rocplot(fitted, dat[-train,"y"], add=T, col="red")

# 9.6.4 SVM with multiple classes
# If the response is a factor containing more than two levels, then the svm() function
# will perform multi-class classification using the one-versus-one approach. We explore
# that setting here by generating a third class of observations.
set.seed(1)
x <- rbind(x, matrix(rnorm(50*2), ncol=2))
y <- c(y, rep(0,50))
x[y==0, 2] <- x[y==0, 2]+2
dat <- data.frame(x=x, y=as.factor(y))

par(mfrow=c(1,1))
plot(x, col=(y+1))

# We now fit an SVM to the data
svm.fit <- svm(y~., data=dat, kernel="radial", cost=10, gamma=1)
plot(svm.fit, dat)

# The e1071 library can also be used to perform support vector regression, if the
# response vector that is passed in to svm() is numerical rather than a factor.

# 9.6.5 Application to Gene Expression Data
# We now examine the Khan data set, which consists of a number of tissue samples
# corresponding to four distinct types of small round blue cell tumors. For each
# tissue sample, gene expression measurements are available.
library(ISLR)
names(Khan)

dim(Khan$xtrain)
dim(Khan$xtest)

length(Khan$ytrain)
length(Khan$ytest)

table(Khan$ytrain)
table(Khan$ytest)

# We will use a support vector approach to predict cancer subtype using gene 
# expression measurements. In this data set, there are a very large number of
# features relative to the number of observations. This suggests that we should use
# a linear kernel, because the additional flexibility that will result from using a 
# polynomial or radial kernel is unnecessary.
dat <- data.frame(x=Khan$xtrain, y=as.factor(Khan$ytrain))
out <- svm(y~., data=dat, kernel="linear", cost=10)
summary(out)

# We see that there are no training errors. In fact, this is not surprising, because
# the large number of variables relative to the number of observations implies that
# it is easy to find hyperplanes that fully separate the classes. We are most 
# interested not in the support vector classifier's performance on the training
# observations, but rather its performance on the test observations.
table(out$fitted, dat$y)

dat.te <- data.frame(x=Khan$xtest, y=as.factor(Khan$ytest))
pred.te <- predict(out, newdata=dat.te)

table(pred.te, dat.te$y)
