# 8.3 Lab: Decision Trees
# 8.3.1 Fitting Classification Trees

install.packages("tree")
library(tree)

library(ISLR)
attach(Carseats)

High <- ifelse(Sales<=8, "No", "Yes")
Carseats <- data.frame(Carseats, High)

tree.carseats <- tree(High~.-Sales, Carseats)
# We see that the training error rate is 9%. For classification trees, the deviance
# reported in the output of summary() is given by -2 * sum of m sum of k Nmk log
# PHATmk
summary(tree.carseats)

plot(tree.carseats)
text(tree.carseats, pretty=0)

# Display the split criterion (e.g. Price<92.5), the number of observations in that
# branch, the deviance, the overall prediction for the branch (Yes or No), and the
# fraction of observations in that branch that take on values of Yes and No. Branches
# that leads to terminal nodes are indicated using asterisks.
tree.carseats

set.seed(2)
train <- sample(1:nrow(Carseats), 200)
Carseats.test <- Carseats[-train,]
High.test <- High[-train]

# Fitting the train set
tree.carseats <- tree(High ~. -Sales, Carseats, subset=train)
# Running predictions on the tree set
tree.pred <- predict(tree.carseats, Carseats.test, type="class")

table(tree.pred, High.test)

set.seed(3)

# Next, we consider whether pruning the tree might lead to improved results. The
# function cv.tree performs cross-validation in order to determine the optimal
# level of tree complexity; cost complexity pruning is used in order to select a
# sequence of trees for consideration. We use the arg FUN=prune.misclass in order to
# indicate that we want the classification error rate to guide the cross-validation
# and pruning process, rather than the default for the cv.tree function, which is 
# deviance.
cv.carseats <- cv.tree(tree.carseats, FUN=prune.misclass)
names(cv.carseats)

cv.carseats

# Note that, despite the name, dev corresponds to the cross-validation error rate in
# this instance. The tree with 9 terminal nodes results in the lowest rate as a 
# function of both size and k.
par(mfrow=c(1,2))
plot(cv.carseats$size, cv.carseats$dev, type="b")
plot(cv.carseats$k, cv.carseats$dev, type="b")

# We now apply the prune.misclass function in order to prune the tree to obtain the
# nine-node tree
prune.carseats <- prune.misclass(tree.carseats, best=9)
plot(prune.carseats)
text(prune.carseats, pretty=0)

tree.pred <- predict(prune.carseats, Carseats.test, type="class")
table(tree.pred, High.test)

# Now 77% of the test observations are correctly classified, so not only has the pru-
# ning process produced a more interpretable tree, but it has also improved the classi-
# fication accuracy.
(94+60)/200

prune.carseats <- prune.misclass(tree.carseats, best=15)
plot(prune.carseats)
text(prune.carseats, pretty=0)

tree.pred <- predict(prune.carseats, Carseats.test, type="class")
table(tree.pred, High.test)

# 8.3.2 Fitting Regression Trees
library(MASS)
set.seed(1)

train <- sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston <- tree(medv ~., Boston, subset=train)

# In the context of a regression tree, the deviance is simply the sum of squared errors
# for the tree.
summary(tree.boston)

plot(tree.boston)
text(tree.boston, pretty=0)

cv.boston <- cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type="b")

prune.boston <- prune.tree(tree.boston, best=5)
plot(prune.boston)
text(prune.boston, pretty=0)

yhat <- predict(tree.boston, newdata=Boston[-train,])
boston.test <- Boston[-train, "medv"]
plot(yhat, boston.test)
abline(0,1)

# The test set MSE associated with the regression tree is 25.05. The squared root of
# the MSE is therefore around 5.005, indicating that this model leads to the
# predictions that are within around $5.005 of the true median home value for the
# suburb.
mean((yhat-boston.test)^2)

# 8.3.3 Bagging and Random Forests
install.packages("randomForest")
library(randomForest)

set.seed(1)

# Recall that bagging is simply a special case of a random forest with m = p
# The argument mtry indicates that all 13 predictors should be considered for each 
# split of the tree - in other words, that bagging should be done.
bag.boston <- randomForest(medv~., data=Boston, subset=train, mtry=13, importance=T)
bag.boston

yhat.bag <- predict(bag.boston, newdata=Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1)

# The test MSE associated with the bagged regression is 13.50, almost half that
# obtained using an optimally-pruned single tree.
mean((yhat.bag-boston.test)^2)

# We can change the number of trees grown by randomForest using the ntree argument
bag.boston <- randomForest(medv~., data=Boston, subset=train, mtry=13, ntree=25)
yhat.bag <- predict(bag.boston, newdata=Boston[-train,])
mean((yhat.bag-boston.test)^2)

# By default, randomForest uses p.3 variables when building a random forest of
# regression trees, and sqrt(p) variables when building a random forest of 
# classification trees.
set.seed(1)
rf.boston <- randomForest(medv~., data=Boston, subset=train, mtry=6, importance=T)
yhat.rf <- predict(rf.boston, newdata=Boston[-train,])

# The test MSE is 11.66; this indicates that random forests yielded an improvement
# over bagging in this case
mean((yhat.rf-boston.test)^2)

# Two measures of variable importance are reported. The former is based upon the mean
# decrease of accuracy in predictions on the out of bag samples when a given variable
# is excluded from the model. The latter is a measure of the total decrease in node
# impurity that results from splits over that variable, averaged over all trees.
# In the case of regression trees, the node impurity is measured by the training RSS,
# and for classification trees by the deviance.
importance(rf.boston)

# The results indicate that across all of the trees considered in the random forest,
# the wealth level of the comunity (lstat) and the house size (rm) are by far the two
# most important variables
varImpPlot(rf.boston)

# 8.3.4 Boosting
install.packages("gbm")
library(gbm)

set.seed(1)
# We run gbm with the option distribution = gaussian since this is a regression
# problem; if it were a binary classification problem, we would use distribution =
# bernoulli.
boost.boston <- gbm(medv~., data=Boston[train,], distribution = "gaussian", 
                    n.trees=5000, interaction.depth=4)
# The summary function produces a relative influence plot and also outputs the
# relative influence statistics.
summary(boost.boston)

par(mfrow=c(1,2))
# These plots ilustrate the marginal effect of the selected variables on the response
# after integrating out the other variables. In this case, as we might expect, median
# house prices are increasing with rm and decreasing with lstat.
plot(boost.boston, i="rm")
plot(boost.boston, i="lstat")

yhat.boost <- predict(boost.boston, newdata=Boston[-train,], n.trees=5000)
mean((yhat.boost-boston.test)^2)

# If we want to, we can perform boosting with a different value of the shrinkage 
# parameter lambda. The default value is 0.001, but this is easily modified.
boost.boston <- gbm(medv~., data=Boston[train,], distribution = "gaussian", 
                    n.trees=5000, interaction.depth=4, shrinkage=0.2, verbose=F)
yhat.boost <- predict(boost.boston, newdata=Boston[-train,], n.trees=5000)
mean((yhat.boost-boston.test)^2)
