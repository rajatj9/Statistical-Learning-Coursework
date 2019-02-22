#Chapter 8
#Exercise 8

library(ISLR)
library(tree)
set.seed(3684)
nrow(Carseats)

#(a) Splitting into training and testing data
train = sample(1:nrow(Carseats), 200)
carseats.train = Carseats[train,]
carseats.test = Carseats[-train,-1]
test.y = Carseats[-train, "Sales"]

#(b) Fitting the regression tree to the training set
tree.carseats=tree(Sales~.,data = Carseats,subset=train)

#Plotting the tree
plot(tree.carseats)
text(tree.carseats, pretty=0)
summary(tree.carseats)

#predictions
preds=predict(tree.carseats,carseats.test)
#TEST MSE
test.mse = mean((preds-test.y)^2)

#(c) using cross-validation to obtain optimal level of tree complexity
cv.carseats = cv.tree(tree.carseats, FUN = prune.tree)
par(mfrow=c(1,2))
plot(cv.carseats$size, cv.carseats$dev, type="b")
plot(cv.carseats$k, cv.carseats$dev, type="b")
best.tree = which.min(cv.carseats$dev)
best.tree.size = cv.carseats$size[best.tree]
#Pruning the tree
carseats.pruned = prune.tree(tree.carseats, best = 3)
plot(carseats.pruned)
text(carseats.pruned,pretty=0)


pruned.preds = predict(carseats.pruned, carseats.test)
pruned.mse = mean((pruned.preds - test.y)^2)

#Applying bagging 
library(randomForest)
set.seed(3684)
bagged.carseats = randomForest(Sales~., data = Carseats, subset = train, mtry = 10, importance=TRUE)

#Calculating the test error from bagging
bagged.preds = predict(bagged.carseats, carseats.test)
bagged.mse = mean((bagged.preds-test.y)^2)

#Applying random forest
rf.carseats = randomForest(Sales~., data = Carseats, mtry = 3,subset = train, importance = TRUE)
rf.preds = predict (rf.carseats, carseats.test)
rf.mse = mean((rf.preds-test.y)^2)


#Calculating RF Error for different values of m
m=c()
m.error=c()
for (i in 1:5)
{
  print(c("Iteration: ", i))
  rf.carseats3 = randomForest(Sales~., data = Carseats, mtry = 3,subset = train, importance = TRUE)
  rf.preds3 = predict (rf.carseats3, carseats.test)
  rf.mse3 = mean((rf.preds3-test.y)^2)
  print(c("3: ",rf.mse3))
  rf.carseats10 = randomForest(Sales~., data = Carseats, mtry = 10,subset = train, importance = TRUE)
  rf.preds10 = predict (rf.carseats10, carseats.test)
  rf.mse10 = mean((rf.preds10-test.y)^2)
  print(c("10: ",rf.mse10))
  print("#########")
  

}
par(mfrow=c(1,1))
plot(m,m.error, type="l")
