library(leaps)
#Chapyter 6 Exercise 8
#(a) 
set.seed(1)
x=rnorm(100)
e=rnorm(100)
b0=1
b1=2
b2=-1
b3=0.3
y= b0 + b1*x + b2*(x^2)+b3*(x^3)+e

#Creating the dataset
data = data.frame(y,poly(x,10,raw=T))

#Using regsubsets
regfit.full=regsubsets(y~.,data = data,nvmax=10)
regfit.summary=summary(regfit.full)

par(mfrow=c(1,3))
plot(regfit.summary$adjr2, xlab="Number of Predictors", ylab = "Adjusted R-square", type="l")
plot(regfit.summary$cp, xlab="Number of Predictors", ylab="Cp", type="l")
plot(regfit.summary$bic, xlab="Number of Predictors", ylab="BIC", type="l")


which.min(regfit.summary$cp)

which.min(regfit.summary$bic)

which.max(regfit.summary$adjr2)

par(mfrow=c(1,3))
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")
plot(regfit.full,scale="adjr2")


#(d)
#Using regsubsets
regfit.full=regsubsets(y~.,data = data,nvmax=10, method = "backward")
regfit.summary=summary(regfit.full)

par(mfrow=c(1,3))
plot(regfit.summary$adjr2, xlab="Number of Predictors", ylab = "Adjusted R-square", type="l")
plot(regfit.summary$cp, xlab="Number of Predictors", ylab="Cp", type="l")
plot(regfit.summary$bic, xlab="Number of Predictors", ylab="BIC", type="l")


which.min(regfit.summary$cp)

which.min(regfit.summary$bic)

which.max(regfit.summary$adjr2)

coefficients(regfit.full,3)

#(e)
library(glmnet)
X_matrix = model.matrix(y~.,data)[,-1]
lasso.model = cv.glmnet(X_matrix,y,alpha=1)
best.lambda = lasso.model$lambda.min
plot(lasso.model)

best.model = glmnet(X_matrix, y, alpha=1)
preds = predict(best.model, s=best.lambda, type="coefficients")

#(f)
b7=2
Y = b0 + b7*(x^7) + e
fulldata = data.frame(Y,poly(x,10,raw=T))
fullmodel=regsubsets(Y~.,data=fulldata, nvmax=10)
model.summary=summary(fullmodel)
which.min(model.summary$cp)
which.min(model.summary$bic)
which.max(model.summary$adjr2)

coefficients(fullmodel,2)
coefficients(fullmodel,1)
coefficients(fullmodel,4)

#Applying Lasso
mat_X = model.matrix(Y~.,data=fulldata)[,-1]
lasso.mod = cv.glmnet(mat_X,Y, alpha=1)
best_lambda = lasso.mod$lambda.min
best_mod=glmnet(mat_X,Y, alpha=1)
preds = predict(best_mod, s=best_lambda, type="coefficients")
