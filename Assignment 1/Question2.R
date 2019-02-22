#Question 2
library(ISLR)
set.seed(3684)
data = Auto
attach(data)

# Part (a)
mpg01 = rep(0, 392)

mpg01 [mpg > median(mpg)] = 1

data = data.frame(data, mpg01)

#Part (b)

#Creating scatter plots using different predictors while omitting name
pairs(data[,-9])

#Co-relation matrix of the predictors
cor(data[,-9])

#Creating box plots 
par(mfrow=c(2,3))
boxplot(cylinders ~ mpg01, data = Auto, main = "Cylinders vs mpg01")
boxplot(displacement ~ mpg01, data = Auto, main = "Displacement vs mpg01")
boxplot(horsepower ~ mpg01, data = Auto, main = "Horsepower vs mpg01")
boxplot(weight ~ mpg01, data = Auto, main = "Weight vs mpg01")
boxplot(acceleration ~ mpg01, data = Auto, main = "Acceleration vs mpg01")
boxplot(year ~ mpg01, data = Auto, main = "Year vs mpg01")

#From the graphical and data analysis of the predictors in the dataset,
#We can see that there is significant co-relation of mpg01 with "cylinders", "weight", "displacement" and "horsepower".

#Part (c)
set.seed(3684)
#Splitting data into test and train
train = sample(dim(data)[1], dim(data)[1]*0.8)
test = -train
train_data = data[train,]
test_data = data[test,]
mpg01.test = mpg01[test]


#Part (d)
glm.fit = glm (mpg01 ~ cylinders + weight + displacement + horsepower, data=train_data, family = binomial )
summary(glm.fit)

#Finding test error
#Finding predicted probabilities on test data
probabilities = predict(glm.fit, test_data, type = "response")
#Dummy variable to store probabilities
predictions.glm = rep(0, length(probabilities))
predictions.glm[probabilities > 0.5] = 1
#Comparing test results
table(predictions.glm, mpg01.test)
test_error = mean(predictions.glm != mpg01.test)
test_error
#The test error of the model obtained is 0.08860759.
