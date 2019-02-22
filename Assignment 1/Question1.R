#Question1
set.seed(3684)

# Part(a)
x = rnorm(100)

# Part (b)
eps = rnorm(100, 0, 0.5)

#Part (c)
Y = -1 + 0.5*x + eps

#The length of Vector Y is 100.
#The values of ??0 and ??1 are -1 and 0.5 respectively. 

#Part(d)
plot(x,Y)

# Observation: The scatterplot shows that there is siginificant co-relation between x and y. We can see that the corelation is positive as when x increase, Y also increases. 
# The relationship between x and Y seems to be linear. 

#Part (e)
#Fitting simple linear regression model
lm.fit = lm (Y ~ x)
summary(lm.fit)


#The predicted values of ??0 and ??1 are -1.15902 and 0.57612 respectively.
#We can say that there is an increase of 0.57612 units in Y when x is increased by 1 unit.
#As the the p-value of x is < 0.01, we can reject the null hypothesis and conclude that there is some co-relation between x and Y.
#Furthermore, as the values of ??^0 and ??^1 are very close to ??0 and ??1, we can say that the model fits the data well.

#Part(f)
#Plotting the regression line
abline(lm.fit, col='green')
legend(-3,1, c("Least Square Line", "Population Regression Line"), fill=c("green", "blue"), title='Legend')
#Plotting the population line
abline(-1,0.5, col='blue')

#Part (g)
#Fitting polynomial regression model
lm2.fit = lm(Y ~ x + I(x^2))
summary(lm2.fit)
#The RSE for the linear regression model is 0.493, where as the RSE in polynomial regression model is 0.4925. Since there is not a significant difference in the RSE, the quadratic term imporves the model only minutely.
#Furthermore, we can see that in the polynomial regression model, the coeffecient of the x^2 term is very small compared to the coeffecient of x, as the relationship between x and Y is mainly linear. 
#In addition to this, using a polynomial model might increase the variance, and hence we might get a higher test error.
#Hence, there is no notable improvement by using polynomial regression model.

#Part(h)
set.seed(3684)
X = rnorm(100)
eps = rnorm(100, 0, 0.15)
Y = -1 + 0.5*X + eps
plot(X,Y)
lm3.fit = lm (Y ~ X)
summary(lm3.fit)
abline(lm.fit, col='green')
legend(-3,1, c("Least Square Line", "Population Regression Line"), fill=c("green", "blue"), title='Legend')
#Plotting
abline(-1,0.5, col='blue')

#In this case, we observe that RSE is 0.1479, which is very low compared to the previous models. 
#Since the variance of the data is low, we are able to achieve a very low RSE and We can see that the regression line is very close to the population line and they are almost overlapping. 
#This is also evident from the high vaue of R-squared which is 0.9307.
#Hence the regression line fits the data very accurately. 


#Part (i)
set.seed(3684)
X = rnorm(100)
eps = rnorm(100, 0, 0.75)
Y = -1 + 0.5*X + eps
plot(X,Y)
lm4.fit = lm (Y ~ X)
summary(lm4.fit)
abline(lm.fit, col='green')
legend(-3,1, c("Least Square Line", "Population Regression Line"), fill=c("green", "blue"), title='Legend')
#Plotting
abline(-1,0.5, col='blue')

#In this case, we observe that the RSE is 0.7395 which is very high as compared to the previous models.
#As the variance of the data is increased, the RSE of the regression line increases.
#Due to the increase in the variance, the regression line is not able to fit the data accurately. This is also evident from the low R-squared value which is 0.4257.
#Hence, increasing the variance of the data reduces the prediciton accuracy and results in a worse fit.


# Part (j)
#Confidence interval for the orignal dataset
confint(lm.fit)
#Confidence interval for the less noisy dataset
confint(lm3.fit)
#Confidence interval for the more noisy dataset
confint(lm4.fit)

#We observe that in case of the less noisy dataset, the width of the confidence interval decreases as compared to the orignal dataset, hence we can be more certain about the predict the values of ??0 and ??1.
#We observe that in case of the more noisy dataset, the width of the confidence interval increases as compared to the orignal dataset, hence we are less certain about the predict the values of ??0 and ??1.

