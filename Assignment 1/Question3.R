library(ISLR)
library(boot)

data = Default

set.seed(3684)

#Part(a)
lr.model =glm(default~income+balance,family = binomial,data=Default)

summary(lr.model)

# From the summary of the model we can see that:
# the standard error in the computation of the coeffecient of income is 4.985e-06.
# the standard error in the computation of the coeffecient of balance is 2.274e-04.

#Part (b)
boot.fn = function(data, indices) {
  lr.model =glm(default~income+balance,family = binomial,data=Default, subset=indices)
  coefs = coef(lr.model)
  return (coefs)
}

# Part (c)
boot(Default,boot.fn,200)

# From the Bootstrap Statistics we can see that:
# the standard error in the computation of the coeffecient of income is 4.953730e-06.
# the standard error in the computation of the coeffecient of balance is 2.362944e-04.

#Part (d)
#We can see that the standard errors in the predicted coeffecients from the two methods are very close to each other.

