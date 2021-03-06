library(ISLR)
library(MASS)
library(boot)
set.seed(3684)
data = Boston
attach(data)

#Part (a)
medv_mean = mean(medv)
#The estimate mean of medv is 22.53281.

# Part (b)
standard_error = sd(medv)/sqrt(length(medv))


#The estimated standard error is 0.4088611.

# Part (c)
boot.mean = function(data, indices) {
  medv_mean = mean(medv[indices])
  return (medv_mean)
}

boot(medv,boot.mean,50)

#The estimated standard error from bootstrap technique is 0.3871729.
#The value estimated from bootstrap is fairly close to the value calculated from the sample.


#Part (d)
library(stringr)
medv.bootstrap = boot(medv,boot.mean,50)

text = capture.output(medv.bootstrap) # Output will be stored as text
text = str_extract(text ,"^t1.*$") # Extracting the line which contains the statistics
text = text[!is.na(text)] # Removing the un-necessary lines
medv.boot.standard_error = as.numeric(unlist(str_extract_all(text, '[0-9.]+$')))
medv.boot.standard_error

medv.standard_error.ci = c(medv_mean - 2*medv.boot.standard_error, medv_mean + 2*medv.boot.standard_error )
medv.standard_error.ci

#Performing the t-test.
t.test(medv)



#Part (e)

medv_median = median(medv)

#The median of medv for the dataset is 21.2

#Part (f)
boot.median = function(data, indices) {
  return (median(data[indices]))
}
boot(medv, boot.median, 50)


#The value of the median obtained using the bootstrap method is the same as the value obtained in (e).
# The standard error obtained using the bootstrap method for the median medv value is 0.3509986.

#Part (g)
predicted_percent = quantile(medv, c(0.1))


#The estimate for the 10th percentile medv in Boston suburbs is 12.75.

#Part (h)
boot.percent =  function(data, indices)
{
  return (quantile(data[indices],c(0.1)))
}
boot(medv, boot.percent, 50)

#The value of the 10th percentile estimate is the same as the value obtained in part(g).
#The standard error in the 10th percentile value using the bootstrap method is 0.4251158.
