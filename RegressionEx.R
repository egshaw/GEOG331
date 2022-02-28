#Regression scratch work 2/28/2022

#subset for Iris virginica
v.iris <- iris[iris$Species == "virginica",]


#linear model relating petal to sepal length
fit <- lm(v.iris$Petal.Length~v.iris$Sepal.Length)

#view results
summary(fit)

#create a scatter plot (usually do this first)
plot(v.iris$Sepal.Length,v.iris$Petal.Length,
     main = "Iris Virginica",
     xlab = "Sepal Length",
     ylab = "Petal Length",
     col = "purple", pch = 16)

#plot the residuals, stored in regression summary
plot(v.iris$Sepal.Length, summary(fit)$residuals,
     xlab = "Sepal Length",
     ylab = "Residuals",
     col = "purple",
     pch = 16)

#add a horizontal line
abline (h = 0,
        lty = "dashed")

#histogram of residuals
hist(summary(fit)$residuals,
     main = "Regression Residuals",
     xlab = "Residual",
    col = "purple" )

#these are all qualitative evaluations. they are good checks to see if other regression
#may be called for.

#shapiro-wilk's test for normality
shapiro.test(summary(fit)$residuals)
#p-value of 0.7503 supports accepting the null hypothesis that the data are normally distributed.

#qqplot
qqnorm(summary(fit)$residuals, pch = 16)

qqline(summary(fit)$residuals, datax = FALSE, distribution = qnorm,
       probs = c(0.25, 0.75), qtype = 7, pch = 16)

#most of these tests are redundant. you can choose which to use based on your situation.
#in order to have a meaningful intercept, it is possible to center the data.
#How do we assess the results? (decision tree)
#do residuals meet base assumptions of normality and standard variance? No -> a linear relationship cannot be determined
                                                                      # Yes -> Are residuals spatially autocorrelated?
              #Yes -> Interpret with CAUTION ! Seek an alternative analysis
              #No -> Does the slope significantly differ from zero?
                                                                  #No -> The dependent variable is not significantly related to the independent variable, show results
                                                                  #Yes -> The dependent variable is significantly related to the indenpendent variable.