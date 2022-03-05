# 2.28 in class

# subset for iris virginica 
flower <- iris[iris$Species=="virginica",]

# linear model relating petal lenth to sepal length 
fit <- lm(flower$Petal.Length~flower$Sepal.Length)
summary(fit)

# we get 
Call:
  lm(formula = flower$Petal.Length ~ flower$Sepal.Length)

Residuals:
  Min       1Q   Median       3Q      Max 
-0.68603 -0.21104  0.06399  0.18901  0.66402 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)          0.61047    0.41711   1.464     0.15    
flower$Sepal.Length  0.75008    0.06303  11.901  6.3e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.2805 on 48 degrees of freedom
Multiple R-squared:  0.7469,	Adjusted R-squared:  0.7416 
F-statistic: 141.6 on 1 and 48 DF,  p-value: 6.298e-16

# create a scatter plot on sepal and petal length

plot(flower$Sepal.Length, flower$Petal.Length, main = "Iris virginica",
     xlab = "Sepal length", ylab = "Petal length", col= "purple",
     pch=16)

# residual plot 

plot(flower$Sepal.Length, summary(fit)$residuals,
     xlab = "Sepal length", 
     ylab = "Petal length", 
     col= "purple",
     pch=16)

# add a horizontal line
abline(h=0, lty="dashed")
# the residuals are randomly distributed 



# plot the histogram or residuals 

hist(summary(fit)$residuals, main = "regression residuals",
     xlab = "residual" ,
     col="purple")
# it is indeed bell-shaped 


# shapiro test to test whether the residuals are normally distributed

shapiro.test(summary(fit)$residuals)
# the p value is way above 0.05 so the residuals are 
# indeed normally distributed

# qq plot
qqnorm(summary(fit)$residuals, pch=16)

qqline(summary(fit)$residuals, datax= FALSE,
       distribution= qnorm, probs = c(0.25,0.75), 
       qtype = , pch=16)
