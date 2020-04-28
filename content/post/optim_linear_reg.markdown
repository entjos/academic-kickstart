---
title: "Optimisation of a Linear Regression Model"
author: "Joshua Philipp Entrop"
date: "2020-04-26"
output: md_document
---

In this post I would like to show how to manually optimise a linear regression model using the optim command in R. Usually if you learn how to fit a linear regression model in R, you would learn how to use the lm command to do this. However, if you would like to know how to do this manually, examples are rare.

If you want to optimise a function, the most important question of course is which function should be optimised? As it is taught in most statistic classes this function is in case of a linear regression model the distribution of the residuals (R). We know that this distribution follows a normal distriibution with mean 0 and a unknown standart deviation `\(\sigma\)`:

`$$\sum_{i = 1}^{i = n} R_i \sim N(0, \sigma)$$`

Where R equals:

`$$R_i = y_1 - \hat{y_i}$$`

Thus, we are interested in a function for `\(\hat{y_i}\)` which minimises the residuals and hence, gives us the best estimates for the function of interest. Taking all this information together we can write the function we would like to optimise, shown below.


```r
#Reading the example data set icu from the package aplore3
library(aplore3)
y = icu$sys                 #Set our depended variable
x1 = icu$age                #Set our fist independed variable
x2 = as.numeric(icu$gender) #Set our second independed variable

#Define our liklihood function we like to optimise
ll_lm <- function(par, y, x1, x2){
  
  alpha <- par[1]
  beta1 <- par[2]
  beta2 <- par[3]
  sigma <- par[4]
  
  R = y - alpha - beta1 * x1 - beta2 * x2
  
  -sum(dnorm(R, mean = 0, sigma, log = TRUE))
}
```

A function that can be used for the <TT> optim </TT> command needs to have a <TT> par </TT> argument, which includes the unknown parameters. The <TT> par </TT> arguments needs a vector with initial values or guesses for all unkown parameters. As shown in the example above the <TT> par </TT> argument includes initial values for all 4 unknown parameters. In this example the first value of the <TT> par </TT> arguments equals alpha, the second beta1, the third beta2 and the fourth sigma, which is our unknown standart deviation of the normal distribution of our residuals. Additionally, we included our two independent variables x1 and x2 and our dependened variable y as function arguments in the optim call.

Also note, that I used the <TT> log </TT> argument of the <TT> dnorm </TT> function to get the logaritmic values of the dnorm function. This is necessary, if we wold like to sum the singel liklihood values instead of taking the product of them.

The linear model we would like to fit in this example is:

`$$E(Y|X) = \alpha + \beta_1  x_1 + \beta_2  x_2$$`

Hence, the residuals for this model can be calculated as:

`$$R = y - \alpha - \beta_1 x_1 - \beta_2 x_2$$`

Since we know that these residuals follow a normal distribution with mean 0, we just need to find the standardt deviation for the normal distribution of the residuals and the values for our coefficents, that fits best our data. To do this, we would like to minimise the sum of errors. This is done by the <TT> optim </TT>  command. However, since the <TT> optim </TT> command always maximises functions, we just need to put a minus before our summation.

Before, we run the <TT> optim </TT> command we also need to find good guesses for our estimates, since the initial parameter values which are chosen for the optimisation influences our estimates. In this case we just calculate the  conditional means for our subgroups and use them as guess for our coefficients.


```r
est_alpha <- mean(icu$sys)

est_beta1 <- mean(icu$sys[icu$age >= 40 & icu$age <= 41]) - mean(icu$sys[icu$age >= 41 & icu$age <= 52])

est_beta2 <- mean(icu$sys[icu$gender == "Male"]) - mean(icu$sys[icu$gender == "Female"])

est_sigma <- sd(icu$sys)
```

Now we can use the optim function to search for our maximmum liklihood estimates (mles) for the different coefficents. For the optim function, we need the function we like to optimise, in this case <TT> ll_lm </TT>, our guesses for the estimates and the empirical data we want to use for the optimisation.


```r
mle_par <- optim(fn = ll_lm,                #Function to be optimised
                 par = c(alpha = est_alpha, #Initial values
                         beta1 = est_beta1, 
                         beta2 = est_beta2, 
                         sigma = est_sigma), 
                 y = icu$sys,               #Empirical data from the data set icu
                 x1 = icu$age,
                 x2 = as.numeric(icu$gender))

mle_par$par                                 #Showing estimates for unknown parameters
```

```
##        alpha        beta1        beta2        sigma 
## 123.99854056   0.06173058   3.37040256  32.77094809
```

If we now compare our estimates with the results of the <TT> lm </TT> command for the same model, we can see slight differences in the esitmates, which may be due to different optimisation alogrithms or due to our initial guesses for the parameters.


```r
summary(lm(sys ~ age + as.numeric(gender), data = icu))
```

```
## 
## Call:
## lm(formula = sys ~ age + as.numeric(gender), data = icu)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -98.544 -20.510  -1.445  18.884 122.272 
## 
## Coefficients:
##                     Estimate Std. Error t value Pr(>|t|)    
## (Intercept)        124.39209    9.32708  13.337   <2e-16 ***
## age                  0.06276    0.11738   0.535    0.593    
## as.numeric(gender)   3.09867    4.83773   0.641    0.523    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 33.05 on 197 degrees of freedom
## Multiple R-squared:  0.003889,	Adjusted R-squared:  -0.006224 
## F-statistic: 0.3845 on 2 and 197 DF,  p-value: 0.6813
```

This approach for a manuell optimisation of a linear regression model can also be adopted for other linear regression model with more coefficents. In this case the formula for the residuals needs to be adjusted to the structure of the model that you like to fit.
