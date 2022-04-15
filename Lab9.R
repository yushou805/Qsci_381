#################
# Problem 1 
# Simple Linear Regression
#################
fish <- read.csv("Fish.csv")
head(fish)

# let's look at our response/outcome/y
# it is a continuous variable
# linear regression assumes that the response
# is normally distributed
hist(fish$Height)
# histogram is skewed, so our assumption
# of normality may be wrong

#glm() does linear regression for us!
glm(Height ~ Weight, data = fish,
    family = "gaussian") #R assumes we want Gaussian (normal), so this is unnecessary
#typically use summary() on a linear regression model
mod1 <- glm(Height ~ Weight, data = fish)
summary(mod1)
# How do we interpret output?
# Rows are coefficients
# Columns give estimate, standard error,
#     and test statistic + p-value
# Null Deviance = SST (total sum of squares; total variance)
# Residual Deviance = SSE (sum of squared errors; unexplained variance)

# What is the intercept of this linear regression model? 
# What is the slope of the linear regression model?

# How do we interpret this slope?
# For every one unit increase in weight, 
# a fish’s height is expected to increase 
# by 0.0086729 units

# Look at p-value 
# What are the null and alternative hypotheses
# of this test?
#     H0: beta == 0
#     HA: beta != 0
#     test statistic is calculated as (beta - 0)/SE
0.0086729/0.0006588
# At the alpha = 0.05 level, what do we conclude?
# Is the slope significant?
#   Yes, slope is significantly different from 0
#   We reject the null hypothesis


5.5163660 + 0.0086729*500

# alternative: 
mod1$coefficients
b0 <- mod1$coefficients[1]
b1 <- mod1$coefficients[2]
b0 + b1*500

# R also has a predict() function
new.weights <- data.frame(
  Weight = c(250, 500, 750, 1000)
)

predict(mod1, newdata = new.weights)

# What are residuals?
#   Difference between observed and expected
#   e_i = y_i - y_i_hat
# Why do we care?
#   Regression is found by minimizing the 
#   the sum of squared residuals
# What is the first residual?
#   First, what is observed height and weight?
fish[1,]
height1 <- 11.52
weight1 <- 242
#   Next, what is predicted value?
predicted1 <- b0 + b1*242
predicted1
#or 
mod1$fitted.values[1]
#   Finally, residual = observed - predicted
residual1 <- height1 - predicted1
residual1
#or 
mod1$residuals[1]

#r-squared
SSE <- sum(mod1$residuals^2) #residual deviance
SST <- sum((fish$Height - mean(fish$Height))^2) #null deviance
SST
SST <- var(fish$Height)*(nrow(fish)-1)
SST
1 - SSE/SST
#equivalently
(SST - SSE)/SST
# for simple linear regression, same as
cor(fish$Height, fish$Weight)^2
# conclude that 52% of total variation is explained
# by best-fit line 

# diagnostics
# Plot 1: Residuals vs. Fitted
plot(mod1$fitted.values, mod1$residuals,
     xlab = "Predicted Values",
     ylab = "Residuals",
     main = "Residuals vs. Fitted")
abline(h = 0, col = "red", lty = 2)
# thoughts?
# looks somewhat problematic
# Plot 2: Q-Q Plot
qqnorm(mod1$residuals)
qqline(mod1$residuals, 
       col = "red", lty = 2)
# again, does not look good

#################
# Problem 2
# Multiple Regression
################
mod2 <- glm(Height ~ Weight + Length1 + Width, 
           data = fish,
           family = "gaussian")
summary(mod2)
# What are the slope coefficients for each parameter?
# How can we interpret the coefficient value for weight? 
#     For every one unit increase in weight, 
#     a fish’s height is expected to increase 
#     by 0.005492 units, 
#     holding Length1 and Width constant


#predicting for new data
new.fish<- data.frame(
  Weight  = c(500),
  Length1 = c(27),
  Width   = c(5)
)
  
predict(mod2, newdata = new.fish)

#by-hand
3.236028 + 0.005492*500 + -0.223767*27 + 2.132598*5

#residual of first fish
height1 - mod2$fitted.values[1]
mod2$residuals[1]

#r-squared (unadjusted)
SSE <- sum(mod2$residuals^2)
SSE
SST <- sum((fish$Height - mean(fish$Height))^2)
SST
var(fish$Height)*(nrow(fish)-1)
1 - SSE/SST

#adjusted r-squared
n <- nrow(fish)
q <- 4 #three predictors + intercept
MSE <- SSE/(n - q) #n - q = 155 == df of residual deviance
MST <- SST/(n - 1) #n - 1 = 158 == df of null deviance
1 - MSE/MST

# diagnostics
# Plot 1: Residuals vs. Fitted
plot(mod2$fitted.values, mod2$residuals,
     xlab = "Predicted Values",
     ylab = "Residuals",
     main = "Residuals vs. Fitted")
abline(h = 0, col = "red", lty = 2)
# thoughts?
# looks better, but still not good
# Plot 2: Q-Q Plot
qqnorm(mod2$residuals)
qqline(mod2$residuals, 
       col = "red", lty = 2)
# again, looks better, but has problems

#################
# Problem 3
# Logistic Regression
################

heart <- read.csv("heart_Disease.csv")
head(heart)

table(heart$disease)

plot(heart$max_heart_rate, heart$disease,
     xlab = "Maximum Heart Rate",
     ylab = "Presence of Heart Disease")
# Looks like individuals with high 
# values of maximum heart rate are 
# more likely to have heart disease

# let's look at the disease variable, our outcome
table(heart$disease)
# disease is discrete (categorical)
# specifically, it is binomial
#     "success" = 1
#     "failure" = 0
# thus, need a logistic regression model

# have to modify glm function for logistic regression
mod3 <- glm(disease ~ max_heart_rate, 
            family = "binomial", #family isn't Gaussian
            data = heart)
summary(mod3)
# Output looks pretty similar
# BUT, our coefficients have different interpretations
# For every one unit increase in maximum heart rate, 
# the expected change in log odds of having heart 
# disease is 0.043951

# For every one unit increase in maximum heart rate, 
# what is the expected change in odds of having heart 
# disease?
exp(x+0.043951) = exp(x)*exp(0.043951)
exp(0.043951)
# thus, odds increase by about 4%

# What is the predicted probability of 
# heart disease for someone with a maximum 
# heart rate of 155?
mod3$coefficients
b0 <- mod3$coefficients[1]
b1 <- mod3$coefficients[2]
b0+b1*155
exp(b0 + b1*155)/(1 + exp(b0 + b1*155))

#can also use predict()
new.person <- data.frame(
  max_heart_rate = c(155)
)

predict(mod3, newdata = new.person,
        type = "response") 
#have to specify type = response for logistic model
#otherwise, predict gives logit value
log(0.6037219/(1 - 0.6037219))
predict(mod3, newdata = new.person) 


#################
# Problem 4
# Categorical Predictor
################

sort(unique(fish$Species))
table(fish$ï..Species)

mod4 <- glm(Weight ~ ï..Species, data = fish)
summary(mod4)
#mean of each species
aggregate(fish$Weight, list(fish$Species), mean)
# intercept is mean of Bream, which is serving as the reference
# where do other values come from?
617.82857 - 463.0104 #intercept - Parkki value = mean of Parkki
617.82857 + 100.88   #intercept + Pike value = mean of Pike
