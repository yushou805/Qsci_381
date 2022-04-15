# Q1
Lake <- read.csv("LakeHuron.csv")
mean(Lake$Level)
median(Lake$Level)
# Q2
var(Lake$Level)
sd(Lake$Level)
# Q3
hist(Lake$Level, xlab = "Lake Level", ylab = "Count", 
     main = "Histogram of Lake Level")
# Q4
pnorm(580.01,579.0041,1.318299)
qnorm(0.7772769,0,1)
# Q5
pnorm(580.01,579.0041,1.318299)
pnorm(580.01,579.0041,1.318299,lower.tail = F)
# Q6
qnorm(0.25,579.0041,1.318299)
# Q7
qnorm(0.9,579.0041,1.318299)
# Q8
data <- pnorm(Lake$Level,579.0041,1.318299)
plot(Lake$Level, data, xlab = "Lake Level", ylab = "Probability", 
     main = "Cumulative Probabilties for all levels ")
# Q9
qqnorm(Lake$Level, ylab = "Lake Level", main="QQ-Plot of Lake Level");qqline(Lake$Level)
