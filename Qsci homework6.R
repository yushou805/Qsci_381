#Q1
mean <- mean(c(637, 428, 1167, 739, 2050, 1259, 432, 1176, 875, 831, 610))
sd <- sd(c(637, 428, 1167, 739, 2050, 1259, 432, 1176, 875, 831, 610))
qt(0.025,10)
se <- sd/sqrt(11)
me <- qt(0.025,10)*(sd/sqrt(11))
CI <- c(mean-me, mean+me)

#Q2
alpha <- qnorm(0.05)
voted <- 98/200
z <- (voted - 0.53)/sqrt(0.53*0.47/200)
pnorm(-1.133413)*2
