set.seed(1)
min.x <- -5

max.x <- 5

num.samples <- 1000

x <- seq(from = min.x, to = max.x, length = num.samples)
# Open new blank plot with x limits from -5 to 5, and y limits from 0 to 1
plot(c(-5, 5), c(0, 1), xlab = 'x', ylab = 'f(x)', main = "Normal probability density function", type = "n")

# Add each density plot one at a time
lines(x, dnorm(x, mean = 0, sd = 0.5), lwd = 2, col = "red")

lines(x, dnorm(x, mean = 0, sd = 1), lwd = 2, col = "green")

lines(x, dnorm(x, mean = 0, sd = 2), lwd = 2, col = "blue")

lines(x, dnorm(x, mean = -2, sd = 1), lwd = 2, col = "magenta")

# We can also add a legend to the plot  
legend("topright", c("mean=0, sd=0.5", "mean=0, sd=1", "mean=0, sd=2", "mean=-2, sd=1"), col = c("red","green","blue","magenta"), lty = 1, lwd = 2