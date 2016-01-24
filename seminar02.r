set.seed(1)
min.x <- -5

max.x <- 5

num.samples <- 1000

x <- seq(from = min.x, to = max.x, length = num.samples)

# Open new blank plot with x limits from -5 to 5, and y limits from 0 to 1
plot(c(-5, 5), c(0, 1), xlab = 'x', ylab = 'f(x)',
     main = "Normal probability density function", 
     type = "n")

# Add each density plot one at a time
lines(x, dnorm(x, mean = 0, sd = 0.5), lwd = 2, col = "red")
dnor
lines(x, dnorm(x, mean = 0, sd = 1), lwd = 2, col = "green")

lines(x, dnorm(x, mean = 0, sd = 2), lwd = 2, col = "blue")

lines(x, dnorm(x, mean = -2, sd = 1), lwd = 2, col = "magenta")

# We can also add a legend to the plot  
legend("topright", c("mean=0, sd=0.5", "mean=0, sd=1", "mean=0, sd=2", "mean=-2, sd=1"), col = c("red","green","blue","magenta"), lty = 1, lwd = 2)

## A more elegant approach to add the lines is to use a 
## for loop to go through and add each density to the plot. We will define some variables to loop over first.

normal.mean <- c(0, 0, 0, -2)

normal.sd <- c(0.5, 1, 2, 1)

colors <- c("red", "green", "blue", "magenta")

## Now we can use the for loop to repeatedly call 
## the lines function with different arguments.

# Open new plot with x limits from -5 to 5, and y limits from 0 to 1
plot(c(-5, 5), c(0, 1), xlab = 'x', ylab = 'f(x)', main = "Normal probability density function", type = "n")

# Add density plots with a for loop
for(i in 1:length(normal.mean)){
  lines(x, dnorm(x, mean = normal.mean[i], sd = normal.sd[i]), lwd = 2, col = colors[i])
}

# Add a legend to the plot  
legend("topright", paste0("mean=", normal.mean, ", sd=", normal.sd), col = colors, lty = 1, lwd = 2)
set.seed(1)
n <- 100
normal.mean <- 1
normal.sd <- 1
y <- rnorm(n, mean = normal.mean, sd = normal.sd)
(y.mean <- mean(y))
(y.mean.manual <- (sum(y)/n))


## explore convergence on true mean as sample size
## increases
set.seed(1)

## true mean (comes from other steps which I
## didn't include)
true.mean <- normal.mean
true.variance <- normal.sd^2

# Number of samples
num.samp <- 100

# Size of each sample
samp.size <- 10

# Generate the samples in a matrix with num.samp rows and samp.size columns
y <- matrix(rnorm(n = num.samp * samp.size, mean = normal.mean, sd = normal.sd),
            nrow = num.samp, ncol=samp.size)

y.mean <- apply(y, 1, mean)
y.means <- apply(y, 2, mean)
## 1 is rows, 2 is columns, 1:2 is both

all.equal(apply(y, 1, mean), rowMeans(y))

length(y.mean)
length(y.means)

mean.diff <- y.mean - true.mean
head(mean.diff)

boxplot(mean.diff)

## function for all dis code
normalSampleMean <- function(normal.mean, normal.sd, num.samp, samp.size){
  y <- matrix(rnorm(n = num.samp * samp.size, mean = normal.mean, sd = normal.sd),
              nrow = num.samp, ncol=samp.size)
  
  y.mean <- rowMeans(y)
  
  return(y.mean)
}

samp.sizes <- c(10, 100, 1000, 1e4)

names(samp.sizes) <- paste0("n=", samp.sizes)

samp.sizes

set.seed(1)

num.samp <- 100

y.mean <- sapply(samp.sizes, normalSampleMean,
                 num.samp = num.samp, normal.mean
                 = normal.mean, normal.sd = 
                   normal.sd)

boxplot(y.mean - true.mean, xlab = "Sample size (n)",
        ylab = expression(bar(Y)[n]-mu))


## NOW ON TO THE CLT ------------------------
df <- 1

x <- seq(0, 10, length=1000)

plot(x, dchisq(x, df = df), type="l", xlab='x', ylab='f(x)', main="Chi-square probability density function")

set.seed(1)

# set sample size and the number of samples to draw
samp.size <- 5

num.samp <- 1000

# Draw 1000 samples and compute means
y <- matrix(rchisq(n = num.samp * samp.size, df = df), nrow = num.samp, ncol=samp.size)

y.mean <- rowMeans(y)

## Next, we will compute Zn with the true mean and 
## variance given below.

# Compute the true values
true.mean <- df

true.variance <- df*2

# Compute normalised values
z.n <- (sqrt(samp.size) * (y.mean - true.mean)) / sqrt(true.variance)

# Plot a histogram
hist(z.n, probability=TRUE, xlab=expression(Z[n]))

# Compute the normal density and overlay it on the plot in red
y <- seq(min(z.n), max(z.n), length=1000)

dens <- dnorm(y, mean=0, sd=1)

lines(y, dens, col="red")

## small sample size (5) means our data doesn't 
## fit very well with the normal distribution

## Function
chisqNormalisedMean <- function(df, num.samp, samp.size){
  # Compute the true values
  true.mean <- df
  
  true.variance <- df*2
  
  # Draw samples 
  y <- matrix(rchisq(n = num.samp * samp.size, df = df), nrow = num.samp, ncol=samp.size)
  
  y.mean <- rowMeans(y)
  
  # Compute normalised values
  z.n <- (sqrt(samp.size) * (y.mean - true.mean)) / sqrt(true.variance)
  
  return(z.n)
}

## Plotting code function

plotNormalComparison <- function(df, num.samp, n){
  z.n <- chisqNormalisedMean(df, num.samp, n)
  
  # It will be nice to have a title
  fig.title <- paste0("sample size = ", n)
  
  # Plot a histogram
  hist(z.n, probability=TRUE, main=fig.title, xlab=expression(Z[n]))
  
  # Compute the normal density and overlay it on the plot in red
  y <- seq(min(z.n), max(z.n), length=1000)
  
  dens <- dnorm(y, mean=0, sd=1)
  
  lines(y, dens, col="red")
}

## Now we plot with different n and see how it looks

set.seed(1)

samp.sizes <- c(5, 10, 100, 1000, 1e4)

plot.status <- lapply(samp.sizes, plotNormalComparison, num.samp = num.samp, df = df)

## Kolmogorov-Smirnov test to determine if sample
## is close enough to actual distribution

set.seed(1)

n <- 5

z.n <- chisqNormalisedMean(df, num.samp, n)

ks.test(z.n, pnorm, mean=0, sd=1)

## Indicates liklihood that they are not 
## the same. Now we try with 1000 instead 
## of a measly 5

set.seed(1)

n <- 1000

z.n <- chisqNormalisedMean(df, num.samp, n)

ks.test(z.n, pnorm, mean=0, sd=1)

### Take-home problem
coin.n <- seq.default(from = 1, to = 1000, length = 1000)
coin.p <- 0.5

f.toss <- function(n, p){(sum
  (rbinom(n, 1, p)))/n
  }

plot.default(coin.n, sapply(coin.n, f.toss, p = 
                      coin.p), ylim = c(0, 1), ylab = "Proportion", xlab = "Number of Tosses"
             , main = "Central Tendency of Coin Flips")
abline(h = coin.p, lwd = 1, col = "red")
 

