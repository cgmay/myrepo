set.seed(540)
n <- 10
B <- 4

x <- matrix(rnorm(n * B), nrow = n)
rownames(x) <- sprintf("obs%02d", 1:n)
colnames(x) <- sprintf("samp%02d", 1:B)



mean(x[ , 2])
colMeans(x)
apply(x, 2, mean)

mean(colMeans(x))

samp.n <- c(10, 100, 1000, 10000)
samp.rv <- lapply(samp.n, rnorm)
samp.mean <- sapply(samp.rv, mean)
samp.sem <- sapply(samp.rv, function(x) sd(x)/sqrt(length(x)))
samp.iqr <- sapply(samp.rv, IQR)
samp.mad <- sapply(samp.rv, mad)

NormalDis <- data.frame(sampSize = samp.n,
           sampMean = samp.mean,
           sampIQR = samp.iqr,
           sampMAD = samp.mad,
           obsSEM = samp.sem,  
           row.names = sapply(samp.n, function(x) paste0("n", x))
           )

C2samp.n <- c(10, 100, 1000, 10000)
C2samp.rv <- lapply(C2samp.n, function(x) rchisq(x, 2))
C2samp.mean <- sapply(C2samp.rv, mean)
C2samp.sem <- sapply(C2samp.rv, function(x) sd(x)/sqrt(length(x)))
C2samp.iqr <- sapply(C2samp.rv, IQR)
C2samp.mad <- sapply(C2samp.rv, mad)

Chi2Dis <- data.frame(sampSize = C2samp.n,
           sampMean = C2samp.mean,
           sampIQR = C2samp.iqr,
           sampMAD = C2samp.mad,
           obsSEM = C2samp.sem,  
           row.names = sapply(samp.n, function(x) paste0("n", x))
)

set.seed(1)

## normal comparison of probabilities
samp.size <- 1000
true.mean <- 5
true.sd <- 2
threshhold <- 8

r.norm <- rnorm(samp.size, mean = true.mean, sd = true.sd)
p.norm <- pnorm(threshhold, mean = true.mean, sd = true.sd)

p.norm
mean(r.norm <= threshhold)

# cauchy comparison of probabilities
ca.samp.size <- 1000
ca.true.mean <- 5
ca.true.sd <- 2
ca.threshhold <- 8

r.cauchy <- rcauchy(ca.samp.size)
p.cauchy <- pcauchy(ca.threshhold)

1 - p.cauchy
1 - mean(r.cauchy <= ca.threshhold)

# prob of being within an interval
lower.i <- 2
upper.i <- 7
sum(r.norm > lower.i & r.norm < upper.i)/
  length(r.norm)

library(lattice)
set.seed(500)
## theoretical vs. empirical distriubution for a single sample
## demo of the Central Limit Theorem
n <- 35
x  <- rnorm(n)

densityplot(~ x)

densityplot(~x, n = 200, ylim = dnorm(0) * c(-0.1, 1.15),
            panel = function(x, ...) {
              panel.densityplot(x, ...)
              panel.mathdensity(n = 200, col.line = "grey74")
            })

## empirical distribution of sample means for various sample sizes
B <- 1000
n <- round(10^(seq(from = 1, to = 2.5, length = 4)), 0)
names(n) <- paste0("n", n)
getSampleMeans <- function(n, B) colMeans(matrix(rnorm(n * B), nrow = n))
x <- data.frame(sapply(n, getSampleMeans, B))

## using the "extended formula interface" in lattice
jFormula <- as.formula(paste("~", paste(names(n), sep = "", collapse = " + ")))
## building the formula programmatically is slicker than the alternative, which
## is hard wiring to "~ n10 + n32 + n100 + n316", which is not a crime
densityplot(jFormula, x, xlab = "sample means",
            auto.key = list(x = 0.9, y = 0.9, corner = c(1, 1),
                            reverse.rows = TRUE))

## keeping the data "tidy", i.e. tall and skinny, for a happier life
xTallSkinny <- stack(x)
names(xTallSkinny) <- c("x","n")
xTallSkinny$n <- factor(xTallSkinny$n, levels = colnames(x))
densityplot(~ x, xTallSkinny, xlab = "sample means", groups = n,
            auto.key = list(x = 0.9, y = 0.9, corner = c(1, 1),
                            reverse.rows = TRUE))