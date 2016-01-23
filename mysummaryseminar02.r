mysummary <- function(x,npar=TRUE,print=TRUE) {
  if (!npar) {
    center <- mean(x); spread <- sd(x) 
  } else {
    center <- median(x); spread <- mad(x) 
  }
  if (print & !npar) {
    cat("Mean=", center, "\n", "SD=", spread, "\n")
  } else if (print & npar) {
    cat("Median=", center, "\n", "MAD=", spread, "\n")
  }
  result <- list(center=center,spread=spread)
  return(result)
}

mysummary(rpois(220, 6), npar=FALSE)

# transpose of a matrix
# a poor alternative to built-in t() function

mytrans <- function(x) { 
  if (!is.matrix(x)) {
    warning("argument is not a matrix: returning NA")
    return(NA_real_)
  }
  y <- matrix(1, nrow=ncol(x), ncol=nrow(x)) 
  for (i in 1:nrow(x)) {
    for (j in 1:ncol(x)) {
      y[j,i] <- x[i,j] 
    }
  }
  return(y)
}

# try it
z <- matrix(1:10, nrow=5, ncol=2)
tz <- mytrans(z)
tz

prDat <- read.table("GSE4051_MINI.txt", header = TRUE, row.names = 1)
attach(iris)
by(iris[, 1:4], Species, colMeans)

head(prDat)
by(prDat[, 4:6], prDat$devStage, colMeans)
