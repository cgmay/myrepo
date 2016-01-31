library(lattice)
library(ggplot2)
library(plyr)

prDat <- read.table("GSE4051_data.tsv")
str(prDat, max.level = 0)

prDes <- readRDS("GSE4051_design.rds")
str(prDes)

set.seed(987)
(theGene <- sample(1:nrow(prDat), 1))

pDat <- data.frame(prDes, gExp = unlist(prDat[theGene, ]))
str(pDat)

aggregate(gExp ~ gType, pDat, FUN = mean)

# using a plyr function
ddply(pDat, ~ gType, summarize, gExp = mean(gExp) )

#stripplot for t results using lattice
stripplot(gType ~ gExp, pDat)
# again with ggplot2
ggplot(pDat, aes(x = gExp, y = gType)) +
  geom_point()

#t test
(ttRes <- t.test(gExp ~ gType, pDat))
str(ttRes)

# extract important components
ttRes$statistic
ttRes$p.value

wilRes <- wilcox.test(gExp ~ gType, data = pDat)
str(wilRes)

ksRes <- ks.test(pDat$gExp[pDat$gType == "wt"], 
                  pDat$gExp[pDat$gType == "NrlKO"])
ksRes.dat <- c(ksRes$statistic, ksRes$p.value)
wilRes.dat <- c(wilRes$statistic, wilRes$p.value)
ttRes.dat <- c(ttRes$statistic, ttRes$p.value)

(kswtDat <- data.frame(KSTEST = ksRes.dat,WILCOXTEST =  wilRes.dat, 
                      TTEST = ttRes.dat, row.names = c("STATISTIC", "P.VALUE"), 
           stringsAsFactors = FALSE))
kDat <- readRDS("GSE4051_MINI.rds")
kMat <- as.matrix(kDat[c('crabHammer', 'eggBomb', 'poisonFang')])
str(kMat)

#by hand median computation

median(kMat[ , 1])
# or better..
median(kMat[ , 'crabHammer'])

# now using apply
apply(kMat, 2, median)

# another way to do it, using FUN inputs as well
apply(kMat, 2, quantile, probs = 0.5)
# where probs = 0.5 is an argument 
# required by quantile

apply(kMat, 2, quantile, probs = c(0.25, 0.75))

# minimum of each sample
apply(kMat, 1, min)

# the gene which is responsible for
# the minimum value

# [] gives index for the min gene, colnames
# gives the name associated with that value
colnames(kMat)[apply(kMat, 1, which.min)]
# e.g. of index
(k.index <- apply(kMat, 1, which.min))

#built-in functions
rowSums(kMat)
colMeans(kMat)

all.equal(rowSums(kMat), apply(kMat, 1, sum))

# average eggBomb expression by devStage
aggregate(eggBomb ~ devStage, kDat, FUN = mean)

# or a combinaation of factors
aggregate(eggBomb ~ devStage * gType, kDat, FUN = mean)

# or a range rather than just a mean
aggregate(eggBomb ~ devStage * gType, kDat, FUN = range)

## Now the same things with plyr
ddply(kDat, ~ devStage, summarize, avg = mean(eggBomb))
ddply(kDat, ~ gType * devStage, summarize, avg = mean(eggBomb))

## Two sample tests
keepGenes <- c("1431708_a_at", "1424336_at", "1454696_at",
               "1416119_at", "1432141_x_at", "1429226_at" )
miniDat <- subset(prDat, rownames(prDat) %in% keepGenes)
miniDat <- data.frame(gExp = as.vector(t(as.matrix(miniDat))),
                      gene = factor(rep(rownames(miniDat), each = ncol(miniDat)),
                                    levels = keepGenes))
miniDat <- suppressWarnings(data.frame(prDes, miniDat))
str(miniDat)

# plot to visualize "hits" and boring genes
stripplot(gType ~ gExp | gene, miniDat,
          scales = list(x = list(relation = "free")),
          group = gType, auto.key = TRUE)
# ggplot2
ggplot(miniDat, aes(x = gExp, y = gType, colour = gType)) +
  facet_wrap(~ gene, scales = "free") +
  geom_point(alpha = 0.7) +
  theme(panel.grid.major.x = element_blank())

# do a ttest, one gene at first
someDat <- droplevels(subset(miniDat,
                             gene == keepGenes[1]))
t.test(gExp ~ gType, someDat)

# scaling this up to all 6 genes, we cannot
# use aggregate() anymore.using built-in functions
# would be annoying, so lets use plyr!

d_ply(miniDat, ~ gene, function(x) t.test(gExp ~ gType, x), .print = TRUE)

# if we want to retain info. remember t.test output
# is a list
ttRes <- dlply(miniDat, ~ gene, function(x) t.test(gExp ~ gType, x))
names(ttRes)
ttRes[["1431708_a_at"]]

# lets say we know we only want the test 
# statistic and p value
(ttRes <- ddply(miniDat, ~ gene, function(z) {
  zz <- t.test(gExp ~ gType, z)
  round(c(tStat = zz$statistic, pVal = zz$p.value), 4)
  }))

