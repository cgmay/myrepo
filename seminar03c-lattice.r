library(lattice)
kDat <- readRDS("GSE4051_MINI.rds")
str(kDat)

table(kDat$devStage)
table(kDat$gType)

with(kDat, table(devStage, gType))

# Scatterplot
xyplot(eggBomb ~ crabHammer, kDat)

xyplot(poisonFang ~ crabHammer, kDat)

# Both plots at once!
xyplot(eggBomb + poisonFang ~ crabHammer, 
       kDat, auto.key = TRUE)

# different method
xyplot(eggBomb + poisonFang ~ crabHammer, kDat,
       outer = TRUE, grid = TRUE)

# now add genotype
xyplot(eggBomb + poisonFang ~ crabHammer,
       kDat, outer = TRUE, grid = TRUE, 
       groups = gType, auto.key = TRUE)

# to do this properly, we must reshape
nDat <-
  with(kDat,
       data.frame(sidChar, sidNum, devStage, gType, crabHammer,
                  probeset = factor(rep(c("eggBomb", "poisonFang"), 
                                        each = nrow(kDat))),
                  geneExp = c(eggBomb, poisonFang)))
str(nDat)
head(nDat)

# now the plotting using nDat
xyplot(geneExp ~ crabHammer | probeset, nDat,
       grid = TRUE, groups = gType,
       auto.key = TRUE)

# self-directed different version
xyplot(geneExp ~ crabHammer | probeset, nDat,
       grid = TRUE, groups = devStage,
       auto.key = TRUE)

## Stripplot
oDat <-
  with(kDat,
       data.frame(sidChar, sidNum, devStage, gType,
                  probeset = factor(rep(c("crabHammer", "eggBomb",
                                          "poisonFang"), each = nrow(kDat))),
                  geneExp = c(crabHammer, eggBomb, poisonFang)))
str(oDat)

stripplot(~ geneExp, oDat)

stripplot(probeset ~ geneExp, oDat, 
          jitter.data = TRUE)

stripplot(~ geneExp | probeset, oDat,
          layout = c(nlevels(oDat$probeset), 1))

stripplot(~ geneExp | probeset, oDat,
          groups = gType, auto.key = TRUE,
          layout = c(nlevels(oDat$probeset), 1))

stripplot(geneExp ~ devStage, oDat)

stripplot(geneExp ~ devStage | probeset, oDat,
          layout = c(nlevels(oDat$probeset), 1))

stripplot(geneExp ~ devStage | probeset, oDat,
          layout = c(nlevels(oDat$probeset), 1),
          groups = gType, auto.key = TRUE)

# adding averages. p and a are what define how 
# the mean is displayed
stripplot(geneExp ~ devStage | probeset, oDat,
          layout = c(nlevels(oDat$probeset), 1),
          groups = gType, auto.key = TRUE,
          grid = TRUE, type = c('p', 'a'))

## Density Plot

densityplot(~ geneExp, oDat)

densityplot(~ geneExp | gType, oDat, grid = TRUE)

densityplot(~ geneExp | probeset, oDat,
            groups = gType, auto.key = TRUE,
            grid = TRUE)

# playing with bandwidth and n. bandwidth specifies
# the underlying spread. n controls number of points
# at which kernel density is estimated. more points
# means smoother curve
jBw <- 0.2
jn <- 400

densityplot(~ geneExp, oDat, groups = gType,
            auto.key = TRUE,
            bw = jBw, n = jn,
            main = paste("bw =", jBw, ", n =", jn))

## Boxplot
bwplot(geneExp ~ devStage, oDat)

bwplot(geneExp ~ devStage | gType, oDat)

# violin plot
# check out the panel stuff, don't know what
# it means yet
bwplot(geneExp ~ devStage, oDat,
       panel = panel.violin)

## Heatmaps
prDes <- readRDS("GSE4051_design.rds")
prDat <- read.table("GSE4051_data.tsv")

set.seed(1)
(yo <-  sample(1:nrow(prDat), size = 50))
hDat <- prDat[yo, ]

hDat <- as.matrix(t(hDat))
row.names(hDat) <- with(prDes,
                       paste(devStage, gType, sidChar, sep="_"))

heatmap(hDat, Rowv = NA, Colv = NA, 
        scale = "none", 
        margins = c(5, 8))

heatmap(hDat, Rowv = NA, Colv = NA,
        col = cm.colors(256), scale = "none",
        margins = c(5, 8))

library(RColorBrewer)
display.brewer.all()

jGreysFun <- colorRampPalette(brewer.pal(n = 9, "Greys"))
jBuPuFun <- colorRampPalette(brewer.pal(n = 9, "BuPu"))

heatmap(hDat, Rowv = NA, Colv = NA, scale = "none",
        margins = c(5, 8), col = jGreysFun(256))

heatmap(hDat, Rowv = NA, Colv = NA, scale = "none",
        margins = c(5, 8), col = jBuPuFun(256))

# allow dendrograms
heatmap(hDat,
        margins = c(5, 8), col = jBuPuFun(256))

heatmap(hDat, margins = c(5, 8),
        scale = c("column"), col = jBuPuFun(256))

library(gplots)
heatmap.2(hDat, col = jBuPuFun, trace = "none")

library(hexbin)
set.seed(924)
(yo <- sample(1:ncol(prDat), size = 2))
y <- prDat[[yo[1]]]
z <- prDat[[yo[2]]]
str(y)
str(z)

xyplot(y ~ z, asp = 1)

# find the points! various options
smoothScatter(y ~ z, asp = 1) #using graphics
xyplot(y ~ z, asp = 1, panel = 
         panel.smoothScatter,
       nbin = 150) #using lattice
hexbinplot(y ~ z) #using hexbin


# Plot matrix
set.seed(3)
(yo <- sample(1:ncol(prDat), size = 4))

pairDat <- subset(prDat, select = yo)

#plotting using base function
pairs(pairDat)
pairs(pairDat,
      panel = function(...)
      smoothScatter(..., add = TRUE))

#using lattice
splom(pairDat)
splom(pairDat,
      panel = panel.smoothScatter,
      raster = TRUE)

#using hexbin
hexplom(pairDat)

