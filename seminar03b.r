install.packages("ggplot2")
library(ggplot2)
kDat <- readRDS("GSE4051_MINI.rds")
str(kDat)

table(kDat$devStage)
table(kDat$gType)
with(kDat, table(devStage, gType))

qplot(crabHammer, eggBomb, data = kDat)

p <- ggplot(kDat, aes(x = crabHammer, y = eggBomb))
str(p)

(p <- p + geom_point())
(p <- p + stat_smooth())
(p <- p + theme_bw() +
  xlab("Expression of crabHammer") +
ylab("Expression of eggBomb") +
  ggtitle("Scatterplot for expression levels"))

nDat <- 
  with(kDat,
       data.frame(sidChar, sidNum, devStage, gType, crabHammer,
                  probeset = factor(rep(c("eggBomb", "poisonFang"),
                                        each = nrow(kDat))),
                  geneExp = c(eggBomb, poisonFang)))

str(nDat)
(p <- ggplot(nDat, aes(crabHammer, geneExp, color = probeset)) +
  geom_point())

# add smoothing line
(p <- p + stat_smooth(se = F))


# there are 2 lines because the groups are defined in the data
# layer. We can turn it into one line by specifying another
# aesthetic in the new later

(p <- ggplot(nDat, aes(crabHammer, geneExp, color = probeset)) +
  geom_point() +
  stat_smooth(se = F, aes(group = 1)))

# separate plots
(p <- ggplot(nDat, aes(crabHammer, geneExp)) +
  geom_point() +
  facet_wrap(~ probeset))

# colour code by genotype
(p <- ggplot(nDat, aes(crabHammer, geneExp, color = gType)) +
  geom_point() +
  facet_wrap(~ probeset))

#colour code by deveopment stage
(p <- ggplot(nDat, aes(crabHammer, geneExp, color = devStage)) +
  geom_point() +
  facet_wrap(~ probeset))

# Stripplot
oDat <- 
  with(kDat,
       data.frame(sidChar, sidNum, devStage, gType,
                  probeset = factor(rep(c("crabHammer", "eggBomb",
                                          "poisonFang"), each = nrow(kDat))),
                  geneExp = c(crabHammer, eggBomb, poisonFang)
                  ))
str(oDat)
# plotting expression of each probe set
(p <- ggplot(oDat, aes(geneExp, probeset)) +
  geom_point())

# jitter spaces the points out for better visualization
(p <- ggplot(oDat, aes(geneExp, probeset)) +
  geom_point(position = position_jitter(height = 0.3)))

# expression changes over course of development
(p <- ggplot(oDat, aes(devStage, geneExp)) +
  geom_point())

# separated by probe set
(p <- p + facet_wrap(~ probeset))

# with genotype info
(p <- p + aes(color = gType))

# adding averages
(p <- p + stat_summary(fun.y = mean, geom = "point", 
                       shape = 4, size = 4))


# Density plots
(p <- ggplot(oDat, aes(geneExp)) +
  geom_density())

# another method
(p <- ggplot(oDat, aes(geneExp)) +
  stat_density(geom = "line", position = "identity"))

# more similar to lattice, with points at the bottom
<<<<<<< HEAD
(p <- ggplot(oDat, aes(geneExp)) +
  stat_density(geom = "line", position = "identity") +
  geom_point(aes(y = 0.05)))

# If you want you can eliminate overplotting with jitter
(p <- ggplot(oDat, aes(geneExp)) +
  stat_density(geom = "line", position = "identity") +
  geom_point(aes(y = 0.05), position = 
               position_jitter(height = 0.01)))

# adjusting bandwidth (still don't know what this is, perhaps it
# affects the smoothing, i.e. how much gaps or changes in density
# affect the curve. smaller bandwidth is less smooth)
(p <- ggplot(oDat, aes(geneExp)) +
  stat_density(geom = "line", position = "identity", adjust = 0.5) +
  geom_point(aes(y = 0.05), position = 
               position_jitter(height = 0.01)))

#separated by genotype
(p <- p + facet_wrap(~ gType))

# or overlayed by genotype
(p <- ggplot(oDat, aes(geneExp, color = gType)) +
  stat_density(geom = "line", position = "identity", adjust = 0.5) +
  geom_point(aes(y = 0.05), position = 
               position_jitter(height = 0.01)))

# separated now by developmental stage
(p <- ggplot(oDat, aes(geneExp, color = devStage)) +
  stat_density(geom = "line", position = "identity", adjust = 0.5) +
  geom_point(aes(y = 0.01), position = 
               position_jitter(height = 0.01)) +
  facet_wrap(~ gType))

#the other way
(p <- ggplot(oDat, aes(geneExp, color = gType)) +
  stat_density(geom = "line", position = "identity") +
  geom_point(aes(y = 0.01), position = 
               position_jitter(height = 0.01)) +
  facet_wrap(~ devStage))

## Boxplots
(p <- ggplot(oDat, aes(devStage, geneExp)) +
  geom_boxplot())
# separated genotypes
(p <- p + facet_wrap(~ gType))

#violin plot
(p <- ggplot(oDat, aes(devStage, geneExp)) +
  geom_violin())

## Overplotting and plot matrix
prDat <- read.table("GSE4051_data.tsv")
str(prDat, max.level = 0)

prDes <- readRDS("GSE4051_design.rds")
str(prDes)

set.seed(2)
(yo <- sample(1:ncol(prDat), size = 2))

bDat <- data.frame(y = prDat[[yo[1]]], z = prDat[[yo[2]]])
str(bDat)

(p <- ggplot(bDat, aes(z, y)) +
  geom_point())
# make the dots more transparent for interpretability
(p <- ggplot(bDat, aes(z, y)) +
  geom_point(alpha = 0.1))

# density plot
(p <- ggplot(bDat, aes(z, y)) +
  stat_density2d())

# using colours
(p <- ggplot(bDat, aes(z, y)) +
  stat_density2d(geom = "tile", contour = F, aes(fill = ..density..))
+
  scale_fill_gradient(low = "white", high = "blue"))

## Hexbin stuff
library(hexbin)
(p <- ggplot(bDat, aes(z, y)) +
  stat_binhex())

## pairwise scatterplots
set.seed(3)
(yo <- sample(1:ncol(prDat), size = 4))
pairDat <- subset(prDat, select = yo)
str(pairDat)

install.packages("GGally")
library("GGally")
(p <- ggpairs(pairDat))

## A Heatmap
library(RColorBrewer)

# set seed so that we have exactly reproducable results
set.seed(1)

# choose 50 probes out of the 30k to work with
yo <- sample(1:nrow(prDat), size = 50)
hDat <- prDat[yo, ]
colnames(hDat) <- with(prDes, paste(devStage, gType, sidChar, sep = "_"))

#transform the data to tall format
prDatTall <- data.frame(sample = rep(colnames(hDat), each = nrow(hDat)), 
                        probe = rownames(hDat),
                        expression = unlist(hDat))

# creat a blue -> purple palette
jBuPuFun <- colorRampPalette(brewer.pal(n = 9, "BuPu"))
paletteSize <- 256
jBuPuPalette <- jBuPuFun(paletteSize)

# heatmap!
ggplot(prDatTall, aes(probe, sample, fill = expression)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  geom_tile() +
  scale_fill_gradient2(low = jBuPuPalette[1],
                       mid = jBuPuPalette[paletteSize/2],
                       high = jBuPuPalette[paletteSize],
                       midpoint = (max(prDatTall$expression) +
                                     min(prDatTall$expression)) / 2,
                       name = "Expression")

