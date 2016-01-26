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

