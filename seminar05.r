library(lattice)
library(ggplot2)
library(reshape2)

prDat <- read.table("GSE4051_data.tsv")
str(prDat, max.level = 0)
prDes <- readRDS("GSE4051_design.rds")
str(prDes)

(luckyGenes <- c("1419655_at", "1438815_at"))
prepare_data <- function(x) {
  miniDat <- subset(prDat, rownames(prDat) %in% x)
  miniDat <- data.frame(gExp = as.vector(t(as.matrix(miniDat))),
                        gene = factor(rep(rownames(miniDat), each = ncol(miniDat)),
                                      levels = x))
  miniDat <- suppressWarnings(data.frame(prDes, miniDat))
  miniDat
}

jDat <- prepare_data(luckyGenes)
str(jDat)

stripplot(gExp ~ devStage | gene, jDat,
          group = gType, jitter.data = TRUE,
          auto.key = TRUE, type = c('p', 'a'), grid = TRUE)


make_stripplot <- function(d_frame) {
  gExp <- d_frame[ , "gExp"]
  devStage <- d_frame[ , "devStage"]
  gene <- d_frame[ , "gene"]
  dat <- d_frame
  gType <- d_frame[ , "gType"]
  
  stripplot(gExp ~ devStage | gene, d_frame, group = gType, 
            jitter.data = TRUE, auto.key = TRUE,
            type = c('p', 'a'), grid = TRUE)
}
make_stripplot(jDat)

ggplot(jDat, aes(x = devStage, y = gExp, colour = gType, group = gType)) +
       geom_point() +
       facet_wrap(~ gene) +
   stat_summary(fun.y = mean, geom = "line")

make_stripplot_gg <- function(d_frame) {
devStage <-  d_frame[ , "devStage"]
gExp <-  d_frame[ , "gExp"]
gType <- d_frame[ , "gType"]
gene <- d_frame[ , "gene"]

ggplot(d_frame, aes(x = devStage, y = gExp, colour = gType, group = gType)) +
  geom_point() +
  facet_wrap(~ gene) +
  stat_summary(fun.y = mean, geom = "line")
}

make_stripplot_gg(jDat)

make_stripplot(newDat <- prepare_data("1456341_a_at"))
str(newDat)
head(newDat)

t.dat <- prepare_data("1456341_a_at")
t.test(gExp ~ devStage,
       subset(t.dat, devStage %in% 
                c("P2", "4_weeks"))
       )
# the above result is SLIGHTLY different from
# what is on seminar page

anov.dat <- prepare_data("1438786_a_at")
make_stripplot(anov.dat)

anov.dat_lm <- lm(formula = gExp ~ devStage, data = anov.dat, 
   subset = gType == "wt")

str(anov.dat_lm)
