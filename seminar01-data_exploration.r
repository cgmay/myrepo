prDat <- read.table("GSE4051_MINI.txt", header = TRUE, row.names = 1)

## How many rows are there?
nrow(prDat)
dim(prDat)

## How many columns or variables are there?
ncol(prDat)
length(prDat)

## Inspect the first few observations or
## the last few or a random sample.
head(prDat)
tail(prDat)
prDat[sample(nrow(prDat), 7), ]

## What does row correspond to - different 
## genes or different mice?
str(prDat)
## different mice. 

## What are the variable names? 
names(prDat)
dimnames(prDat)

## What “flavor” is each variable,
## i.e. numeric, character, factor? 
str(prDat)

## For sample, do a sanity check 
## that each integer between 1 and 
## the number of rows in the dataset 
## occurs exactly once. 
identical(seq(from = 1, to = nrow(prDat)), 
          sort(prDat$sample))

## For each factor variable, what are 
## the levels? 
str(prDat)
levels(prDat$devStage)
levels(prDat$gType)

## How many observations do we have for
## each level of devStage? For gType? 
summary(prDat$devStage)
summary(prDat$gType)

table(prDat$devStage)
table(prDat$gType)

## Perform a cross-tabulation of devStage 
## and gType.
table(prDat$devStage, prDat$gType)

## If you had to take a wild guess, what 
## do you think the intended experimental 
## design was? 

#  To analyze the expression of 3 genes 
#  (crabHammer, eggBomb, and poisonFang)
#  throughout development stages in WT and
#  nrl KO mice.

## What actually happened in real life?

#  there was no significant difference between the
#  expression of any genes in the different
#  genotypes

## For each quantitative variable, what 
## are the extremes? How about average or median? 
max(prDat$crabHammer)
min(prDat$crabHammer)
mean(prDat$crabHammer)
median(prDat$crabHammer)

max(prDat$eggBomb)
min(prDat$eggBomb)
mean(prDat$eggBomb)
median(prDat$eggBomb)

range(prDat$crabHammer)
summary(prDat$crabHammer)
quantile()
fivenum()