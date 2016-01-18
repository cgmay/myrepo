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

## What are the variable names? 

## What “flavor” is each variable,
## i.e. numeric, character, factor? 

## For sample, do a sanity check 
## that each integer between 1 and 
## the number of rows in the dataset 
## occurs exactly once. 

## For each factor variable, what are 
## the levels? 

## How many observations do we have for
## each level of devStage? For gType? 

## Perform a cross-tabulation of devStage 
## and gType.

## If you had to take a wild guess, what 
## do you think the intended experimental 
## design was? 

## What actually happened in real life?

## For each quantitative variable, what 
## are the extremes? How about average or median? 

