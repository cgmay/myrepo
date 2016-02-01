suppressPackageStartupMessages(library(dplyr))
gd_url <- "http://tiny.cc/gapminder"
gdf <- read.delim(file = gd_url)
str(gdf)
