library(data.table)
library(lattice)
library(limma)
library(plyr)
library(dplyr)


p_dat <- fread("XCMS_diffreport-HyperPhe.tsv", select = c(1:31))

(p_dat_rt <- p_dat %>% 
  select(c(2:6, 6, 9, 15:26, 28:31)) %>% 
  arrange(desc(rtmed)))

hp_pat <- c("QT_121004_26", "QT_121004_34", "QT_121004_48", 
            "QT_121004_56", "QT_121004_64", "QT_121004_66")
c_pat <- c("QT_121004_21", "QT_121004_23", "QT_121004_25", 
           "QT_121004_27", "QT_121004_31", "QT_121004_33", 
           "QT_121004_35", "QT_121004_37", "QT_121004_41", 
           "QT_121004_43")


p_dat_sig <- adply(p_dat_rt, 1, .fun = function(x) {
  t.test(subset(x, subset = "rtmed" > 0, select = hp_pat),
         subset(x, subset = "rtmed" > 0, select = c_pat))$p.value
  }) 

p_dat_sig <- rename(p_dat_sig, replace = c("V1" = "tt_pvalue"))
p_dat_sig <- p_dat_sig %>% 
  filter(tt_pvalue < 0.05)

head(p_dat_sig)
p_dat_sigorder <- p_dat_sig %>% 
  arrange(tt_pvalue) %>% 
  select(c(1, 5:ncol(p_dat_sig)))

pdat_heatmap <- p_dat_sig %>% 
  select(c(7:(ncol(p_dat_sig)-1)))

library(RColorBrewer)
bl_pu_fun <- colorRampPalette(brewer.pal(n = 9, "BuPu"))

pmat_heatmap <- as.matrix(t(pdat_heatmap))
heatmap(pmat_heatmap, margins = c(5, 8), col = bl_pu_fun(256),
        scale = c("column"))


