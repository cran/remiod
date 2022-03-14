## ---- include = FALSE-----------------------------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE,comment = "#>")
options(tibble.print_min = 4L, tibble.print_max = 4L, width = 100)
library(reshape2)
library(remiod)

## ----data-----------------------------------------------------------------------------------------
data(schizo)

schizow = dcast(schizo, id + tx ~ week, value.var = "imps79o")
colnames(schizow) = c(colnames(schizow)[1:2], paste0("y",colnames(schizow)[-c(1:2)]))

schizow[,colnames(schizow)[-c(1:2)]] = lapply(schizow[,colnames(schizow)[-c(1:2)]], 
                                              function(x) factor(x, levels = c("1", "2", "3", "4"), ordered=TRUE))

head(schizow)

## ----missing, fig.cap="Missing pattern of Schizophrenia data", fig.align="center", fig.height = 5, fig.width = 7, echo=FALSE----
JointAI:: md_pattern(schizow, color = c('#34111b', '#e30f41'), border = 'white',
           legend.position = 'none', print_xaxis = T)


## ----setup----------------------------------------------------------------------------------------
test = remiod(formula = y6 ~ tx + y0 + y1 + y2 + y3 + y4 + y5, data=schizow,
              trtvar = 'tx', algorithm = "jags", method = "MAR", 
              n.iter = 0, warn = FALSE, mess = FALSE) 
list.models(test)

## ----setup_ord------------------------------------------------------------------------------------
test_ord = remiod(formula = y6 ~ tx + y0 + y1 + y2 + y3 + y4 + y5, data=schizow,
                  trtvar = 'tx', algorithm = "jags", model_order = paste0("y",0:5),  
                  method = "MAR", n.iter = 0, warn = FALSE, mess = FALSE) 
list.models(test_ord)

## ----J2R------------------------------------------------------------------------------------------
test_j2r = remiod(formula = y6 ~ tx + y0 + y1 + y3, data = schizow, trtvar = 'tx', 
                  algorithm = "jags", method = "J2R", ord_cov_dummy = F, 
                  n.iter = 20, n.adapt = 10, n.chains = 2, warn = FALSE, mess = FALSE) 
names(test_j2r)

## ----extract--------------------------------------------------------------------------------------
extdt = extract_MIdata(object=test_j2r, method="J2R",mi.setting=NULL, M=10, minspace=2)
table(extdt$Imp_)

