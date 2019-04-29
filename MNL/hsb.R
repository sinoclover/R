require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)

ml <- read.dta("F://Coding/R/MNL/data/hsbdemo.dta")
print(ml)

with(ml, table(ses, prog))

