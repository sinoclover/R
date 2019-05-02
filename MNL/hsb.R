require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)

ml <- read.dta("F://Coding/R/MNL/data/hsbdemo.dta")
print(ml)

with(ml, table(ses, prog))
ml$prog2 <- relevel(ml$prog, ref = "academic")
test <- multinom(prog2 ~ read + write + math, data = ml)
summary(test)
