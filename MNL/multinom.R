# 直接使用multinom进行多项logit回归分析
# 其实mlogit和multinom要求的数据格式一致，也类似于pylogit要求的格式。
# 而mlogit进行数据重塑前的数据反而较为复杂。
library(nnet)
library(splines)

# 导入数据集
rawdata <- read.csv('F://Coding/R/MNL/data/demoR.csv')
print(rawdata)
summary(multinom(choice ~ l + a + b, data = rawdata))
