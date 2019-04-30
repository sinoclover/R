library(mlogit)
library(nnet)
library(splines)

# 例子
# data("HC", package = "mlogit")
# print(HC)
# HC <- mlogit.data(HC, choice = "depvar", varying=c(2:8, 10:16), shape="wide")
# print(HC)

# 导入数据集
rawdata <- read.csv('F://Coding/R/MNL/data/demoR2.csv')
print(rawdata)
# 数据集转换
mydata <- mlogit.data(rawdata, shape='wide', choice='choice', varying = 2:16)
print(mydata)
# write.csv(mydata, file='F://Coding/python/Behance/pylogit/data/demoR.csv')

# # 构建相应的效用函数并进行回归计算
# f <- mFormula(choice ~ l + a + b)
# head(model.matrix(f, mydata), 5)
# res <- mlogit(f, data=mydata)
# summary(res)

# 系统计算奇异
# summary(mlogit(choice ~ l + a + b, data = mydata))

# 使用multinom
summary(multinom(choice ~ l + a + b, data = mydata))
# 附加样条
res <- multinom(choice ~ ns(l, df=4)+ns(a, df=4)+ns(b, df=4) , data = mydata)
summary(res)
