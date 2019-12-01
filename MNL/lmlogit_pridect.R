library(nnet)
library(splines)
require(stats)

# 写入自变量及因变量
data_color <- read.csv('F://Coding/R/MNL/p_color.csv')
print(data_color)

# 构建变量
ns1 <- ns(data_color$l, df=4)
ns11 <- predict(ns1, 0.31)
nsDL <- ns1-ns11[1,]

ns2 <- ns(data_color$a, df=4)
ns22 <- predict(ns2,-0.0157)
nsDA <- ns2-ns22[1,]

ns3 <- ns(data_color$b, df=4)
ns33 <- predict(ns3, -0.2677)
nsDB <- ns3-ns33[1,]

# 进行线性回归并引入自然三次样条
ns <- lm(ln2 ~ nsDL+nsDA+nsDB, data=data_color)
print(ns)
coef(ns)
summary(ns)

# 数据节点内的全色彩空间数据构造
newdata <- expand.grid(l = seq(min(data_color$l), max(data_color$l), 0.1), 
                       a = seq(min(data_color$a), max(data_color$a), 0.1), 
                       b = seq(min(data_color$b), max(data_color$b), 0.1))
print(newdata)
class(newdata)
# 根据获得的系数进行色彩效用推广
# 记录各个样条基函数并计算各效用值
L <- ns(newdata$l, df=4)
L <- data.frame(L)
print(L)
ul = coef(ns)[2] * L$X1 + coef(ns)[3] * L$X2 + coef(ns)[4] * L$X3 + coef(ns)[5] * L$X4
A <- ns(newdata$a, df=4)
A <- data.frame(A)
print(A)
ua = coef(ns)[6] * A$X1 + coef(ns)[7] * A$X2 + coef(ns)[8] * A$X3 + coef(ns)[9] * A$X4
B <- ns(newdata$b, df=4)
B <- data.frame(B)
print(B)
ub = coef(ns)[10] * B$X1 + coef(ns)[11] * B$X2 + coef(ns)[12] * B$X3 + coef(ns)[13] * B$X4
u = ul+ua+ub
print(u)
newdata$u <- u
print(newdata)
write.csv(newdata, file = 'F://Coding/R/MNL/data_color_pridect.csv', row.names = FALSE)

