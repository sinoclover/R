# 直接使用multinom进行多项logit回归分析
# 其实mlogit和multinom要求的数据格式一致，也类似于pylogit要求的格式。
# 而mlogit进行数据重塑前的数据反而较为复杂。
library(nnet)
library(splines)
library(rgl)

# 导入常规数据集
rawdata <- read.csv('F://Coding/R/MNL/data/demoR.csv')
print(rawdata)
fit <- multinom(choice ~ l + a + b, data = rawdata)
print(fit)
coef(fit)
# 添加新数据
newdata <- expand.grid(l = seq(0, 100, 10), a = seq(-100, 100, 20), b = seq(-100, 100, 20))
print(newdata)
# 计算效用值
u <- coef(fit)[2] * newdata$l + coef(fit)[3] * newdata$a + coef(fit)[4] * newdata$b
u <- data.frame(u)
print(u)
# 获得相应的效用值与LAB的关系，用以进行可视化

# 附加样条
ns1 <- multinom(choice ~ ns(l, df=4, intercept=TRUE)+ns(a, df=4, intercept=TRUE)+ns(b, df=4, intercept=TRUE) , data = rawdata)
print(ns1)
coef(ns1)[2]

# 结合系数利用上述新数据对样条计算效用值
ns1l <- ns(newdata$l, df=4, intercept = TRUE)
ns1l <- data.frame(ns1l)
print(ns1l)
ul = coef(ns1)[2] * ns1l$X1 + coef(ns1)[3] * ns1l$X2 + coef(ns1)[4] * ns1l$X3 + coef(ns1)[5] * ns1l$X4
print(ul)

ns1a <- ns(newdata$a, df=4, intercept = TRUE)
ns1a <- data.frame(ns1a)
print(ns1a)
ua = coef(ns1)[6] * ns1a$X1 + coef(ns1)[7] * ns1a$X2 + coef(ns1)[8] * ns1a$X3 + coef(ns1)[9] * ns1a$X4
print(ua)

ns1b <- ns(newdata$b, df=4, intercept = TRUE)
ns1b <- data.frame(ns1b)
print(ns1b)
ub = coef(ns1)[10] * ns1b$X1 + coef(ns1)[11] * ns1b$X2 + coef(ns1)[12] * ns1b$X3 + coef(ns1)[13] * ns1b$X4
print(ub)

u_ns = ul + ua + ub
u_ns = data.frame(u_ns)
print(u_ns)
predict(ns1, newdata)

# 导入线性回归数据集
lmdata <- read.csv('F://Coding/R/MNL/data/demolm.csv')
print(lmdata)
summary(lm(logit_value ~ dl + da + db, data = lmdata))
# # 样条样本量不足
# ns2 <- lm(logit_value ~ ns(dl, df=4)+ns(da, df=4)+ns(db, df=4) , data = lmdata)
# print(ns2)
