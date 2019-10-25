# 直接使用multinom进行多项logit回归分析
# 其实mlogit和multinom要求的数据格式一致，也类似于pylogit要求的格式。
# 而mlogit进行数据重塑前的数据反而较为复杂。
library(nnet)
library(splines)

# 1.1利用multinom进行基于相应的数据格式进行多项logit回归
# 导入常规数据集
rawdata <- read.csv('F://Coding/R/MNL/data/demoR.csv')
print(rawdata)
fit <- multinom(choice ~ l + a + b, data = rawdata)
print(fit)
coef(fit)
# 1.2根据上述回归系数进行基于原数据的概率验证
# 导入线性回归数据集，使用原数据的色彩值进行效用验证
lmdata <- read.csv('F://Coding/R/MNL/data/demolm.csv')
print(lmdata)
# 计算效用值
u_exam <- coef(fit)[2] * lmdata$l + coef(fit)[3] * lmdata$a + coef(fit)[4] * lmdata$b + coef(fit)[1]
print(u_exam)
# 转换为概率值
pr <- exp(u_exam)
print(pr)
# 对概率进行归一化处理
pr <- pr / sum(pr)
print(pr)
# 1.3色彩空间预测
# 对于线性的系数可以添加新数据进行预测,都处于线性空间
newdata <- expand.grid(l = seq(0, 100, 10), a = seq(-100, 100, 20), b = seq(-100, 100, 20))
print(newdata)
# 计算效用值
u <- coef(fit)[2] * newdata$l + coef(fit)[3] * newdata$a + coef(fit)[4] * newdata$b
u <- data.frame(u)
print(u) # 获得相应的效用值与LAB的关系，用以进行可视化

# 2.1创新点，利用偏好影响值计算各色彩方案概率，通过线性回归反推相关色彩属性变量系数，从而进行整个色彩空间的预测。
# 直接通过对概率进行LOGIT，然后对原色彩数据进行线性回归，选择最后一项最为基准参考。
fit_lm <- lm(logit_value ~ dl + da + db, data = lmdata)
print(fit_lm)
coef(fit_lm)
# 2.2根据上述回归系数通过原色彩数据计算效用值进行验证
u_examlm <- coef(fit_lm)[2] * lmdata$l + coef(fit_lm)[3] * lmdata$a + coef(fit_lm)[4] * lmdata$b + coef(fit_lm)[1]
print(u_examlm)
# 转换为概率值
pr_lm <- exp(u_examlm)
print(pr_lm)
# 对概率进行归一化处理
pr_lm <- pr_lm / sum(pr_lm)
print(pr_lm)

# 3.1对于1.1进行样条基展开，并进行多项logit回归以观测方案属性变量的非线性性
ns1 <- multinom(choice ~ ns(l, df=4, intercept=TRUE)+ns(a, df=4, intercept=TRUE)+ns(b, df=4, intercept=TRUE) , data = rawdata)
print(ns1)
coef(ns1)
# 3.2根据上述回归系数进行基于原数据的概率验证
# 记录各个样条基函数并计算各效用值
ns1l <- ns(lmdata$l, df=4, intercept = TRUE)
print(ns1l)
ns1l <- data.frame(ns1l)
ul = coef(ns1)[2] * ns1l$X1 + coef(ns1)[3] * ns1l$X2 + coef(ns1)[4] * ns1l$X3 + coef(ns1)[5] * ns1l$X4

ns1a <- ns(lmdata$a, df=4, intercept = TRUE)
ns1a <- data.frame(ns1a)
ua = coef(ns1)[6] * ns1a$X1 + coef(ns1)[7] * ns1a$X2 + coef(ns1)[8] * ns1a$X3 + coef(ns1)[9] * ns1a$X4

ns1b <- ns(lmdata$b, df=4, intercept = TRUE)
ns1b <- data.frame(ns1b)
ub = coef(ns1)[10] * ns1b$X1 + coef(ns1)[11] * ns1b$X2 + coef(ns1)[12] * ns1b$X3 + coef(ns1)[13] * ns1b$X4

# 计算总效用值
u_ns = ul + ua + ub + coef(ns1)[1]
print(u_ns)
# 转换为概率值
pr_ns <- exp(u_ns)
print(pr_ns)
# 对概率进行归一化处理
pr_ns <- pr_ns / sum(pr_ns)
print(pr_ns)

# # 4.1样条样本量不足，具有13个系数，但仅有5个样本，暂时无法进行线性回归。
# ns2 <- lm(logit_value ~ ns(dl, df=4)+ns(da, df=4)+ns(db, df=4) , data = lmdata)
# print(ns2)

# # 结合系数利用上述新数据对样条计算效用值
# # 注意，上述样条系数是关于原数据的样条，对于预测而言，应限制最值等于原数据最值进行拟合，保证空间的连续，而超出最值外的数据则是线性的。
# ns1l <- ns(newdata$l, df=4, intercept = TRUE)
# ns1l <- data.frame(ns1l)
# print(ns1l)
# ul = coef(ns1)[2] * ns1l$X1 + coef(ns1)[3] * ns1l$X2 + coef(ns1)[4] * ns1l$X3 + coef(ns1)[5] * ns1l$X4
# print(ul)
# 
# ns1a <- ns(newdata$a, df=4, intercept = TRUE)
# ns1a <- data.frame(ns1a)
# print(ns1a)
# ua = coef(ns1)[6] * ns1a$X1 + coef(ns1)[7] * ns1a$X2 + coef(ns1)[8] * ns1a$X3 + coef(ns1)[9] * ns1a$X4
# print(ua)
# 
# ns1b <- ns(newdata$b, df=4, intercept = TRUE)
# ns1b <- data.frame(ns1b)
# print(ns1b)
# ub = coef(ns1)[10] * ns1b$X1 + coef(ns1)[11] * ns1b$X2 + coef(ns1)[12] * ns1b$X3 + coef(ns1)[13] * ns1b$X4
# print(ub)
# 
# u_ns = ul + ua + ub
# u_ns = data.frame(u_ns)
# print(u_ns)


