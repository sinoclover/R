# 对multinom()进行验证
library(nnet)

examdata <- read.csv('F://Coding/R/MNL/data/lmexam.csv')
print(examdata)
# 这种数据形式一般多针对个体属性变量对选择的影响，不涉及方案属性变量
# 例如个体的年龄、性别、收入、学历等对于购买某几类产品的选择影响，因此得出的系数是在选定基准参考后针对其他各个选择的相应系数。
# 即代入个体的相关解释变量的向量，根据logit计算值大小转换为概率后，以预测是否选择某一项。
examdata$choice2 <- relevel(examdata$choice, ref = 'E')  # 如果不设置则默认为第一项
exam <- multinom(choice ~ l + a + b, data = examdata)
summary(exam)
