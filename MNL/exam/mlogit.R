library(mlogit)
library(nnet)
library(splines)

# ����
# data("HC", package = "mlogit")
# print(HC)
# HC <- mlogit.data(HC, choice = "depvar", varying=c(2:8, 10:16), shape="wide")
# print(HC)

# �������ݼ�
rawdata <- read.csv('F://Coding/R/MNL/data/demoR2.csv')
print(rawdata)
# ���ݼ�ת��
mydata <- mlogit.data(rawdata, shape='wide', choice='choice', varying = 2:16)
print(mydata)
# write.csv(mydata, file='F://Coding/python/Behance/pylogit/data/demoR.csv')

# # ������Ӧ��Ч�ú��������лع����
# f <- mFormula(choice ~ l + a + b)
# print(f)
# head(model.matrix(f, mydata), 5)
# res <- mlogit(f, data=mydata)
# summary(res)

# ϵͳ��������
# summary(mlogit(choice ~ l + a + b, data = mydata))

# ʹ��multinom
summary(multinom(choice ~ l + a + b, data = mydata))
# ��������
res <- multinom(choice ~ ns(l, df=4)+ns(a, df=4)+ns(b, df=4) , data = mydata)
summary(res)