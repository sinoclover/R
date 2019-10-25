# ֱ��ʹ��multinom���ж���logit�ع����
# ��ʵmlogit��multinomҪ������ݸ�ʽһ�£�Ҳ������pylogitҪ��ĸ�ʽ��
# ��mlogit������������ǰ�����ݷ�����Ϊ���ӡ�
library(nnet)
library(splines)

# 1.1����multinom���л�����Ӧ�����ݸ�ʽ���ж���logit�ع�
# ���볣�����ݼ�
rawdata <- read.csv('F://Coding/R/MNL/data/demoR.csv')
print(rawdata)
fit <- multinom(choice ~ l + a + b, data = rawdata)
print(fit)
coef(fit)
# 1.2���������ع�ϵ�����л���ԭ���ݵĸ�����֤
# �������Իع����ݼ���ʹ��ԭ���ݵ�ɫ��ֵ����Ч����֤
lmdata <- read.csv('F://Coding/R/MNL/data/demolm.csv')
print(lmdata)
# ����Ч��ֵ
u_exam <- coef(fit)[2] * lmdata$l + coef(fit)[3] * lmdata$a + coef(fit)[4] * lmdata$b + coef(fit)[1]
print(u_exam)
# ת��Ϊ����ֵ
pr <- exp(u_exam)
print(pr)
# �Ը��ʽ��й�һ������
pr <- pr / sum(pr)
print(pr)
# 1.3ɫ�ʿռ�Ԥ��
# �������Ե�ϵ���������������ݽ���Ԥ��,���������Կռ�
newdata <- expand.grid(l = seq(0, 100, 10), a = seq(-100, 100, 20), b = seq(-100, 100, 20))
print(newdata)
# ����Ч��ֵ
u <- coef(fit)[2] * newdata$l + coef(fit)[3] * newdata$a + coef(fit)[4] * newdata$b
u <- data.frame(u)
print(u) # �����Ӧ��Ч��ֵ��LAB�Ĺ�ϵ�����Խ��п��ӻ�

# 2.1���µ㣬����ƫ��Ӱ��ֵ�����ɫ�ʷ������ʣ�ͨ�����Իع鷴�����ɫ�����Ա���ϵ�����Ӷ���������ɫ�ʿռ��Ԥ�⡣
# ֱ��ͨ���Ը��ʽ���LOGIT��Ȼ���ԭɫ�����ݽ������Իع飬ѡ�����һ����Ϊ��׼�ο���
fit_lm <- lm(logit_value ~ dl + da + db, data = lmdata)
print(fit_lm)
coef(fit_lm)
# 2.2���������ع�ϵ��ͨ��ԭɫ�����ݼ���Ч��ֵ������֤
u_examlm <- coef(fit_lm)[2] * lmdata$l + coef(fit_lm)[3] * lmdata$a + coef(fit_lm)[4] * lmdata$b + coef(fit_lm)[1]
print(u_examlm)
# ת��Ϊ����ֵ
pr_lm <- exp(u_examlm)
print(pr_lm)
# �Ը��ʽ��й�һ������
pr_lm <- pr_lm / sum(pr_lm)
print(pr_lm)

# 3.1����1.1����������չ���������ж���logit�ع��Թ۲ⷽ�����Ա����ķ�������
ns1 <- multinom(choice ~ ns(l, df=4, intercept=TRUE)+ns(a, df=4, intercept=TRUE)+ns(b, df=4, intercept=TRUE) , data = rawdata)
print(ns1)
coef(ns1)
# 3.2���������ع�ϵ�����л���ԭ���ݵĸ�����֤
# ��¼���������������������Ч��ֵ
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

# ������Ч��ֵ
u_ns = ul + ua + ub + coef(ns1)[1]
print(u_ns)
# ת��Ϊ����ֵ
pr_ns <- exp(u_ns)
print(pr_ns)
# �Ը��ʽ��й�һ������
pr_ns <- pr_ns / sum(pr_ns)
print(pr_ns)

# # 4.1�������������㣬����13��ϵ����������5����������ʱ�޷��������Իع顣
# ns2 <- lm(logit_value ~ ns(dl, df=4)+ns(da, df=4)+ns(db, df=4) , data = lmdata)
# print(ns2)

# # ���ϵ���������������ݶ���������Ч��ֵ
# # ע�⣬��������ϵ���ǹ���ԭ���ݵ�����������Ԥ����ԣ�Ӧ������ֵ����ԭ������ֵ������ϣ���֤�ռ����������������ֵ��������������Եġ�
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

