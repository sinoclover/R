# ��multinom()������֤
library(nnet)

examdata <- read.csv('F://Coding/R/MNL/data/lmexam.csv')
print(examdata)
# ����������ʽһ�����Ը������Ա�����ѡ���Ӱ�죬���漰�������Ա���
# �����������䡢�Ա����롢ѧ���ȶ��ڹ���ĳ�����Ʒ��ѡ��Ӱ�죬��˵ó���ϵ������ѡ����׼�ο��������������ѡ�����Ӧϵ����
# ������������ؽ��ͱ���������������logit����ֵ��Сת��Ϊ���ʺ���Ԥ���Ƿ�ѡ��ĳһ�
examdata$choice2 <- relevel(examdata$choice, ref = 'E')  # �����������Ĭ��Ϊ��һ��
exam <- multinom(choice ~ l + a + b, data = examdata)
summary(exam)