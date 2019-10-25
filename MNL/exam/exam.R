library(nnet)
library(splines)

rawdata <- read.csv('F://Coding/R/MNL/data/demoR.csv')
print(rawdata)
ns1 <- multinom(choice ~ ns(l-78, df=4)+ns(a-2, df=4)+ns(b-73, df=4), data = rawdata)
print(ns1)
ns2 <- multinom(choice ~ ns(l, df=4, intercept=TRUE)+ns(a, df=4, intercept=TRUE)+ns(b, df=4, intercept=TRUE), data = rawdata)
print(ns2)

data <- read.csv('F://Coding/R/MNL/data/demolm2.csv')
print(data)
ns3 <- lm(result ~ ns(l, df=4)+ns(a, df=4)+ns(b, df=4), data=data)
print(ns3)
ns4 <- lm(result ~ ns(l, df=4, intercept=TRUE)+ns(a, df=4, intercept=TRUE)+ns(b, df=4, intercept=TRUE), data=data)
print(ns4)

# зЂвт
L = ns(data$l, df=4)
A = ns(data$a, df=4)
B = ns(data$b, df=4)

baseline_l <- as.vector(L[15,])
baseline_l <- matrix(rep(baseline_l, each=15), ncol = 4)
L1 <- L - baseline_l
print(L1)
baseline_a <- as.vector(A[15,])
baseline_a <- matrix(rep(baseline_a, each=15), ncol = 4)
A1 <- A - baseline_a
print(A1)
baseline_b <- as.vector(B[15,])
baseline_b <- matrix(rep(baseline_b, each=15), ncol = 4)
B1 <- B - baseline_b
print(B1)

ns5 <- lm(data$result ~ L + A + B)
ns6 <- lm(data$result ~ L1 + A1 + B1)
print(ns5)
print(ns6)
