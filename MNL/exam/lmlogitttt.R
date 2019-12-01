library(nnet)
library(splines)
require(stats)

data_color <- read.csv('F://Coding/R/MNL/p_color.csv')

ns1 <- ns(data_color$l, df=4)
ns11 <- predict(ns1, 0.63)
ns111 <- ns1-ns11[1,]

ns2 <- ns(data_color$a, df=4)
ns22 <- predict(ns2,-0.4252)
ns222 <- ns2-ns22[1,]

ns3 <- ns(data_color$b, df=4)
ns33 <- predict(ns3, 0.315)
ns333 <- ns3-ns33[1,]

ns <- lm(ln2 ~ ns111+ns222+ns333, data=data_color)
print(ns)
coef(ns)
summary(ns)

