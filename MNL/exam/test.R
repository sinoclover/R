require(splines)
X <- c(4,80,3)
print(X)
test <- ns(X, df=4, intercept=TRUE)
print(test)
test <- data.frame(test)
print(test)
