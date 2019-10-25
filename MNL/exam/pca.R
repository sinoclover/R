testdata <- read.csv('F:/Coding/R/MNL/data/pcatest.csv')
print(testdata)
pr <- princomp(testdata, cor=TRUE, scores = TRUE)
summary(pr, loadings=TRUE)
        
