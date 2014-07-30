#!/usr/bin/Rscript --vanilla



x1 <- runif(100, min=0, max=10)
x2 <- runif(100, min=0, max=10)
y1 <- ifelse(x1+x2 > 10, 1, 0)

tester <- data.frame(
  exam1 = x1,
  exam2 = x2,
  admitted = y1)

write.csv(tester, "atester.csv", row.names=FALSE)


mymodel <- glm(admitted ~ exam1 + exam2, data=tester, family="binomial",
               control = list(maxit = 50))
