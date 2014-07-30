#!/usr/bin/Rscript --vanilla



# workspace cleanup
rm(list=ls())

# options
options(echo=TRUE)
options(stringsAsFactors=FALSE)


library(dplyr)
library(ggplot2)


#training <- read.csv("./second.csv") %>%
#training <- read.csv("./acceptance.csv", colClasses="numeric") %>%
training <- read.csv("./atester.csv") %>%
  mutate(yesorno=admitted==1)




mod <- glm(training$admitted ~ training$exam1 + training$exam2,
           family="binomial")

#mslope <- coef(mod)[2]/(-coef(mod)[3])
#mintercept <- coef(mod)[1]/(-coef(mod)[3]) 

mslope <- 4.268732 / -4.340145
mintercept <- -40.591103 / -4.340145


ggplot(training, aes(x=exam1, y=exam2, shape=yesorno, color=yesorno)) +
  geom_point() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks=element_blank()) +
  geom_abline(intercept = mintercept, slope = mslope)


