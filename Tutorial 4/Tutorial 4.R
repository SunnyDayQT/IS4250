library(dplyr)
library(ggplot2)

#Question(a)
g_rate = 3/10
b_rate = 4/6
differ = g_rate - b_rate
cat("Yes and the difference is",abs(differ))

#Question(b)
testor = rbind(c(3,7),c(4,2))
fisher.test(testor)
cat("No, the differnce is not statistically different since p is not statistical significant, do not reject the Null Hypothesis.")

#Question(c)
cat("Cannot use chi-square test because it is used for the sample whose size is large")
