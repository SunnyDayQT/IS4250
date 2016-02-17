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
cat("Yes, the differnce is statistically different.")

#Question(c)
cat("Cannot use chi-square test because it is used for the sample whose size is large")
