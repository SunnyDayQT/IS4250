library(haven)
library(Hmisc)
data5=read_sav("/Users/jim/Downloads/country122.sav")
model = lm(lifeexpf~birthrat,data=data5)

lifeexpf = data5[,c(6)]
birthrat = data5[,c(7)]
dataNew = data5[,c(6,4,3,15,16,13)]

plot(birthrat,lifeexpf)
abline(model)

strong = cor.test(lifeexpf,birthrat)
print(strong)

case = data.frame(birthrat=25)
prediction = predict(model,newdata= case)
print("Expected female life expectancy:")
print(prediction)

print("Linear regression makes several key assumptions:1.Linear relationship 2.Multivariate normality 3.No or little multicollinearity 4.No auto-correlation 5.Homoscedasticity")

model2 = lm(lifeexpf ~ gdp+urban+hospbed+docs+radio, data = data5)

sig = rcorr(as.matrix(dataNew))
print(sig)