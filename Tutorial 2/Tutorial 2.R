library(ggplot2)
data=read.csv("/Users/sunny/Desktop/Last sem/IS4250/Tutorial 2/labs.csv")

#Question 1
sat=data[,12]
wait=data[,15]
kpi_s=table(sat)[2]/length(sat)
kpi_u=table(sat)[1]/length(sat)

df_1=data.frame(Satisfaction=c("Yes","No"),Percentage=c(kpi_s,kpi_u))
pie_1=ggplot(df_1, aes(x="", y=Percentage, fill=Satisfaction))+
  geom_bar(width = 1, stat = "identity")+coord_polar("y", start=0)
print(pie_1)

kpi_w=sum(wait)/length(wait)

#Question 2
