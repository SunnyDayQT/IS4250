library(ggplot2)
data=read.csv("/Users/sunny/Desktop/Last sem/IS4250/Tutorial 2/labs.csv")

#Question 1
sat=data[,12]
wait=data[,15]

#For satisfaction
kpi_s=table(sat)[2]/length(sat)
kpi_u=table(sat)[1]/length(sat)
df_1=data.frame(Satisfaction=c("Yes","No"),Percentage=c(kpi_s,kpi_u))
pie_1=ggplot(df_1, aes(x="", y=Percentage, fill=Satisfaction))+
  geom_bar(width = 1, stat = "identity")+coord_polar("y", start=0)
print(pie_1)

#For waiting time
w_mean=mean(wait)
w_max=max(wait)
w_min=min(wait)
w_median=median.default(wait)
variable=c("Mean","Median","Max","Min")
wait_value=c(w_mean,w_median,w_max,w_min)
  
bar_1=barplot(wait_value, names = variable,
        xlab = "Statistics", ylab = "Waiting Time / minute(s)",
        main = "Statistics for Waiting Time",col="pink")

text(x=bar_1,0,round(wait_value, 1), xpd=TRUE,cex=1,pos=3)


#Question 2
age_sat=data[,c(1,12)]
gender_sat=data[,c(6,12)]

#For gender&satisfaction
genderm_s=gender_sat[gender_sat$Gender == "M",]
genderf_s=gender_sat[gender_sat$Gender == "F",]
sat_m=table(genderm_s)[2,2]/nrow(genderm_s)
sat_f=table(genderf_s)[1,2]/nrow(genderf_s)

var=c("M","F")
rate=c(sat_m,sat_f)

bar_1=barplot(rate, names = var,
              xlab = "Gender", ylab = "Satisfaction Rate(%)",
              main = "Difference in service satisfaction for gender",col="pink")

text(x=bar_1,0,round(rate, 1), xpd=TRUE,cex=1,pos=3)


