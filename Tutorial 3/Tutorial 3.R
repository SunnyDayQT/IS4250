library(ggplot2)
library(scales)
data=read.csv("/Users/sunny/Desktop/Last sem/IS4250/Tutorial 2/labs.csv")

#Question (a)
sat=data[,12]
wait=data[,15]

#For satisfaction
kpi_s=percent(table(sat)[2]/length(sat))
kpi_u=percent(table(sat)[1]/length(sat))
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


#Question (b)
age_sat=data[,c(1,12)]
gender_sat=data[,c(6,12)]

#For gender&satisfaction
genderm_s=gender_sat[gender_sat$Gender == "M",]
genderf_s=gender_sat[gender_sat$Gender == "F",]
sat_m=table(genderm_s)[2,2]/nrow(genderm_s)
sat_f=table(genderf_s)[1,2]/nrow(genderf_s)

var=c("M","F")
rate=c(sat_m*100,sat_f*100)

bar_2=barplot(rate, names = var,
              xlab = "Gender", ylab = "Satisfaction Rate(%)",
              main = "Difference in service satisfaction for gender",col="pink")

text(x=bar_2,0,round(rate, 1), xpd=TRUE,cex=1,pos=3)

#For age&satisfaction
age_sat=age_sat[order(age_sat$AGE),]
age=vector()
satisfaction_rate=vector()
for (i in age_sat$AGE) {
  if (i%in%age==FALSE){
     age=c(age,i)
  }
}

for (j in age){
  temp=age_sat[age_sat$AGE==j,]
  sat_rate=nrow(temp[temp$SvcSat=="Y",])/nrow(temp)
  satisfaction_rate=c(satisfaction_rate,sat_rate)
}

df_2=data.frame(age,satisfaction_rate)
chart_1=ggplot(df_2,aes(age,satisfaction_rate,group=1))+geom_line()
print(chart_1)

#Question (c)
wait_sat=data[,c(15,12)]
wait_sat=wait_sat[order(wait_sat$WaitTime),]
wait_time=vector()
w_satisfaction_rate=vector()
for (i in wait_sat$WaitTime) {
  if (i%in%wait_time==FALSE){
    wait_time=c(wait_time,i)
  }
}

for (j in wait_time){
  temp=wait_sat[wait_sat$WaitTime==j,]
  sat_rate=nrow(temp[temp$SvcSat=="Y",])/nrow(temp)
  w_satisfaction_rate=c(w_satisfaction_rate,sat_rate)
}

df_3=data.frame(wait_time,w_satisfaction_rate)
chart_2=ggplot(df_3,aes(wait_time,w_satisfaction_rate,group=1))+geom_line()
print(chart_2)

#Question (d)
h_w=data[,c(8,16,13)]
BMI=h_w[,2]/(h_w[,1])^2
h_w[,"BMI"]=BMI

h_w=h_w[order(h_w$TC),]
tc=vector()
bmi=vector()
for (i in h_w$TC) {
  if (i%in%tc==FALSE){
    tc=c(tc,i)
  }
}

for (j in tc){
  temp=h_w[h_w$TC==j,]
  avg_bmi=sum(temp[,4])/nrow(temp)
  bmi=c(bmi,avg_bmi)
}

df_4=data.frame(tc,bmi)
chart_3=ggplot(df_4,aes(tc,bmi,group=1))+geom_line()
print(chart_3)

