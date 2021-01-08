library(dplyr)
library("cdlTools")
library("naniar")
library(ggplot2)
library(grid)
library("fastDummies")
library(tibble)
library(interactions) 
library(jtools) 


fill <- "#4271AE"
lines <- "#1F3552"

data1=read.csv("usa_00005.csv")
head(data1)  # 34291600 obs

#for ( i in 1: nrow(data1))  {
 # data1[i,2]=fips(data1[i,2],to='Name')
#}

#clean the data 
sum(is.na(result3)) # no missing values

data2=data1 %>% replace_with_na(replace = list(INCWAGE = c(999999,999998,0)))

#First, deal with the income

#data2 %>% group_by(YEAR, STATEFIP) %>% filter(STATEFIP==6) %>% summarize(max(INCWAGE)) # Show the max wage in CA in each year

result1= data2 %>% filter ( EMPSTAT == 1 ) %>% select(YEAR, STATEFIP, INCWAGE) %>% group_by(YEAR, STATEFIP) %>% summarise(mean_income=mean(INCWAGE,na.rm =TRUE),median_income=median(INCWAGE,na.rm =TRUE))

#Then, deal with unemployment rate

result2= data2 %>% filter( EMPSTAT == 1 | EMPSTAT == 2) %>% select (YEAR, STATEFIP, EMPSTAT) %>% group_by(YEAR, STATEFIP) %>% summarise(unemployment_rate = sum(EMPSTAT==2)/(sum(EMPSTAT==1)+sum(EMPSTAT==2)))


# Next, read other relevant variables
data3

# Then, deal with the mean house price

data3=read.csv("usa_00006.csv")
data4=data3 %>% replace_with_na(replace = list(VALUEH = c(9999999,9999998,0)))

result3= data4 %>% group_by(YEAR, STATEFIP) %>% summarise(mean_house_value=mean(VALUEH,na.rm=TRUE),median_house_value=median(VALUEH,na.rm=TRUE))
result3


new1=left_join(result1,result2,by=c("YEAR","STATEFIP"))
new2=left_join(new1,result3,by=c("YEAR","STATEFIP"))

new2[new2$mean_house_value==min(new2$mean_house_value),]

# Right-skewed data (The result shows that why I use the mean instead of using average)
d1 = data2 %>% filter ( EMPSTAT == 1 , INCWAGE>0) 
p1 <- ggplot(d1, aes(x = INCWAGE)) +
  geom_density(fill = fill, colour = lines,
               alpha = 0.6)+
labs(x = "pre-tax wage", title = "The density plot of wage for individuals in survey")
p1

p2 <- ggplot(data4, aes(x = VALUEH)) +
  geom_density(fill = fill, colour = lines,
               alpha = 0.6)+
  labs(x = "housing price", title = "The density plot of housing price for individuals in survey")
p2

#knitr::kable(new1)




# Add income tax information

source("income_tax_info.R")

low_tax=c(low_tax_rate_2008,low_tax_rate_2009,low_tax_rate_2010,low_tax_rate_2011,low_tax_rate_2012,low_tax_rate_2013,low_tax_rate_2014,low_tax_rate_2015,low_tax_rate_2016,low_tax_rate_2017,low_tax_rate_2018)

high_tax=c(high_tax_rate_2008,high_tax_rate_2009,high_tax_rate_2010,high_tax_rate_2011,high_tax_rate_2012,high_tax_rate_2013,high_tax_rate_2014,high_tax_rate_2015,high_tax_rate_2016,high_tax_rate_2017,high_tax_rate_2018)

new2[, "low_tax"]=low_tax
new2[, "high_tax"]=high_tax
new2


#Add age, ethnicity, education effect 

data5=read.csv("usa_00007.csv")#min(data5$AGE)
#table(data5$STATE,data5$YEAR)
result4=data5 %>% select(YEAR,STATEFIP,AGE) %>% group_by(YEAR,STATEFIP) %>% summarise(age16_30=sum(AGE<30 & AGE>=16)/sum(AGE>0),age31_60=sum(AGE<60 & AGE>=31)/sum(AGE>0),age60=sum(AGE>=60)/sum(AGE>0))

range(data5$EDUC)

result5=data5 %>% select(YEAR,STATEFIP,RACBLK,RACWHT) %>% group_by(YEAR,STATEFIP) %>% summarise(white=sum(RACWHT==2)/sum(RACWHT>0))

result6=data5 %>% select(YEAR,STATEFIP,EDUC) %>% group_by(YEAR,STATEFIP) %>% summarise(educ12=sum(EDUC>=7)/sum(EDUC<20))

new3=left_join(result4,result5,by=c("YEAR","STATEFIP"))
new4=left_join(new3,result6,by=c("YEAR","STATEFIP"))
new5=left_join(new2,new4,by=c("YEAR","STATEFIP"))  
colnames(new5)[2]="State"
# Add the fixed effects of states and year

new6= fastDummies::dummy_cols(new5,select_columns = c("YEAR","State"))

#write.csv(new6,'table1.csv')

# Add out-migration rate factor
source("Out_migration rate(or).R")
out_migration=c(or2008,or2009,or2010,or2011,or2012,or2013,or2014,or2015,or2016,or2017,or2018)
new7=add_column(new6,out_migration, .after="State")

# Crime rate data
source("crime_rate.R")
crime_rate=as.vector(crime_rate[,1])
crime_rate_all=c(crime_rate,c15,c16,c17,c18)
crime_rate_all=unlist(crime_rate_all)
crime_rate_all=as.numeric(crime_rate_all)
class(crime_rate_all)
new8=add_column(new7,crime_rate_all, .after="educ12")

quantile ( crime_rate_all )


# Add new migration rate

source("out_migration_group_revised.R")
out_migration_new=c(or2008,or2009,or2010,or2011,or2012,or2013,or2014,or2015,or2016,or2017,or2018)
new9=add_column(new8,out_migration_new, .after="State")


# Do the regression analysis

# Add IV
data6=read.csv("cps_00006.csv")
data7=data6 %>% replace_with_na(replace = list(METRO = c(0,9)))
data8=data7 %>% replace_with_na(replace = list(UHRSWORKLY = 999))
data9=data8 %>% replace_with_na(replace = list(DIFFMOB = 0))
new10= data9 %>% group_by(YEAR,STATEFIP) %>% summarise(metro=mean(METRO== 3,na.rm=TRUE), hp = mean(DIFFMOB == 2,na.rm=TRUE), work=mean(UHRSWORKLY< 10,na.rm=TRUE))  %>% select(YEAR,STATEFIP,hp,metro,work) %>% rename(State=STATEFIP)

new10[1:51,"hp"]=new10[52:102,"hp"]
new11=left_join(new9,new10)

sd(new11$crime_rate_all)

# With only main effects not fixed effects
colnames(new11)
model1=lm(data=new11[,c(3,5,7,8,10,11,80,81,82)],out_migration_new*100 ~ . )
summary(model1)

# After adding the fixed effects
model2=lm(data=new11[,c(3,5,7,8,10,11,18:82)],out_migration_new ~ . )
summary(model2)

# After adding all the factors 
colnames(new11)
model3=lm(data=new11[c(-9,-60),c(3,5,7,8,10,11,12:82)],out_migration_new ~ crime_rate_all*mean_house_value+.)
summary(model3)



# Draw the interaction plots

interact_plot(model3, pred = crime_rate_all, modx = mean_house_value, x.label = "housing price",y.label = "out-migration rate", legend.main = "crime rate", colors=c("red","green"))



# Delete outlier points and see what will happen
#dl1=new9
#dl1=dl1[c(-2,-9,-60),]
#model_dl1=lm(data=dl1[,c(3,5,7,8,10:79)],dl1$out_migration_new ~ #dl1$crime_rate_all*dl1$mean_house_value + . )
#summary(model_dl1)
#anova(model2)

# Do the regression with median level instead
colnames(new11)
model4=lm(data=new11[c(-9,-60),c(3,6,7,9,10:82)],out_migration_new ~ crime_rate_all*median_house_value + . )
summary(model4)
#anova(model3,model4)
m1=predict(model3)
m2=predict(model4)
hist(new11[c(-9,-60),]$out_migration_new,probability = TRUE)
plot(density(new11[c(-9,-60),]$out_migration_new),main="Density plot of mean wage/housing price, median wage/housing price")
lines(density(m1),col="red",lwd = 1,lty = 2)
lines(density(m2),col="blue",lwd = 1,lty = 1)
legend("topright", legend = c("Median", "Mean"),
       col = c("red", "blue"), lty = 2:1, cex = 0.5)

# Compare the relationship betweeen income differentials and out-migration rate

io1= new8 %>% select (YEAR, out_migration, mean_income) %>% group_by (YEAR) %>% summarise (sum_migration = mean(out_migration),income_diff=max(mean_income)-min(mean_income)) %>% mutate(year=as.character(YEAR), sum_migration10=sum_migration*100)
plot(io1$year,io1$sum_migration10,ylim=c(1, 30))
lines(x, y2, pch = 18, col = "blue", type = "b", lty = 2)

# Add CPI to income and national unemployment rate 
cpi=c(1.1669,1.171,1.1521,1.1169,1.0942,1.0784,1.0612,1.06,1.0468,1.0249)
nur=c(0.058,0.093,0.096,0,089,0.081,0.074,0.062,0.053,0.049,0.044,0.039)

# sample statistics

mean(new11$work)
range(new11$work)

sd(new11$crime_rate_all)

# Prediciton
source("out_migration_group_revised.R")
out_migration_new=c(or2008,or2009,or2010,or2011,or2012,or2013,or2014,or2015,or2016,or2017,or2018)
migration=out_migration_new*100
migration
class(migration)
year=rep(2008:2018,each=51)
year=as.character(year)
data1=data.frame(year,migration)
mg=data1%>%group_by(year)%>%summarise(sgd=mean(migration))
mg=mg[,2]
mg=unlist(mg)
mgnew=mg[3:11]
library(lmtest)
GDPGR_level <- as.numeric(mgnew)
GDPGR_lags <- as.numeric(GDPGRSub[-N])
GDPGR_AR2 <- dynlm(ts(GDPGR_level) ~ L(ts(GDPGR_level)) + L(ts(GDPGR_level), 2))

coeftest(GDPGR_AR2)
N=length(mgnew)
forecast <- c("2013:Q1" = coef(GDPGR_AR2) %*% c(1, GDPGR_level[N-1], GDPGR_level[N-2]))

all=c(2.918342,2.913235,2.911159,2.906159,2.903159)
year=c(2019:2023)
plot(year,all,ylim=c(1,5))
data22=data.frame(year,all)
data22 %>%
  ggplot( aes(x=year, y=all)) +
  geom_line() +
  geom_point()+
  labs(y="out-migration rate",x="Year")+
  ylim(2.5,3)




d1=data.frame(new2$YEAR,out_migration_new)
d2=d1%>%group_by(new2.YEAR)%>%summarize(ous=mean(out_migration_new)) %>% mutate(new2.YEAR=as.factor(new2.YEAR))

ous
ous[1:3]=c(0.026969279,0.026468078,0.02866918)
Actual_Migration_rate=ous
Predicted_Migration_rate=c(2.61,2.59,2.89,2.97,2.95,3.12,3.02,3.04,2.98,2.96,2.9)
year=c(2008:2018)
year=as.factor(year)

data23=data.frame(year,Actual_Migration_rate,Predicted_Migration_rate)
colors <- c("Actual Migration Rate" = "black", "Predicted Migration Rate" = "red")

data23 %>%
  ggplot( aes(x=year,group=1)) +
  geom_line(aes(y=ous*100,color="Actual Migration Rate")) +
  geom_point(aes(y=ous*100,color="Actual Migration Rate"))+
  geom_line(aes(y=Predicted_Migration_rate,color="Predicted Migration Rate")) +
  geom_point(aes(y=Predicted_Migration_rate,color="Predicted Migration Rate"))+
  labs(y="out-migration rate",x="Year",color=" ")+ scale_color_manual(values = colors)

d3=new2%>%group_by(YEAR)%>%summarize(HP=mean(median_house_value),UN=mean(unemployment_rate), IC=mean(median_income))


