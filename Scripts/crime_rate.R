# Crime rate
# 
# # Crime rate data
# 
# 2018
library(data.table)
crime=fread("CrimeStatebyState.csv")
crime_rate=crime[,3]
names(crime_rate)="V1"
crime_rate =crime_rate  %>% filter (crime_rate >0, V1 != "Violent Crime rate")
c18=read_excel("2018cm.xlsx", sheet=1, range = "C5:E510",col_names = FALSE)
names(c18)=c("V1","V2","V3")
c18=c18 %>% filter(V1 == "Rate per 100,000 inhabitants")
c18=c18[-40,]
c18=c18$V3
length(c18)

#2017

c17=read_excel("2017cm.xlsx", sheet=1, range = "C5:E510",col_names = FALSE)
names(c17)=c("V1","V2","V3")
c17=c17 %>% filter(V1 == "Rate per 100,000 inhabitants")
c17=c17[-40,]
c17=c17$V3
length(c17)

#2016

c16=read_excel("2016cm.xlsx", sheet=1, range = "C5:E510",col_names = FALSE)
names(c16)=c("V1","V2","V3")
c16=c16 %>% filter(V1 == "Rate per 100,000 inhabitants")
c16=c16[-40,]
c16=c16$V3
length(c16)

#2015
#
c15=read_excel("2015cm.xlsx", sheet=1, range = "C5:E510",col_names = FALSE)
names(c15)=c("V1","V2","V3")
c15=c15 %>% filter(V1 == "Rate per 100,000 inhabitants")
c15=c15[-40,]
c15=c15$V3
length(c15)
