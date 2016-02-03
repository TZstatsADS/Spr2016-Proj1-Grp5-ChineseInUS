setwd("~/Downloads/csv_pus")
library(data.table)
library(dplyr)
col<-c("SCHL","ST","PINCP","MSP","OCCP","SCIENGP","WKHP","SEX","ESR","POBP","RAC2P","INDP")
pusa <- fread("~/Downloads/csv_pus/ss13pusa.csv",select = col)
pusb <- fread("~/Downloads/csv_pus/ss13pusb.csv",select = col)
pus <- rbind(pusa, pusb)
rm(pusa, pusb)
gc()
library(dplyr)
masterchinese.pus=
  pus%>%
  filter(SCHL==22)%>%
  filter(POBP==207|POBP==209|POBP==240|RAC2P==43|RAC2P==44)
attach(masterchinese.pus)
ST <- as.factor(ST)
MSP<-as.factor(MSP)
SCIENGP<-as.factor(SCIENGP)
SEX<-as.factor(SEX)
ESR<-as.factor(ESR)
levels(ST) <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut",
                    "Delaware", "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois",
                    "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts",
                    "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", 
                    "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota",
                    "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota",
                    "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia",
                    "Wisconsin", "Wyoming", "Puerto Rico")
levels(MSP)<-c("married&spouse present","married&spouse absent","Widowed","Divorced","Separated","Never married")
levels(SCIENGP)<-c("Sci","Non-sci")
levels(SEX)<-c("Male","Female")
levels(ESR)<-c("empd&work","empd not work","unempd","af&work","not in labor force")

#Salary 
Male<-subset(masterchinese.pus,masterchinese.pus$SEX=="1")
Female<-subset(masterchinese.pus,masterchinese.pus$SEX=="2")
meansal.m<-mean(Male$PINCP,na.rm = T)
meanwkh.m<-mean(Male$WKHP,na.rm = T)
meansal.f<-mean(Female$PINCP,na.rm = T)
meanwkh.f<-mean(Female$WKHP,na.rm = T)
par(mfrow=c(2,1))
plot(meanwkh.m,meansal.m,xlim = c(25,55),col=2,xlab = "mean.workinghour",ylab = "mean.salary")
points(meanwkh.f,meansal.f,xlim = c(25,55),col=4)
legend("topright",col = c(2,4),pch=1,legend = c("Male","Female"),bty="n",cex=0.75)

#Industry
slices.m<-c(sum(Male$SCIENGP==1),sum(Male$SCIENGP==2))
labels.m<-c("Scientific","Non-scientific")
labels.m <- paste(labels.m, " ", round(slices.m/sum(slices.m)*100,2), "%", sep="")
pie(slices.m,labels.m,main="Industry--Male",col =c("grey","white"))

slices.f<-c(sum(Female$SCIENGP==1),sum(Female$SCIENGP==2))
labels.f<-c("Scientific","Non-scientific")
labels.f <- paste(labels.f, " ", round(slices.f/sum(slices.f)*100,2), "%", sep="")
pie(slices.f,labels.f,main="Industry--Female",col =c("grey","white"))


#Salary vs Industry
salar.ind=
  masterchinese.pus%>%
  group_by(SCIENGP)%>%
  summarise(
    avgsalary=mean(PINCP,na.rm=T)
  )
salar.ind$SCIENGP<-as.factor(salar.ind$SCIENGP)
levels(salar.ind$SCIENGP)<-c("Sci","Non-sci")
plot(salar.ind$avgsalary~salar.ind$SCIENGP)

#Occupation
detach("package:plyr", unload=TRUE) 
library(dplyr)
Occupation=
  Female%>%
  filter(OCCP>0)%>%
  group_by(OCCP)%>%
  summarise(n=n())
  mutate(Occupation,Count=n)%>%
  arrange(desc(Count))
  
occp.sort<- Occupation[order(Occupation$n,decreasing = T),]
occp.sort<- as.data.frame(occp.sort)
occp.sort<- subset(occp.sort,occp.sort$n >30)
barplot(occp.sort$n,names.arg = occp.sort$OCCP,cex.names = 0.4)
legend("topright",legend = c("800:ACCOUNTANTS AND AUDITORS","2200:POSTSECONDARY TEACHERS","1020:SOFTWARE DEVELOPERS","2310:ELEMENTARY AND MIDDLE SCHOOL TEACHERS","430:MISCELLANEOUS MANAGERS","120:FINANCIAL MANAGERS","1010:COMPUTER PROGRAMMERS","710:MANAGEMENT ANALYSTS","1006:COMPUTER SYSTEMS ANALYSTS","5700:SECRETARIES AND ADMINISTRATIVE ASSISTANTS","3255:REGISTERED NURSES","1760:PHYSICAL SCIENTISTS, ALL OTHER"),bty="n",cex=0.4)
 