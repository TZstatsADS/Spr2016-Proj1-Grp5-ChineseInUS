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

########################
#we notice a salary difference between male and female.
#there are some factors which may result in this phenomenon.( we can exclude the education level factor given that the sample all have a master degree.)
#some possible factors we can pay attention to: working hour, working industry(sci or non-sci),gender discrimination,etc.
##########################

#working hour
#we want to see whether the difference of salary within gender is caused by different working hour
Male<-subset(masterchinese.pus,masterchinese.pus$SEX=="1")
Female<-subset(masterchinese.pus,masterchinese.pus$SEX=="2")
meansal.m<-mean(Male$PINCP,na.rm = T)
meanwkh.m<-mean(Male$WKHP,na.rm = T)
meansal.f<-mean(Female$PINCP,na.rm = T)
meanwkh.f<-mean(Female$WKHP,na.rm = T)

plot(meanwkh.m,meansal.m,xlim = c(25,55),col="red",pch=19,xlab = "mean.workinghour",ylab = "mean.salary")
points(meanwkh.f,meansal.f,pch=19,xlim = c(25,55),col="lightblue")
legend("topright",col = c("red","lightblue"),pch=19,legend = c("Male","Female"),bty="n",cex=0.75)
####
#we can see that the average working hour of males and females are very close, 
#which indicates the difference in salary is not caused by this factor
###





#Industry
salar.ind=
  masterchinese.pus%>%
  group_by(SCIENGP)%>%
  summarise(
    avgsalary=mean(PINCP,na.rm=T)
  )
salar.ind$SCIENGP<-as.factor(salar.ind$SCIENGP)
levels(salar.ind$SCIENGP)<-c("Sci","Non-sci")
plot(salar.ind$avgsalary~salar.ind$SCIENGP)

#Industrys-gender
slices.m<-c(sum(Male$SCIENGP==1),sum(Male$SCIENGP==2))
labels.m<-c("Scientific","Non-scientific")
labels.m <- paste(labels.m, " ", round(slices.m/sum(slices.m)*100,2), "%", sep="")
pie(slices.m,labels.m,main="Industry--Male",col =c("lightgreen","lightyellow"))

slices.f<-c(sum(Female$SCIENGP==1),sum(Female$SCIENGP==2))
labels.f<-c("Scientific","Non-scientific")
labels.f <- paste(labels.f, " ", round(slices.f/sum(slices.f)*100,2), "%", sep="")
pie(slices.f,labels.f,main="Industry--Female",col =c("lightgreen","lightyellow"))
##################
#we can see that scientific industry earns more than those non-scientific
#and also data shows that males are more likelily to be in a scientific industry than females
#so the industry difference might be accounted for the salary difference
#################################



#gender discrimination
saldif.sci=
  masterchinese.pus%>%
  filter(SCIENGP=="1")%>%
  group_by(SEX)%>%
  summarise(
    avgsalary=mean(PINCP,na.rm=T)
  )
saldif.sci$SEX<-as.factor(saldif.sci$SEX)
levels(saldif.sci$SEX)<-c("Male","Female")
saldif.nonsci=
  masterchinese.pus%>%
  filter(SCIENGP=="2")%>%
  group_by(SEX)%>%
  summarise(
    avgsalary=mean(PINCP,na.rm=T)
  )
saldif.nonsci$SEX<-as.factor(saldif.nonsci$SEX)
levels(saldif.nonsci$SEX)<-c("Male","Female")

par(mfrow=c(1,2))
plot(saldif.sci,ylim=c(50000,100000),main="sci",cex.axis=0.8,cex.lab=0.8)
plot(saldif.nonsci,ylim=c(50000,100000),main="non-sci",cex.axis=0.8,cex.lab=0.8)
###################
#As we can see, there do exist gender discrimination at some extent. 
#For that under the same education level, close working hour and the same industry, 
#there's still much difference in salary between males and females.
####################

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
barplot(occp.sort$n,names.arg = occp.sort$OCCP,col=terrain.colors(12),cex.names = 0.4)
legend("topright",legend = c("800:ACCOUNTANTS AND AUDITORS","2200:POSTSECONDARY TEACHERS","1020:SOFTWARE DEVELOPERS","2310:ELEMENTARY&MIDDLE TEACHERS","430:MISCELLANEOUS MANAGERS","120:FINANCIAL MANAGERS","1010:COMPUTER PROGRAMMERS","710:MANAGEMENT ANALYSTS","1006:COMPUTER SYSTEMS","5700:SECRETARIES$ASSISTANTS","3255:REGISTERED NURSES","1760:PHYSICAL SCIENTISTS, ALL OTHER"),bty="n",cex=0.7)
##################
#we're interested in the occupations that are most welcomed by chinese females
#as we can see, the occupation which attracts most females is accounting and finance related jobs, which comforms to our assumption.
###################