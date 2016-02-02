#import data
setwd("C:/Users/ouwen/Documents/R/Data Science")

#import library
library(data.table)
library(dplyr)
library(ggplot2)
library(maps)
col<-c("SCHL","ST","PINCP","MSP","OCCP","SCIENGRLP","WKHP","SEX","ESR","POBP","RAC2P")
pusa <- fread("ss13pusa.csv",select = col)
pusb <- fread("ss13pusb.csv",select = col)

#making data
pus <- rbind(pusa, pusb)
rm(pusa, pusb)
gc()


masterchinese.pus=
        pus%>%
        filter(SCHL==22)%>%
        filter(POBP==207|POBP==209|POBP==240)%>%
        filter(RAC2P==43|RAC2P==44)


#attach data and making necessary amendements        
attach(masterchinese.pus)
ST <- as.factor(ST)
MSP<-as.factor(MSP)
SCIENGRLP<-as.factor(SCIENGRLP)
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
levels(SCIENGRLP)<-c("Sci","Non-sci")
levels(SEX)<-c("Male","Female")
levels(ESR)<-c("empd&work","empd not work","unempd","af&work","af with job but not work","not in labor force")


#prep
all_state <- map_data("state")
data <- as.data.frame(prop.table(table(ST)))
data$state <- c(sort(tolower(c("district of columbia", state.name))),tolower("Puerto Rico"))
all_state$freq <- data$Freq[match(all_state$region, data$state)]*100

#work hour of chinese masters
work_hour <- masterchinese.pus %>%
        filter(is.na(WKHP) == F) %>%
        group_by(ST) %>%
        summarise(workhour = mean(WKHP))
state_47 = data[-c(2,23,30,38,52),]
state_47$workhour = work_hour$workhour
all_state$workhour = work_hour$workhour[match(all_state$region,state_47$state)]

p_work <- ggplot(all_state, aes(x=long, y=lat, group=group))+
        geom_polygon(aes(fill=workhour), color = "gray78") +
        scale_fill_gradient(name="working hour", low="white", high = "blue")
p_work <- p_work + theme(strip.background = element_blank(),
                         strip.text.x     = element_blank(),
                         axis.text.x      = element_blank(),
                         axis.text.y      = element_blank(),
                         axis.ticks       = element_blank(),
                         axis.line        = element_blank(),
                         panel.background = element_blank(),
                         panel.border     = element_blank(),
                         panel.grid       = element_blank(),
                         legend.position  = "right") +
        xlab("") + ylab("") + ggtitle("Avg. working time of Chinese by State")

p_work


#income level of chinese masters
income_level <- masterchinese.pus %>%
        filter(is.na(PINCP) == F) %>% 
        group_by(ST) %>% 
        summarise(income = mean(PINCP))
state_49 = data[-c(2,23,30,38,52),]
state_49$income = income_level$income
all_state$income = state_49$income[match(all_state$region,state_49$state)]


p_income <- ggplot(all_state, aes(x=long, y=lat, group=group)) + 
        geom_polygon(aes(fill=income), colour="gray78") + 
        scale_fill_gradient(name="Income", low="white", high="blue")
p_income <- p_income + theme(strip.background = element_blank(),
                   strip.text.x     = element_blank(),
                   axis.text.x      = element_blank(),
                   axis.text.y      = element_blank(),
                   axis.ticks       = element_blank(),
                   axis.line        = element_blank(),
                   panel.background = element_blank(),
                   panel.border     = element_blank(),
                   panel.grid       = element_blank(),
                   legend.position  = "right") +
        xlab("") + ylab("") + ggtitle("Avg. Income of Chinese with high degree by State")

p_income

#Income to wage ratio
ratio <- all_state$income/all_state$workhour
all_state <- cbind(all_state, ratio)
p_ratio <- ggplot(all_state, aes(x=long, y=lat, group=group)) + 
        geom_polygon(aes(fill=ratio), colour="gray78") + 
        scale_fill_gradient(name="Income", low="white", high="blue")
p_ratio <- p_ratio + theme(strip.background = element_blank(),
                        strip.text.x     = element_blank(),
                        axis.text.x      = element_blank(),
                        axis.text.y      = element_blank(),
                        axis.ticks       = element_blank(),
                        axis.line        = element_blank(),
                        panel.background = element_blank(),
                        panel.border     = element_blank(),
                        panel.grid       = element_blank(),
                        legend.position  = "right") +
        xlab("") + ylab("") + ggtitle("Ratio of Income to working hours")

p_ratio
