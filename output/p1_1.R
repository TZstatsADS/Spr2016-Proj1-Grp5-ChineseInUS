#import data
setwd("C:/Users/ouwen/Documents/R")

#import library
library(data.table)
library(dplyr)
library(ggplot2)
library(maps)
col<-c("SCHL","ST","PINCP","MSP","OCCP","SCIENGRLP","WKHP","SEX","ESR","POBP","RAC2P", "INDP")
pusa <- fread("ss13pusa.csv",select = col)
pusb <- fread("ss13pusb.csv",select = col)

#making data
pus <- rbind(pusa, pusb)
rm(pusa, pusb)
gc()


    chinese <-
        pus%>%
        filter(POBP==207|POBP==209|POBP==240|RAC2P==43|RAC2P==44)


#making necessary amendements        
chinese$ST <- as.factor(chinese$ST)
chinese$MSP <- as.factor(chinese$MSP)
chinese$SCIENGRLP <- as.factor(chinese$SCIENGRLP)
chinese$SEX <- as.factor(chinese$SEX)
chinese$ESR <- as.factor(chinese$ESR)
levels(chinese$ST) <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut",
                "Delaware", "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois",
                "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts",
                "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", 
                "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota",
                "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota",
                "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia",
                "Wisconsin", "Wyoming", "Puerto Rico")
levels(chinese$MSP)<-c("married&spouse present","married&spouse absent","Widowed","Divorced","Separated","Never married")
levels(chinese$SCIENGRLP)<-c("Sci","Non-sci")
levels(chinese$SEX)<-c("Male","Female")
levels(chinese$ESR)<-c("empd&work","empd not work","unempd","af&work","af with job but not work","not in labor force")

chinese$NDP <- ifelse(chinese$INDP >= 170 & chinese$INDP <= 290, 170, chinese$INDP)
chinese$INDP <- ifelse(chinese$INDP >= 370 & chinese$INDP <= 490, 370, chinese$INDP)
chinese$INDP <- ifelse(chinese$INDP >= 570 & chinese$INDP <= 770, 570, chinese$INDP)
chinese$INDP <- ifelse(chinese$INDP >= 1070 & chinese$INDP <= 3990, 1070, chinese$INDP)
chinese$INDP <- ifelse(chinese$INDP >= 4070 & chinese$INDP <= 6390, 4070, chinese$INDP)
chinese$INDP <- ifelse(chinese$INDP >= 6470 & chinese$INDP <= 6780, 6470, chinese$INDP)
chinese$INDP <- ifelse(chinese$INDP >= 6870 & chinese$INDP <= 7190, 6870, chinese$INDP)
chinese$INDP <- ifelse(chinese$INDP >= 7270 & chinese$INDP <= 7790, 7270, chinese$INDP)
chinese$INDP <- ifelse(chinese$INDP >= 7860 & chinese$INDP <= 7890, 7860, chinese$INDP)
chinese$INDP <- ifelse(chinese$INDP >= 7970 & chinese$INDP <= 8290, 7970, chinese$INDP)
chinese$INDP <- ifelse(chinese$INDP >= 8370 & chinese$INDP <= 8470, 8370, chinese$INDP)
chinese$INDP <- ifelse(chinese$INDP %in% c(8660, 8680, 8690), 8370, chinese$INDP) 
chinese$INDP <- ifelse(chinese$INDP >= 8770 & chinese$INDP <= 9290, 8370, chinese$INDP)
chinese$INDP <- ifelse(chinese$INDP %in% c(8560, 8570, 8580, 8590, 8670), 8560, chinese$INDP)
chinese$INDP <- ifelse(chinese$INDP >= 9370 & chinese$INDP <= 9590, 9370, chinese$INDP)
chinese$INDP <- ifelse(chinese$INDP >= 9670 & chinese$INDP <= 9870, 9670, chinese$INDP)
chinese$INDP <- ifelse(chinese$INDP >= 9920, 9920, chinese$INDP)
chinese$INDP <- factor(chinese$INDP)
levels(chinese$INDP) <- c("Agriculture, Forestry, Fishing, Hunting", "Mining", "Utilities, Construction", 
                          "Manufacturing", "Trade, Logistic", "Information, Communications", "Finance",
                          "Professional", "Education", "Health", "Other Services",
                          "Arts, Entertainment", "Public Administration", "Military", "Unemployed"
)


#### WORKING HOUR!!!!!!!!!!!
#### HERE, DON'T MISS!!!!!!!
#### YUCHEN, LOOK AT ME!!!!!
#prep
all_state <- map_data("state")
data <- as.data.frame(prop.table(table(chinese$ST)))
data$state <- c(sort(tolower(c("district of columbia", state.name))),tolower("Puerto Rico"))
all_state$freq <- data$Freq[match(all_state$region, data$state)]*100

#work hour of chinese masters
work_hour <- chinese %>%
        filter(is.na(WKHP) == F) %>%
        group_by(ST) %>%
        summarise(workhour = mean(WKHP))
state_47 = data[-c(52),]
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
income_level <- chinese %>%
        filter(is.na(PINCP) == F) %>% 
        group_by(ST) %>% 
        summarise(income = mean(PINCP))
state_49 = data[-c(52),]
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

