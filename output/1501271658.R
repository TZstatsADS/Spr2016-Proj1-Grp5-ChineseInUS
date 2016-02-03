setwd("F:/CU Textbooks And Related Stuff/STAT W4249/Project1/pums")

install.packages("ggplot2")
install.packages("colorspace")
install.packages("maps")
library(data.table)
library(dplyr)
library(colorspace)
library(ggplot2)
library(gridExtra)
library(maps)
library(RColorBrewer)
col<-c("SCHL","AGEP","FAGEP","ST","PINCP","MSP","OCCP","SCIENGRLP","WKHP","SEX","ESR","POBP","RAC2P")
pusa <- fread("ss13pusa.csv",select = col)
pusb <- fread("ss13pusb.csv",select = col)
pus <- rbind(pusa, pusb)
rm(pusa, pusb)
gc()


#Data Manipulation
chinese <- pus%>%
    filter(RAC2P==43|RAC2P==44)%>%
    filter(POBP==207|POBP==209|POBP==240)
save(chinese, file="chinese_selected_cols.csv")
#Add Indicator MSPG 
chinese$MSPG <- ifelse(chinese$MSP == 1||chinese$MSP == 2, 1, 0)
chinese$MSP <- factor(chinese$MSP)
levels(chinese$MSP) <- c("Now married, spouse present", "Now married, spouse absent","Widowed", "Divorced", "Separated", "Never married")
#Add Indicator ESRG 
chinese$ESR <- factor(chinese$ESR)
levels(chinese$ESR) <- c("Employed", "Employed, not at work", "Unemployed", "Employed", "Employed, not at work", "Not in labor force")
chinese$ESRG <- ifelse(chinese$ESR == "Employed", 1, 0)

chinese$SCHL <- ifelse(chinese$SCHL <= 16, 16, chinese$SCHL)
chinese$SCHL <- ifelse(chinese$SCHL >= 17 & chinese$SCHL <= 19, 19, chinese$SCHL)
chinese$SCHL <- factor(chinese$SCHL)
levels(chinese$SCHL) <- c("High school or lower", "Some college", "Associate's degree", "Bachelor's degree", "Master's degree", "Professional degree", "Doctorate degree")

chinese$SEX <- factor(chinese$SEX)
levels(chinese$SEX) <- c("Male", "Female")


chinese$ST <- factor(chinese$ST)
levels(chinese$ST) <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut",
                "Delaware", "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois",
                "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts",
                "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", 
                "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota",
                "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota",
                "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia",
                "Wisconsin", "Wyoming", "Puerto Rico")

chinese$OCCP <- ifelse(chinese$OCCP >= 10 & chinese$OCCP <= 430, 10, chinese$OCCP)
chinese$OCCP <- ifelse(chinese$OCCP >= 500 & chinese$OCCP <= 740, 500, chinese$OCCP)
chinese$OCCP <- ifelse(chinese$OCCP >= 800 & chinese$OCCP <= 950, 800, chinese$OCCP)
chinese$OCCP <- ifelse(chinese$OCCP >= 1005 & chinese$OCCP <= 1240, 1005, chinese$OCCP)
chinese$OCCP <- ifelse(chinese$OCCP >= 1300 & chinese$OCCP <= 1560, 1300, chinese$OCCP)
chinese$OCCP <- ifelse(chinese$OCCP >= 1600 & chinese$OCCP <= 1965, 1600, chinese$OCCP)
chinese$OCCP <- ifelse(chinese$OCCP >= 2000 & chinese$OCCP <= 2060, 2000, chinese$OCCP)
chinese$OCCP <- ifelse(chinese$OCCP >= 2100 & chinese$OCCP <= 2160, 2100, chinese$OCCP)
chinese$OCCP <- ifelse(chinese$OCCP >= 2200 & chinese$OCCP <= 2550, 2200, chinese$OCCP)
chinese$OCCP <- ifelse(chinese$OCCP >= 2600 & chinese$OCCP <= 2920, 2600, chinese$OCCP)
chinese$OCCP <- ifelse(chinese$OCCP >= 3000 & chinese$OCCP <= 3540, 3000, chinese$OCCP)
chinese$OCCP <- ifelse(chinese$OCCP >= 3600 & chinese$OCCP <= 3655, 3600, chinese$OCCP)
chinese$OCCP <- ifelse(chinese$OCCP >= 3700 & chinese$OCCP <= 3955, 3700, chinese$OCCP)
chinese$OCCP <- ifelse(chinese$OCCP >= 4000 & chinese$OCCP <= 4150, 4000, chinese$OCCP)
chinese$OCCP <- ifelse(chinese$OCCP >= 4210 & chinese$OCCP <= 4250, 4210, chinese$OCCP)
chinese$OCCP <- ifelse(chinese$OCCP >= 4300 & chinese$OCCP <= 4650, 4300, chinese$OCCP)
chinese$OCCP <- ifelse(chinese$OCCP >= 4700 & chinese$OCCP <= 4965, 4700, chinese$OCCP)
chinese$OCCP <- ifelse(chinese$OCCP >= 5000 & chinese$OCCP <= 5940, 5000, chinese$OCCP)
chinese$OCCP <- ifelse(chinese$OCCP >= 6005 & chinese$OCCP <= 6130, 6005, chinese$OCCP)
chinese$OCCP <- ifelse(chinese$OCCP >= 6200 & chinese$OCCP <= 6765, 6200, chinese$OCCP)
chinese$OCCP <- ifelse(chinese$OCCP >= 6800 & chinese$OCCP <= 6940, 6800, chinese$OCCP)
chinese$OCCP <- ifelse(chinese$OCCP >= 7000 & chinese$OCCP <= 7630, 7000, chinese$OCCP)
chinese$OCCP <- ifelse(chinese$OCCP >= 7700 & chinese$OCCP <= 8965, 7700, chinese$OCCP)
chinese$OCCP <- ifelse(chinese$OCCP >= 9000 & chinese$OCCP <= 9750, 9000, chinese$OCCP)
chinese$OCCP <- ifelse(chinese$OCCP >= 9800 & chinese$OCCP <= 9830, 9800, chinese$OCCP)
chinese$OCCP <- factor(chinese$OCCP)

levels(chinese$OCCP) <- c("Null1", "MGR", "BUS","FIN", "CMM", "ENG","SCI","CMS","LGL","EDU","ENT","MED","HLS","PRT","EAT","CLN","PRS","SAL","OFF","FFF","CON","EXT","RPR","PRD","TRN","MIL","Null2")


#Basic glimpse of occupation fields among Chinese
data <- as.data.frame(prop.table(table(chinese$AGEP, chinese$OCCP)))
data$margin <- prop.table(table(chinese$AGEP))
data$height <- data$Freq/data$margin
data$center <- c(0, cumsum(data$margin)[1:length(levels(factor(chinese$AGEP))) -1]) + data$margin/2
ggplot(data, aes(center, height)) + 
    geom_bar(stat = "identity", aes(width = margin, fill = Var2), col = "gray", alpha = 0.7) +
    labs(x = "Age", y = "Frequency", title = "Field of Work of Chinese") +
    scale_x_continuous(breaks = seq(0, 1, 0.1), labels=c("16", "27", "31", "33", "36", "38", "41", "43", "46", "50", "93"))

#Narrow down above to occupation field of our interest
prime_OCCP.chinese<-chinese%>%
    na.omit()%>%
    filter(OCCP %in%  c("Null1","OFF","PRT","FIN","SAL","SCI","CMM","MGR"))
data <- as.data.frame(prop.table(table(prime_OCCP.chinese$AGEP, prime_OCCP.chinese$OCCP)))
data$margin <- prop.table(table(prime_OCCP.chinese$AGEP))
data$height <- data$Freq/data$margin
data$center <- c(0, cumsum(data$margin)[1:length(levels(factor(prime_OCCP.chinese$AGEP))) -1]) + data$margin/2


#Accentuate SCI field occupation of above
color <- scales::alpha(c(brewer.pal(n = 9, name = "Pastel1"), brewer.pal(n = 8, name = "Pastel2"), brewer.pal(n = 10, name = "Set3")),0.5)
color[6:7] <- c("orange", "firebrick1")
ggplot(data, aes(center, height)) + 
    geom_bar(stat = "identity", aes(width = margin, fill = Var2), col = "gray", alpha = 0.7) +
    scale_fill_manual(values = color)+
    labs(x = "Age", y = "Frequency", title = "Particular Fields of Work of Chinese") +
    scale_x_continuous(breaks = seq(0, 1, 0.1), labels=c("16", "27", "31", "33", "36", "38", "41", "43", "46", "50", "93"))


#Narrow down Chinese data to Chinese with master or higher degree
master_or_higher.chinese<-chinese%>%
    filter(SCHL %in% c("Master's degree", "Professional degree", "Doctorate degree"))

master_or_higher.chinese <- subset(chinese, SCHL== "Master's degree" |  SCHL=="Professional degree" |SCHL=="Doctorate degree")


#Narrow down above to be also with occupation in fields of interest
prime_OCCP_master_or_higher.chinese<-master_or_higher.chinese%>%
    filter(OCCP %in%  c("Null2","FIN","SAL","SCI","CMM","MGR","BUS","MED"))

prime_OCCP_master_or_higher.chinese<- subset(master_or_higher.chinese, OCCP=="Null2" | OCCP=="FIN" | OCCP=="SAL" | OCCP=="SCI" | OCCP=="CMM" | OCCP=="MGR" | OCCP=="BUS" | OCCP=="MED")


#Boxplot of working hours per week VS Chinese with higher education level in particular fields
ggplot(prime_OCCP_master_or_higher.chinese , aes(x=OCCP, y=WKHP)) + 
    geom_boxplot(aes(fill=OCCP), size=0.3) +
    stat_summary(fun.y=mean, geom="point", shape=23, size=3) + 
    ggtitle("Working hours per week")

#Marital status among Chinese with higher education level in particular fields
data <- as.data.frame(prop.table(table(prime_OCCP_master_or_higher.chinese$OCCP, prime_OCCP_master_or_higher.chinese$MSP),margin=1))
ggplot(data, aes(x = Var1, y = Freq, group = Var2)) + 
    geom_bar(stat = "identity", aes(colour = Var2, fill = Var2), alpha = 0.3) + 
    labs(x = "Occupation", y = "Frequency", title = "Marital Status vs Occupation")

#Sex distribution among Chinese with higher education level in particular fields
data <- as.data.frame(prop.table(table(prime_OCCP_master_or_higher.chinese$OCCP, prime_OCCP_master_or_higher.chinese$SEX),margin=1))
color <- scales::alpha(brewer.pal(n = 3, name = "Set2"))
ggplot(data, aes(x = Var1, y = Freq, group = Var2)) + 
    geom_bar(stat = "identity", aes(colour = Var2, fill = Var2), alpha = 0.3) + 
    scale_fill_manual(values = color)+
    labs(x = "Occupation", y = "Frequency", title = "SEX vs Occupation")

#Occupation distribution in paticular fields among Chinese with higher education level in different states
data <- as.data.frame(prop.table(table(prime_OCCP_master_or_higher.chinese$ST, prime_OCCP_master_or_higher.chinese$OCCP),margin=1))
ggplot(data, aes(x = Var1, y = Freq, group = Var2)) + 
    geom_bar(stat = "identity", aes(colour = Var2, fill = Var2), alpha = 0.3) + 
    labs(x = "Occupation", y = "Frequency", title = "Occupation vs ST")

#States distribution among Chinese with higher education level in paticular fields
data <- as.data.frame(prop.table(table(prime_OCCP_master_or_higher.chinese$OCCP, prime_OCCP_master_or_higher.chinese$ST),margin=1))
ggplot(data, aes(x = Var1, y = Freq, group = Var2)) + 
    geom_bar(stat = "identity", aes(colour = Var2, fill = Var2), alpha = 0.3) + 
    labs(x = "ST", y = "Frequency", title = "ST vs Occupation")




#A glimpse of income difference between 2 gender among different occupations
prime_OCCP_master_or_higher.chinese<-prime_OCCP_master_or_higher.chinese%>%
    na.omit()
IncomeOccpMedianMen <- tapply(prime_OCCP_master_or_higher.chinese[prime_OCCP_master_or_higher.chinese$SEX=="Male",]$PINCP, prime_OCCP_master_or_higher.chinese[prime_OCCP_master_or_higher.chinese$SEX=="Male",]$OCCP, median)
IncomeOccpMedianWomen <- tapply(prime_OCCP_master_or_higher.chinese[prime_OCCP_master_or_higher.chinese$SEX=="Female",]$PINCP, prime_OCCP_master_or_higher.chinese[prime_OCCP_master_or_higher.chinese$SEX=="Female",]$OCCP, median)

stripchart(log10(prime_OCCP_master_or_higher.chinese$PINCP) ~prime_OCCP_master_or_higher.chinese$OCCP, col=colorRampPalette(c('cadetblue1','gold'))(6),main = 'Income vs Occupation with regard to Sex', 
           col.main='darkred', pch=19, cex=0.1, method='jitter', jitter=0.4, vertical=T,  las=2, 
           ylab='Income -log10 scale-', cex.axis=0.60)

points(log10(IncomeOccpMedianMen)  ~ seq(1,27), bg='darkslateblue', pch=21, cex=1.2)
points(log10(IncomeOccpMedianWomen)  ~ seq(1,27), bg='forestgreen', pch=21, cex=1.2)
lines(log10(IncomeOccpMedianMen)  ~ seq(1,27), col='darkslateblue', lwd=1.4)
lines(log10(IncomeOccpMedianWomen)  ~ seq(1,27),col='forestgreen', lwd=1.4)
text(log10(IncomeOccpMedianWomen)-0.1  ~ seq(1,27), label=IncomeOccpMedianWomen)
text(log10(IncomeOccpMedianMen)+0.1  ~ seq(1,27), label=IncomeOccpMedianMen)

text(rep(log10(1200), 27)  ~ seq(1,27), 
     label= paste(round(IncomeOccpMedianWomen/IncomeOccpMedianMen*100, 1), '%', sep=''), cex=0.9)

abline(h=seq(3,6,by=0.5), col='grey', lty=2)
legend("topright", legend=c("Median Male", "Median Female"), 
       fill=c("darkslateblue", "forestgreen"), cex=0.9, bg="white")

