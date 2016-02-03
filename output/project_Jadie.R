#Data Preparation 

install.packages("data.table")
install.packages("maps")
library(data.table)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(maps)
pusa <- fread("ss13pusa.csv",select=c("RAC2P","POBP","AGEP","CIT","COW","GCR","MAR","MARHT","SCHL","SEX","OC","PINCP","DECADE","ST","YOEP","INDP","WKHP"))
pusb <- fread("ss13pusb.csv",select=c("RAC2P","POBP","AGEP","CIT","COW","GCR","MAR","MARHT","SCHL","SEX","OC","PINCP","DECADE","ST","YOEP","INDP","WKHP"))
pus <- bind_rows(pusa,pusb)
rm(pusa,pusb)
gc()

# COW(Class of worker)
pus$COW <- factor(pus$COW)
levels(pus$COW) <- c("Private profit", "Private non-profit", "Local government", "State government", "Federal government", "Self-employed", "Self-employed", "Working without pay", "Unemployed")

# DECADE(Decade of entry)
pus$DECADE <- factor(pus$DECADE)
levels(pus$DECADE) <- c("~1950's", "1950's", "1960's", "1970's", "1980's", "1990's", "2000's~")

# ST(State Code)
pus$ST <- as.factor(pus$ST)
levels(pus$ST) <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut",
                    "Delaware", "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois",
                    "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts",
                    "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", 
                    "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota",
                    "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota",
                    "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia",
                    "Wisconsin", "Wyoming", "Puerto Rico")

# SEX
pus$SEX <- factor(pus$SEX)
levels(pus$SEX) <- c("Male", "Female")

# INDP(Industry recode for 2013 and later based on 2012 IND codes)
pus$INDP <- ifelse(pus$INDP >= 170 & pus$INDP <= 290, 170, pus$INDP)
pus$INDP <- ifelse(pus$INDP >= 370 & pus$INDP <= 490, 370, pus$INDP)
pus$INDP <- ifelse(pus$INDP >= 570 & pus$INDP <= 770, 570, pus$INDP)
pus$INDP <- ifelse(pus$INDP >= 1070 & pus$INDP <= 3990, 1070, pus$INDP)
pus$INDP <- ifelse(pus$INDP >= 4070 & pus$INDP <= 6390, 4070, pus$INDP)
pus$INDP <- ifelse(pus$INDP >= 6470 & pus$INDP <= 6780, 6470, pus$INDP)
pus$INDP <- ifelse(pus$INDP >= 6870 & pus$INDP <= 7190, 6870, pus$INDP)
pus$INDP <- ifelse(pus$INDP >= 7270 & pus$INDP <= 7790, 7270, pus$INDP)
pus$INDP <- ifelse(pus$INDP >= 7860 & pus$INDP <= 7890, 7860, pus$INDP)
pus$INDP <- ifelse(pus$INDP >= 7970 & pus$INDP <= 8290, 7970, pus$INDP)
pus$INDP <- ifelse(pus$INDP >= 8370 & pus$INDP <= 8470, 8370, pus$INDP)
pus$INDP <- ifelse(pus$INDP %in% c(8660, 8680, 8690), 8370, pus$INDP) 
pus$INDP <- ifelse(pus$INDP >= 8770 & pus$INDP <= 9290, 8370, pus$INDP)
pus$INDP <- ifelse(pus$INDP %in% c(8560, 8570, 8580, 8590, 8670), 8560, pus$INDP)
pus$INDP <- ifelse(pus$INDP >= 9370 & pus$INDP <= 9590, 9370, pus$INDP)
pus$INDP <- ifelse(pus$INDP >= 9670 & pus$INDP <= 9870, 9670, pus$INDP)
pus$INDP <- ifelse(pus$INDP >= 9920, 9920, pus$INDP)
pus$INDP <- factor(pus$INDP)
levels(pus$INDP) <- c("Agriculture, Forestry, Fishing, Hunting", "Mining", "Utilities, Construction", 
                      "Manufacturing", "Trade, Logistic", "Information, Communications", "Finance",
                      "Professional", "Education", "Health", "Other Services",
                      "Arts, Entertainment", "Public Administration", "Military", "Unemployed"
)

# SCHL(Educational attainment)
pus$SCHL <- ifelse(pus$SCHL <= 16, 16, pus$SCHL)
pus$SCHL <- ifelse(pus$SCHL >= 17 & pus$SCHL <= 19, 19, pus$SCHL)
pus$SCHL <- factor(pus$SCHL)
levels(pus$SCHL) <- c("High school or lower", "Some college", "Associate", "Bachelor", "Master", "Professional", "Doctorate")

chinese <- filter(pus,RAC2P==43|RAC2P==44|POBP==207|POBP==209|POBP==240)

ggplot(chinese, aes(AGEP, group=SEX)) + 
  geom_bar(aes(colour=SEX, fill=SEX), binwidth=1, alpha=0.9) +
  xlab("Age") + ylab("Count") + ggtitle("Chinese Population Age by SEX")

counts <- table(chinese$INDP)
barplot(counts,beside=FALSE,main="Industry Distribution", horiz=FALSE,
        col=rainbow(15),xlab="Count",ylab="Industry",names.arg = FALSE,
        legend = c("Agriculture", "Mining", "Utilities", 
                   "Manufacturing", "Trade, Logistic", "Information", "Finance",
                   "Professional", "Education", "Health", "Other",
                   "ArtsEntertainment", "Administration","Military", "Unemployed"))