#Plot 1

chinese <- filter(pus,RAC2P==43|RAC2P==44|POBP==207|POBP==209|POBP==240)

ggplot(chinese, aes(AGEP, group=SEX)) + 
  geom_bar(aes(colour=SEX, fill=SEX), binwidth=1, alpha=0.9) +
  xlab("Age") + ylab("Count") + ggtitle("Chinese Population Age by SEX")




#Plot 2

ifmarried <- rep(0,dim(chinese)[1])
for (i in 1:dim(chinese)[1]){
  if (chinese$MAR[i]==1){
    ifmarried[i]= 1
  }
}
ggplot(chinese, aes(AGEP, group=ifmarried)) + 
  geom_bar(aes(colour=ifmarried, fill=ifmarried), binwidth=1, alpha=0.9) +
  xlab("Age") + ylab("Count") + ggtitle("Chinese Population Age by Marital Status")





# Plot 3

ggplot(chinese, aes(AGEP, group=MARHT)) + 
  geom_bar(aes(colour=MARHT, fill=MARHT), binwidth=1, alpha=0.9) +
  xlab("Age") + ylab("Count") + ggtitle("Chinese Population Age by Times of Marriage")






#Plot 4

counts <- table(chinese$INDP)
barplot(counts,beside=FALSE,main="Industry Distribution", horiz=FALSE,
        col=rainbow(15),xlab="Count",ylab="Industry",names.arg = FALSE,
        legend = c("Agriculture", "Mining", "Utilities", 
                   "Manufacturing", "Trade, Logistic", "Information", "Finance",
                   "Professional", "Education", "Health", "Other",
                   "ArtsEntertainment", "Administration","Military", "Unemployed"))