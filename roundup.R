####################################
####GETTING STARTED - RUN FIRST ####
####################################

setwd("F:/DATA/SLBE/R scripts/Round up all data/")

library(reshape)
library(plyr)
library(RODBC)
library(splitstackshape)
library(data.table)
library(car)
library(doBy)

options(scipen=999) #Keeps scientific notation away

# Get benthos/scope data from Database
db <- "F:/DATA/SLBE/AVBOT Database.accdb"
con2 <- odbcConnectAccess2007(db)
sqlTables(con2, tableType = "TABLE")$TABLE_NAME   #list table names
RawBenthosLog <- sqlFetch(con2, "Inventory Control Log - Benthos")
PCR <- sqlFetch(con2, "PCR Results")
MasterSER <- sqlFetch(con2, "Serial Log")
FISH <- sqlFetch(con2, "Inventory Control Log - Fish")
close(con2)

colnames(RawBenthosLog) <- make.names(colnames(RawBenthosLog), unique = TRUE)
colnames(PCR ) <- make.names(colnames(PCR ), unique = TRUE)
colnames(MasterSER) <- make.names(colnames(MasterSER), unique = TRUE)
colnames(FISH) <- make.names(colnames(FISH), unique = TRUE)

Pri<- read.csv("PriorityBenthos.csv")
Pri$Date <- as.Date(Pri$Date)

############
ROUND UP BENTHOS WITH PCR RESULTS


SER<- MasterSER[ , c("YearSER","SampleGroup")]

RawBenthosLog<- join(RawBenthosLog,SER,type = "left", match = "all")
PCR<- join(PCR,SER,type = "left", match = "all")
FISH<- join(FISH,SER,type = "left", match = "all")
Pri<- join(Pri,SER,type = "left", match = "all")

write.csv(PCR,"PCR.csv")
write.csv(RawBenthosLog,"RawBenthosLog.csv")
write.csv(FISH,"FISH.csv")
write.csv(Pri,"Pri.csv")


###############################################################################

names(PCR2)[names(PCR2)=="YearSER"] <- "PCR.YearSER"

DT <- data.table(Pri, key = c("Site","Date"))
tm <- data.table(PCR2, key = key(DT))
test <- tm[DT, roll='nearest', allow.cartesian=TRUE]
test <- test[!duplicated(test$YearSER),] 
RawBenthosLog <- data.frame(test)

##Add the date of the VideoSER to get difference in videograb
Videodateyear <- Video[,c("Date","YearSER")]
names(Videodateyear)[names(Videodateyear)=="Date"] <- "Video.Date"
names(Videodateyear)[names(Videodateyear)=="YearSER"] <- "Video.YearSER"
Videodateyear<- unique(Videodateyear)
test2 <- join(RawBenthosLog, Videodateyear, by ="Video.YearSER", type = "left", match = "all")
withVideo <- data.frame(test2)
withVideo$Video.Time.Diff.Days<- abs(difftime(withVideo$Video.Date, withVideo$Date, units="days"))