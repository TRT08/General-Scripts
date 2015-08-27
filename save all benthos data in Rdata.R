#export all as csvs into a desktop folder

require(RODBC) || install.packages("RODBC") 

# Get benthos/scope data from Database
db <- "F:/DATA/SLBE/AVBOT Database.accdb"
con2 <- odbcConnectAccess2007(db)
#sqlTables(con2, tableType = "TABLE")$TABLE_NAME   #list table names
RawBenthosLog <- sqlFetch(con2, "Inventory Control Log - Benthos")
RawBenthosData <- sqlFetch(con2, "Compiled Benthic Analysis")
RawBenthosTaxa <- sqlFetch(con2, "Potential Benthos Taxon List")
Rawmussel.lengths <- sqlFetch(con2, "Mussel Lengths")
Rawscopes <- sqlFetch(con2, "Microscope Magnifications")
SerialLogBenthos <- sqlFetch(con2, "Serial Log - Benthos")
Video <- sqlFetch(con2, "Serial Log - Video")
BT <- sqlFetch(con2, "BT Environmental Data")
HOBO <- sqlFetch(con2, "Combined")
Coords<- sqlFetch(con2, "Site Coordinates - Fixed and Random")
Subst <- sqlFetch(con2, "Benthos Substrate Composition")
BenGIS <- sqlFetch(con2, "Benthos Buffer GIS Zonal Stats")
PCR <- sqlFetch(con2, "Updated PCR Results")
close(con2)

#Make database colnames appropriate for R to use
colnames(RawBenthosLog) <- make.names(colnames(RawBenthosLog), unique = TRUE)
colnames(RawBenthosData) <- make.names(colnames(RawBenthosData), unique = TRUE)
colnames(RawBenthosTaxa) <- make.names(colnames(RawBenthosTaxa), unique = TRUE)
colnames(Rawmussel.lengths) <- make.names(colnames(Rawmussel.lengths), unique = TRUE)
colnames(Rawscopes) <- make.names(colnames(Rawscopes), unique = TRUE)
colnames(SerialLogBenthos) <- make.names(colnames(SerialLogBenthos), unique = TRUE)
colnames(Video) <- make.names(colnames(Video), unique = TRUE)
colnames(BT) <- make.names(colnames(BT), unique = TRUE)
colnames(HOBO) <- make.names(colnames(HOBO), unique = TRUE)
colnames(Coords) <- make.names(colnames(Coords), unique = TRUE)
colnames(Subst) <- make.names(colnames(Subst), unique = TRUE)
colnames(BenGIS) <- make.names(colnames(BenGIS), unique = TRUE)
colnames(PCR) <- make.names(colnames(PCR), unique = TRUE)

mainDir <- "C:/Users/trtucker/Desktop"
subDir <- "BenData"
dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
setwd(file.path(mainDir, subDir))

save.image()
