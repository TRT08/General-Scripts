
require(RODBC) || install.packages("RODBC") 

db <- "F:/DATA/SLBE/AVBOT Database.accdb"
con2 <- odbcConnectAccess2007(db)
tables <- sqlTables(con2, tableType = "TABLE")$TABLE_NAME   #list table names

tables <- factor(tables)
for (i in levels(tables)){
  print(i)
  print(sqlColumns(con2, i)$COLUMN_NAME)
}


close(con2)