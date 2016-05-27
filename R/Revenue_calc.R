#### This script analyzes raw revenue/collections data from Great Plains to  
#### give a point-in-time sum of revenues per reported revenue stream

#### Load required packages
library(readxl)
library(plyr)
library(dplyr)
library(lubridate)

### Loading data

#### Read in Revenue datasets
#hist_rev<-read_excel("O:/Projects/BottomLineStat/Query Files/GP Revenue 2011-2015.xlsx",1)
rev<-read_excel("O:/Projects/BottomLineStat/Query Files/GP 2016.xlsx",1)
#rev<-rbind(hist_rev,rev)

#### Read codebook
rev_code<-select(read_excel("O:/Projects/BottomLineStat/Query Files/Revenue Codebook.xlsx",1,skip=1),
                 Code=GP_code,Category)



### Data cleaning

#### Re-name Great Plains column "(c)Object(4)" to "Code"
names(rev)[names(rev)=="(c)Object(4)"] <- "Code"
names(rev)[names(rev)=="Credit Amount"] <- "Credit"
names(rev)[names(rev)=="Debit Amount"] <- "Debit"
names(rev)[names(rev)=="TRX Date"] <- "TRX_Date"

#### Merge codebook with raw dataset to appropriately categorize transactions
rev<-inner_join(rev,rev_code,by="Code")

#### Calculate net cash (collections) per transaction
rev$Net<-rev$Credit - rev$Debit

#### Create month variable
rev$Month <- strftime(rev$TRX_Date, format="%Y-%m")

### Summarize revenue by category, by month
rev_sum<-ddply(rev, .(Month,Category), summarize, sum_Net=sum(Net))

### Input month you want revenues for using "yyyy-mm" format
col_month<-readline("Enter reporting period: ")

####
print(rev_sum[rev_sum$Month==col_month,])

#### Generate CSVs
write.csv(rev[rev$Category=="Property Tax",],"O:/Projects/BottomLineStat/Output/Property Tax Collections.csv")
write.csv(rev_sum[rev_sum$Month==col_month,],"O:/Projects/BottomLineSTAT/Output/Other Collections.csv")



