
#install lubridate which use to find the years and months 
#in the datase
install.packages('lubridate')
library("lubridate")
library("dplyr")

#read cvs file into dataset
sp500 <- read.csv(file = "sp500.indexDailyPrice.csv",header = TRUE)


#clean dataset
#1. convert yyyymmdd into yyyy-mm-dd
sp500$yyyymmdd <- as.Date(as.character(sp500$yyyymmdd), format = '%Y%m%d')

#2. filter date >= 20000101
sp50020000101and20180117 <- subset(sp500,sp500$yyyymmdd >= '2000-01-01')

#Begin to calculate
#1. Calculate years, we get: 2000, 2001, 2002...
years <- sort.int(unique(year(sp50020000101and20180117$yyyymmdd)))

#2. Calculate each year return
sp500Performance <- data.frame()
for(eachyear in years){
	sp500temp <- subset(sp50020000101and20180117, year(sp50020000101and20180117$yyyymmdd) == eachyear)
	annualizedReturn <- cumprod(sp500temp$return +1)[nrow(sp500temp)]^(250/nrow(sp500temp))-1
	annualizedRisk <- sd(sp500temp$return)*sqrt(250)
	SharpeRatio <- annualizedReturn/annualizedRisk
	teampFrame <- data_frame(eachyear,annualizedReturn,annualizedRisk,SharpeRatio)

	sp500Performance <- rbind(sp500Performance,teampFrame)
}
write.csv(sp500Performance,file="./sp500Performance.csv", fileEncoding = "UTF-8")

#3. Calculate whole return and risk

sp500Performance <- data.frame()

annualizedReturn <- cumprod(sp50020000101and20180117$return +1)[nrow(sp50020000101and20180117)]^(250/nrow(sp50020000101and20180117))-1
annualizedRisk <- sd(sp50020000101and20180117$return)*sqrt(250)
SharpeRatio <- annualizedReturn/annualizedRisk
teampFrame <- data_frame(annualizedReturn,annualizedRisk,SharpeRatio)

sp500Performance <- rbind(sp500Performance,teampFrame)

write.csv(sp500Performance,file="./sp500PerformanceWhole.csv", fileEncoding = "UTF-8")

