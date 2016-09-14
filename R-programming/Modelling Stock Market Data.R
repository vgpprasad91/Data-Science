
//Installing the packages

install.packages("XML")
install.packages("ggplot2")
install.packages("plyr")
install.packages("reshape2")
install.packages("zoo")
library(XML)
library(ggplot2)
library(plyr)
library(reshape2)
library(zoo)

//Reading the csv file
stock <- read.csv("stock.csv")

// Cleaning the data
clean_data <- function(s){ 
s <- gsub("%|\\$|,|\\)|\\(", "", s)
s <- as.numeric(s)
}
stock <- cbind(finviz[,1:6],apply(finviz[,7:68], 2, clean_data))
hist(stock$Price, breaks=100, main="Price Distribution", xlab="Price")
hist(stock$Price[finviz$Price<150], breaks=100, main="Price Distribution", xlab="Price")
avgprices_sector <- aggregate(Price~Sector,data=stock,FUN="mean")
colnames(avgprices_sector)[2] <- "Avg price of the sector"
ggplot(avgprices_sector, aes(x=Sector, y=AvgPrice_sector, fill=Sector)) + geom_bar(stat="identity") + ggtitle("Avg Prices of sector") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
avgprices_ind <- aggregate(Price~Sector+Industry,data=stock,FUN="mean")
avgprices_ind <- avgprices_ind[order(avgprices_ind$Sector,avgprices_ind$Industry),]
colnames(avgprices_ind)[3] <- "Avg Price of Industry"

//Finding the industrial price for the financial sector instead of all the sector
ind_chart <- subset(avgprices_ind,Sector=="Financial")
ggplot(ind_chart, aes(x=Industry, y=AvgPrice_ind, fill=Industry)) + geom_bar(stat="identity") + theme(legend.position="none") + ggtitle("Avg Prices of Industries") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

// Drawing a chart for a particular industry
com_chart <- subset(stock,Industry=="Property & Casualty Insurance")
ggplot(com_chart, aes(x=Company, y=Price, fill=Company)) + geom_bar(stat="identity") + theme(legend.position="none") + ggtitle("Avg Price of the Company") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

// Chart for a particular compaany using the ticker of the company
stock <- subset(stock, Ticker!="AAPL")
avgprices_sector <- aggregate(Price~Sector,data=stock,FUN="mean")
colnames(avgprices_sector)[2] <- "Avg Price of the sector"
ggplot(avgprices_sector, aes(x=Sector, y=AvgPrice_sector, fill=Sector)) + geom_bar(stat="identity") + ggtitle("Avg Prices of the sector") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

// Generating relative valuations
avgsector <- melt(stock, id="Sector")
avgsector <- subset(avgsector,variable%in%c("Price","P.E","PEG","P.S","P.B"))
avgsector <- (na.omit(avgsector))
avgsector$value <- as.numeric(avgsector$value)
avgsector <- dcast(avgsector, Sector~variable, mean)
colnames(avgsector)[2:6] <- c("SAvgPE","SAvgPEG","SAvgPS","SAvgPB","SAvgPrice")
industry_avg <- melt(finviz, id=c("Sector","Industry"))
industry_avg <- subset(industry_avg,variable %in%
c("Price","P.E","PEG","P.S","P.B"))
avgind <- (na.omit(avgind))
avgind$value <- as.numeric(avgind$value)
avgind <- dcast(avgind, Sector+Industry~variable, mean)
avgind <- (na.omit(avgind))
colnames(avgind)[3:7] <- c("IAvgPE","IAvgPEG","IAvgPS","IAvgPB","IAvgPrice")
stock <- merge(stock, avgsector, by.x="Sector", by.y="Sector")
stock <- merge(stock, avgind, by.x=c("Sector","Industry"), by.y=c("Sector","Industry"))
stock$SPEUnder <- 0
stock$SPEGUnder <- 0
stock$SPSUnder <- 0
stock$SPBUnder <- 0
stock$SPriceUnder <- 0
stock$IPEUnder <- 0
stock$IPEGUnder <- 0
stock$IPSUnder <- 0
stock$IPBUnder <- 0
stock$IPriceUnder <- 0
stock$SPEUnder[stock$P.E<finviz$SAvgPE] <- 1
stock$SPEGUnder[stock$PEG<finviz$SAvgPEG] <- 1
stock$SPSUnder[stock$P.S<finviz$SAvgPS] <- 1
stock$SPBUnder[stock$P.B<finviz$SAvgPB] <- 1
stock$SPriceUnder[stock$Price<finviz$SAvgPrice] <- 1
stock$IPEUnder[stock$P.E<finviz$IAvgPE] <- 1
stock$IPEGUnder[stock$PEG<finviz$IAvgPEG] <- 1
stock$IPSUnder[stock$P.S<finviz$IAvgPS] <- 1
stock$IPBUnder[stock$P.B<finviz$IAvgPB] <- 1
stock$IPriceUnder[stock$Price<finviz$IAvgPrice] <- 1
stock$RelValIndex <- apply(stock[79:88],1,sum)
targetstock <- subset(stock, Price>20 & Price<100 & Volume>10000 & Country=="USA" & EPS..ttm.>0 & EPS.growth.next.year>0 & EPS.growth.next.5.years>0 & Total.Debt.Equity<1 & Beta<1.5 & Institutional.Ownership<30 & RelValIndex>8)
counter <- 0
for (symbol in targetstock$Ticker){
  stock <- read.csv("historicalstock.csv")
  stock <- na.omit(stock)
  colnames(stock)[7] <- "AdjClose"
  stock[,1] <- as.Date(stock[,1])
  stock <- cbind(Symbol=symbol,stock)
  maxrow <- nrow(stock)-49
  ma50 <- cbind(stock[1:maxrow,1:2],rollmean(stock$AdjClose,50,align="right"))
  maxrow <- nrow(stock)-199
  ma200 <- cbind(stock[1:maxrow,1:2],rollmean(stock$AdjClose,200,align="right")
  stock <- merge(stock,ma50,by.x=c("Symbol","Date"),by.y=c("Symbol","Date"),all.x=TRUE)
  colnames(stock)[9] <- "MovAvg50"
  stock <- merge(stock,ma200,by.x=c("Symbol","Date"),by.y=c("Symbol","Date"),all.x=TRUE)
  colnames(stock)[10] <- "MovAvg200"
  price_chart <-melt(stock[,c(1,2,8,9,10)],id=c("Symbol","Date"))
  qplot(Date, value, data=price_chart, geom="line",color=variable,main=paste(symbol,"Daily Stock Prices"),ylab="Price")
  ggsave(filename=paste0("stock_price_",counter,".png"))
  price_summary <- ddply(stock, "Symbol", summarise,open=Open[nrow(stock)],high=max(High),low=min(Low),close=AdjClose[1])
  if(counter==0){
    stocks <- rbind(stock)
    price_summaries <- rbind(price_summary)
  }else{
    stocks <- rbind(stocks, stock)
  price_summaries <- rbind(price_summaries, price_summary)
  }
  counter <- counter+1
}
qplot(Date, AdjClose, data=stocks, geom="line", color=Symbol,main="Daily Stock Prices")
ggsave(filename=("stock_price_combined.png"))
summary <- melt(price_summaries,id="Symbol")
ggplot(summary, aes(x=variable, y=value, fill=Symbol)) + geom_bar(stat="identity") + facet_wrap(~Symbol)
ggsave(filename=("stock_price_summaries.png"))
