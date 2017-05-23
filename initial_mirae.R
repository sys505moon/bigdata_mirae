 # Need packages
# install.packages("xlsx")
# install.packages("data.table")
# install.packages("ggplot2")
# install.packages("Hmisc")

# Library
library(xlsx)
library(data.table)
library(ggplot2)
library(Hmisc) # rcorr() 쓰기 위함

# directory setting
getwd()
setwd("/home/moon/R/bigdata_mirae/Jongho_data")
setwd("C:/Users/GooYoung/Documents/R/bigdata_mirae/gooyoung_data/")

# 재수경로



# read.xlsx 
  # data <- read.xlsx("빅데이터페스티벌DB_S&P종목_001.xlsm", sheetIndex = 3)
  # data[,1] <- paste(substr(data[,1], 3, 4), substr(data[,1],6,7), sep = '')
  # data[,1] <- as.numeric(data[,1])

# fread
end_price <- fread("end_price.csv", sep = ',')
high_price <- fread("high_price.csv", sep = ',')
low_price <- fread("low_price.csv", sep = ',')
volume <- fread("volume.csv", sep = ',')
PER <- fread("PER.csv", sep = ',')
PBR <- fread("PBR.csv", sep = ',')
EPS <- fread("EPS.csv", sep = ',')
sales <- fread("sales.csv", sep = ',')
business_profits <- fread("business_profits.csv", sep = ',')
net_profits <- fread("net_profits.csv", sep = ',')
sector <- fread("sector.csv", sep = ',')
market_capital <- fread("market_capital.csv", sep = ',')
dividend_tendency <- fread("dividend_tendency.csv", sep = ',')
dividend_rate <- fread("dividend_rate.csv", sep = ',')

  # data_table$time <- substr(data_table$time, 3, 7)data_table$time <- substr(data_table$time, 3, 7)
  # colnames(data_table)[1] <- 'time'
  # colnames(data_table)[2] <- 'apple'

# colname change

colnames(end_price)[2:length(end_price)] <- paste("end_price", colnames(end_price)[2:length(end_price)], sep = '_')
colnames(high_price)[2:length(high_price)] <- paste("high_price", colnames(high_price)[2:length(high_price)], sep = '_')
colnames(low_price)[2:length(low_price)] <- paste("low_price", colnames(low_price)[2:length(low_price)], sep = '_')
colnames(volume)[2:length(volume)] <- paste("volume", colnames(volume)[2:length(volume)], sep = '_')
colnames(PER)[2:length(PER)] <- paste("PER", colnames(PER)[2:length(PER)], sep = '_')
colnames(PBR)[2:length(PBR)] <- paste("PBR", colnames(PBR)[2:length(PBR)], sep = '_')
colnames(EPS)[2:length(EPS)] <- paste("EPS", colnames(EPS)[2:length(EPS)], sep = '_')
colnames(sales)[2:length(sales)] <- paste("sales", colnames(sales)[2:length(sales)], sep = '_')
colnames(business_profits)[2:length(business_profits)] <- paste("business_profits", colnames(business_profits)[2:length(business_profits)], sep = '_')
colnames(net_profits)[2:length(net_profits)] <- paste("net_profits", colnames(net_profits)[2:length(net_profits)], sep = '_')
colnames(market_capital)[2:length(market_capital)] <- paste("market_capital", colnames(market_capital)[2:length(market_capital)], sep = '_')
colnames(dividend_tendency)[2:length(dividend_tendency)] <- paste("dividend_tendency", colnames(dividend_tendency)[2:length(dividend_tendency)], sep = '_')
colnames(dividend_rate)[2:length(dividend_rate)] <- paste("dividend_rate", colnames(dividend_rate)[2:length(dividend_rate)], sep = '_')

# Convert data.frame

end_price <- data.frame(end_price)
high_price <- data.frame(high_price)
low_price <- data.frame(low_price)
volume <- data.frame(volume)
PER <- data.frame(PER)
PBR <- data.frame(PBR)
EPS <- data.frame(EPS)
sales <- data.frame(sales)
business_profits <- data.frame(business_profits)
net_profits <- data.frame(net_profits)
market_capital <- data.frame(market_capital)
dividend_tendency <- data.frame(dividend_tendency)
dividend_rate <- data.frame(dividend_rate)

# group(item) data_integration  
  # ex)x_AAPL 등 종목별로 각 지표(종가, 거래량 등)를 묶음
  # *주의* 데이터들을 data.table 에서 data.frame 으로 변형해야 함

item_names <- fread("item names.csv", sep =',', header = T)
item_names <- t(item_names)
item_names <- item_names[-1,]
colnames(item_names) <- c("English", "Korean")
item_names <- as.data.frame(item_names)
item_strsplit <- data.frame(do.call('rbind', strsplit(as.character(item_names$English), split = ' ', fixed = TRUE)))

variable_box = c()
for (i in 1:length(item_strsplit$X1)){
  variable_box <- c(variable_box, paste("x", item_strsplit[i,1], sep = '_'))
}

for (i in 1:length(item_strsplit$X1)){
  assign(paste("x", item_strsplit[i,1], sep = '_'), merge(cbind(end_price[c(1, 1+i)], high_price[i+1], low_price[i+1], 
                                                                volume[i+1], market_capital[i+1], 
                                                                dividend_tendency[i+1], dividend_rate[i+1]),
                                                          cbind(PER[c(1,i+1)], PBR[i+1], EPS[i+1], sales[i+1],
                                                                business_profits[i+1], net_profits[i+1]),
                                                          by = 'time', all = TRUE ))
}

 # make data-mart's(item group data) colnames to simple (delete unnecessary words)

for (i in 1:length(variable_box)){
  frame <- get(variable_box[i])
  colnames(frame) <- gsub("_AAPL.UW.Equity", "" , colnames(x_AAPL))
  assign(variable_box[i], frame)
}

 # group(item) correlation search

x_AAPL_cor <- rcorr(as.matrix(x_AAPL[2:14]), type = "pearson")$r
write.csv(x_AAPL_cor, "x_AAPL_cor.csv", row.names = TRUE, fileEncoding = "EUC-KR")

x_AMZN_cor <- rcorr(as.matrix(x_AMZN[2:14]), type = "pearson")$r


# Income rate calculate

cal_income <- function(){
  income = matrix(c(0), nrow = 204, ncol = 121)
  for (i in 2:length(end_price$time)){
    for (j in 2:length(end_price)){
      income[i-1,j] <- (end_price[i,j]-end_price[i-1,j])/end_price[i-1,j]
      }
  }
  return(income)
}
income_rate <- cal_income() # Calculation fot imcome_Ratio
income_rate[,1] <- end_price$time
income_rate[,1] <- paste(substr(income_rate[,1], 3, 4), substr(income_rate[,1],6,7), sep = '')
colnames(income_rate) <- colnames(end_price)
income_rate <- cbind(income_rate, apply(income_rate[,2:121], 1, max, na.rm = TRUE))
income_rate <- cbind(income_rate, apply(income_rate[,2:121], 1, which.max))
colnames(income_rate)[122] <- "best_income_rate"
colnames(income_rate)[123] <- "which.max"
write.csv(income_rate, "income_rate.csv", fileEncoding = "EUC-KR", row.names = FALSE)

 # ggplot(income_AAPL, aes(x=end_price.time, y=income, group=1))+geom_point()


 # Calculate best income-rate each month in data

    #(1) 리스트로 반환값 (차후 계산시 편리할 것)
    TopRankItems <- list()
    TopRankValues <- list()
    SearchTopRank <- function(x, n){
      for (i in 1:nrow(x)){
        TopRankItems[i] <- 0
        TopRankValues[i] <- 0
        for (j in 1:n){
          which <- which.max(x[i,2:121])
          TopRankItems[[i]][j] <- x[i,2:121][which]
          TopRankValues[[i]][j] <- colnames(x[,2:121])[which]
          x[i,2:121][which] <- NA
        }
      }
      basket <- list(TopRankItems, TopRankValues)
      return(basket)
    }
    
    
    #(2) data.frame 반환값 (이해하기 편함)
    TopRankItems <- list()
    SearchTopRank <- function(x, n){
      for (i in 1:nrow(x)){
        TopRankItems[i] <- 0
        for (j in 1:n){
          which <- which.max(x[i,2:121])
          TopRankItems[[i]][j] <- colnames(x[,2:121])[which] 
          TopRankItems[[i]][j+5] <- x[i,2:121][which]
          x[i,2:121][which] <- NA
        }
      }
      return(TopRankItems)
    }
    BestIncomeRate <- SearchTopRank(income_rate, 5)
    BestIncomeRate <- data.frame(matrix(unlist(BestIncomeRate), nrow = 5))
    rownames(BestIncomeRate) <- c("1위", "2위", "3위", "4위", "5위")
    colnames(BestIncomeRate) <- rep(income_rate[,1], each = 2) 
                                              # rep(c(1,2), 2)의 결과는 1,2,1,2 이고
                                              # rep(c(1,2), each =2)의 결과는 1,1,2,2 이다.
    write.csv(BestIncomeRate, "BestIncomeRate.csv", fileEncoding = "EUC-KR", row.names = TRUE)







# data_View


ggplot(data = data_table, aes(x=time, y=apple))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
ggplot(data = data, aes(x=data[,1], y=data[,2], group = 1))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
ggplot(data = data, aes(x=data[,1], y=data[,3]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
ggplot(data = data, aes(x=data[,1], y=data[,4]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
ggplot(data = data, aes(x=data[,1], y=data[,5]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
ggplot(data = data, aes(x=data[,1], y=data[,6]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
ggplot(data = data, aes(x=data[,1], y=data[,7]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
ggplot(data = data, aes(x=data[,1], y=data[,8]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
ggplot(data = data, aes(x=data[,1], y=data[,9]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
ggplot(data = data, aes(x=data[,1], y=data[,10]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
ggplot(data = data, aes(x=data[,1], y=data[,11]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
ggplot(data = data, aes(x=data[,1], y=data[,12]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
ggplot(data = data, aes(x=data[,1], y=data[,13]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
ggplot(data = data, aes(x=data[,1], y=data[,14]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
ggplot(data = data, aes(x=data[,1], y=data[,15]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
ggplot(data = data, aes(x=data[,1], y=data[,16]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
ggplot(data = data, aes(x=data[,1], y=data[,17]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
ggplot(data = data, aes(x=data[,1], y=data[,18]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
ggplot(data = data, aes(x=data[,1], y=data[,19]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
ggplot(data = data, aes(x=data[,1], y=data[,20]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
ggplot(data = data, aes(x=data[,1], y=data[,21]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
ggplot(data = data, aes(x=data[,1], y=data[,22]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
ggplot(data = data, aes(x=data[,1], y=data[,23]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
ggplot(data = data, aes(x=data[,1], y=data[,24]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
ggplot(data = data, aes(x=data[,1], y=data[,25]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
ggplot(data = data, aes(x=data[,1], y=data[,26]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
ggplot(data = data, aes(x=data[,1], y=data[,27]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
ggplot(data = data, aes(x=data[,1], y=data[,28]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
ggplot(data = data, aes(x=data[,1], y=data[,29]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
ggplot(data = data, aes(x=data[,1], y=data[,30]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
ggplot(data = data, aes(x=data[,1], y=data[,31]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
ggplot(data = data, aes(x=data[,1], y=data[,32]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
ggplot(data = data, aes(x=data[,1], y=data[,33]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
ggplot(data = data, aes(x=data[,1], y=data[,34]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
ggplot(data = data, aes(x=data[,1], y=data[,35]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
ggplot(data = data, aes(x=data[,1], y=data[,36]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
ggplot(data = data, aes(x=data[,1], y=data[,37]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
ggplot(data = data, aes(x=data[,1], y=data[,38]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
ggplot(data = data, aes(x=data[,1], y=data[,39]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
ggplot(data = data, aes(x=data[,1], y=data[,40]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))

ggplot(data = data, aes(x=data[,1], y=data[,94]))+geom_point()+geom_line()+theme(axis.text.x = element_text(angle = 45, size = 5))
ggplot(data = data, aes(x=data[,1], y=data[,94]))+geom_line()+theme(axis.text.x = element_text(angle = 45, size = 5))

################################################################################
############ item-resource correlation analysis_by GooYoung ####################
################################################################################

resource <- fread("resource.csv")
resource$time <- substr(resource$time,1,7)
resource$time <- gsub("-","/",resource$time)
resource <- as.data.frame(resource)

resource_item <- merge(x=resource,y=end_price,by="time",all=TRUE)
View(resource_item)

ggplot(data = resource, aes(x=resource[,1], y=resource[,2]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
ggplot(data = resource, aes(x=resource[,1], y=resource[,3]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
ggplot(data = resource, aes(x=resource[,1], y=resource[,4]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
ggplot(data = resource, aes(x=resource[,1], y=resource[,5]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
ggplot(data = resource, aes(x=resource[,1], y=resource[,6]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
ggplot(data = resource, aes(x=resource[,1], y=resource[,7]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
ggplot(data = resource, aes(x=resource[,1], y=resource[,8]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))

rcorr(as.matrix(resource_item[,-c(1)]), type="pearson")$r
resource_item_cor <- data.frame((rcorr(as.matrix(resource_item[,-c(1)]), type="pearson")$r)[1:80,])
resource_item_P <- data.frame((rcorr(as.matrix(resource_item[,-c(1)]), type="pearson")$P)[1:80,])
resource_item_cor <- resource_item_cor[,-c(1:80)]
resource_item_P <- resource_item_P[,-c(1:80)]

View(resource_item_cor)

(resource_item_cor)
View(resource_item_P)


which(item_resource_P>0.05)
item_resource_cor[7]

for(i in which(item_resource_P>0.05))(
  item_resource_cor[i] <- NA
)
