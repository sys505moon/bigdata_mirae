# Need packages
# install.packages("xlsx")
# install.packages("data.table")
# install.packages("ggplot2")
# install.packages("Hmisc")
# install.packages('dplyr')
# install.packages('dtplyr')
# install.packages('rnn')

# Library
library(xlsx)
library(data.table)
library(ggplot2)
library(Hmisc) # rcorr() 쓰기 위함
library(dplyr)
library(dtplyr)
library(rnn)

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

# item_names <- fread("item_names.txt", sep =',', header = T)
item_names <- fread("item names.csv", sep =',', header = T)
item_names <- t(item_names)
item_names <- item_names[-1,]
colnames(item_names) <- c("English", "Korean")
item_names <- as.data.frame(item_names)
item_strsplit <- data.frame(do.call('rbind', strsplit(as.character(item_names$English), split = ' ', fixed = TRUE)))
item_names <- cbind(item_names, as.data.frame(sector$`GICS 섹터`))
colnames(item_names)[3] <- "sector"

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

  # All items correlation mean data by variable( ex) high, low, volume)
for(i in 1:length(item_strsplit$X1)){
  assign(paste("x", item_strsplit$X1[i], "cor", sep = "_"), rcorr(as.matrix(get(paste("x", item_strsplit$X1[i], sep = "_"))[2:14]), type = "pearson")$r)
}
All_Variable_Corr_mean = matrix(nrow = nrow(x_AAPL_cor), byrow = TRUE)
colnames(All_Variable_Corr_mean) <-  "end_price"
rownames(All_Variable_Corr_mean) <- rownames(x_AAPL_cor)
contempo = c()
for(m in 1:nrow(x_AAPL_cor)){
  for(i in 1:length(item_strsplit$X1)){
    contempo = c(contempo, get(paste("x", item_strsplit$X1[i], "cor", sep = "_"))[m,1])
  }
  All_Variable_Corr_mean[m,1] = mean(contempo, na.rm = TRUE)
}
rm(contempo)

  # bundle of correlations data by items
cor_by_items <- as.data.frame(x_AAPL_cor)[1]
for(k in 1:(length(item_strsplit$X1)-1)){
  cor_by_items = cbind(cor_by_items, as.data.frame(get(paste("x", item_strsplit$X1[k+1], "cor", sep = "_")))[1])
}
colnames(cor_by_items) <- item_strsplit$X1

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

# #(1) 리스트로 반환값 (차후 계산시 편리할 것)
# TopRankItems <- list()
# TopRankValues <- list()
# SearchTopRank <- function(x, n){
#   for (i in 1:nrow(x)){
#     TopRankItems[i] <- 0
#     TopRankValues[i] <- 0
#     for (j in 1:n){
#       which <- which.max(x[i,2:121])
#       TopRankItems[[i]][j] <- x[i,2:121][which]
#       TopRankValues[[i]][j] <- colnames(x[,2:121])[which]
#       x[i,2:121][which] <- NA
#     }
#   }
#   basket <- list(TopRankItems, TopRankValues)
#   return(basket)
# }
# 
# 
# #(2) data.frame 반환값 (이해하기 편함)
# TopRankItems <- list()
# SearchTopRank <- function(x, n){
#   for (i in 1:nrow(x)){
#     TopRankItems[i] <- 0
#     for (j in 1:n){
#       which <- which.max(x[i,2:121])
#       TopRankItems[[i]][j] <- colnames(x[,2:121])[which] 
#       TopRankItems[[i]][j+5] <- x[i,2:121][which]
#       x[i,2:121][which] <- NA
#     }
#   }
#   return(TopRankItems)
# }
# BestIncomeRate <- SearchTopRank(income_rate, 5)
# BestIncomeRate <- data.frame(matrix(unlist(BestIncomeRate), nrow = 5))
# rownames(BestIncomeRate) <- c("1위", "2위", "3위", "4위", "5위")
# colnames(BestIncomeRate) <- rep(income_rate[,1], each = 2) 
# # rep(c(1,2), 2)의 결과는 1,2,1,2 이고
# # rep(c(1,2), each =2)의 결과는 1,1,2,2 이다.
# write.csv(BestIncomeRate, "BestIncomeRate.csv", fileEncoding = "EUC-KR", row.names = TRUE)

# Make RNN model 
normalize <- function(x){
  return((x-min(x, na.rm = TRUE))/(max(x, na.rm = TRUE)-min(x, na.rm = TRUE)))
}

  # item bundle using sector data
IT_index <- which(item_names$sector == "Information Technology")
Finan_index <- which(item_names$sector == "Financials")
CD_index <- which(item_names$sector == "Consumer Discretionary")
Health_index <- which(item_names$sector == "Health Care")
Energy_index <- which(item_names$sector == "Energy")
Indust_index <- which(item_names$sector == "Industrials")
Telecom_index <- which(item_names$sector == "Telecommunication Services")
CS_index <- which(item_names$sector == "Consumer Staples")
Material_index <- which(item_names$sector == "Materials")
Utility_index <- which(item_names$sector == "Utilities")
Estate_index <- which(item_names$sector == "Real Estate")

end_price_normalize <- as.data.frame(lapply(end_price[2:121], normalize))
high_price_normalize <- as.data.frame(lapply(high_price[2:121], normalize))
low_price_normalize <- as.data.frame(lapply(low_price[2:121], normalize))
volume_normalize <- as.data.frame(lapply(volume[2:121], normalize))
market_capital_normalize <- as.data.frame(lapply(market_capital[2:121], normalize))
dividend_tendency_normalize <- as.data.frame(lapply(dividend_tendency[2:121], normalize))
for(i in 1:nrow(dividend_tendency_normalize)){
  for(j in 1:ncol(dividend_tendency_normalize)){
    if(is.nan(dividend_tendency_normalize[i,j])){
      dividend_tendency_normalize[i,j] = NA
    }
  }
}
dividend_rate_normalize <- as.data.frame(lapply(dividend_rate[2:121], normalize))
resource_normalize <- as.data.frame(lapply(resource[c(2:40,80,81)], normalize))


# 클러스터 군집의 관련 원자재 data index 
for(i in 1:length(sort(unique(item_names$cluster)))){
  assign(paste("index", i, sep = "_"),
         which(item_names$cluster == i))
}

 # RNN model_2
sector_name = as.character(unique(item_names$sector))
short_sector_name = c("IT", "Finan", "CD", "Health", "Energy", "Indust", "Telecom", "CS", "Material", "Utility", "Estate")

 # Item name and data length(except NA) bundle in CLUSTER
for(i in 1:length(short_sector_name)){
  assign(paste(short_sector_name[i], "sector", "length", sep = "_"), vector())
  for(j in 1:length(item_strsplit$X1[which(item_names$sector == sector_name[i])])){
    assign(paste(short_sector_name[i], "sector", "length", sep = "_"),
           c(get(paste(short_sector_name[i], "sector", "length", sep = "_")),
             nrow(end_price_normalize[which(item_names$sector == sector_name[i])][j])
             -sum(is.na(end_price_normalize[which(item_names$sector == sector_name[i])][j]))))
  }
}



  
# 주 변수
sub_x <- c("end_price_normalize", "high_price_normalize", "low_price_normalize", "volume_normalize",  "market_capital_normalize")


# RNN 실행 함수

RunRNN <- function(epoch){
  for(i in 1:length(unique(item_names$cluster))){
    print(paste("i = ", i, "시작"))
    mrm_index <- data.frame(length = apply(end_price_normalize, 2 , function(x){nrow(end_price_normalize)-sum(is.na(x))}),
                            name = item_strsplit$X1)
    mrm_index <- mutate(mrm_index, index = 1:120)
    mrm_index <- mrm_index[get(paste("index", i, sep = "_")),]
    NA_num <- data.frame(NA_Num = apply(end_price_normalize, 2 , function(x){sum(is.na(x))}))
    NA_num <- transform(NA_num, index = 1:ncol(end_price_normalize))
    for(j in 1:length(unique(mrm_index$length))){
      print(paste("j = ", j, "시작"))
      # Making input data 
      # sort(unique(mrm_index$length), decreasing = T)[1]
      # sub_x 변수 생성
      sub_x_box = c()
      material_box = c()
      for(k in 1:(length(sub_x))){
        if(k == 1){
          assign(paste("x", k, sep = "_"),
                 t(get(sub_x[k])[filter(mrm_index, length == sort(unique(mrm_index$length), decreasing = T)[j])$index])[,(204-sort(unique(mrm_index$length), decreasing = T)[j]+2):204, drop = F]
          )
        }else{
          assign(paste("x", k, sep = "_"),
                 t(get(sub_x[k])[filter(mrm_index, length == sort(unique(mrm_index$length), decreasing = T)[j])$index])[,(204-sort(unique(mrm_index$length), decreasing = T)[j]+1):203, drop = F]
          )
        }
        print(paste("    k =", k, "완료"))
        print(dim(get(paste("x", k, sep = "_"))))
      }
      for(k in 1:(length(sub_x))){
        if(k != 1){
          sub_x_box <- c(sub_x_box, get(paste("x", k, sep = "_")))  
          print("실행")
        }
      }
      
      # 원자재 변수 생성
      if(length(unlist(cluster_item_Material[[i]][2])) > 0){
        for(l in 1:length(unlist(cluster_item_Material[[i]][2]))){
          print(l)
          assign(paste("x", length(sub_x)+l, sep = "_"),
                 t(get(paste("Material", unlist(cluster_item_Material[[i]][2])[l], "normalize", sep = "_"))[filter(mrm_index, length == sort(unique(mrm_index$length), decreasing = T)[j])$index])[,(204-sort(unique(mrm_index$length), decreasing = T)[j]+1):203, drop = F]
          ) # 구영이 리스트 보고 수정
          material_box <- c(material_box, get(paste("x", length(sub_x)+l, sep = "_")))
        }
      }
  
  
      # 혹시모를 NA 처리 / dim = (sort(unique(mrm_index$length), decreasing = T)[j]) X 클러스터에 속한 sample 수
      
      x <- array(c(sub_x_box, material_box), dim = c(dim(x_2), length(sub_x)+length(unlist(cluster_item_Material[[i]][2]))))
      # x <- array(c(sub_x_box, material_box), dim = c(dim(x_2), k+l))
      if(dim(x_1)[2] >= 47){
        y <- array(x_1, dim = c(dim(x_1),1))
      }else{
        next()
      }
      # Making RNN model
      
      # train <- 1:(round(sort(unique(mrm_index$length), decreasing = T)[j]-1)*0.8)
      # test <- seq(1:(round(sort(unique(mrm_index$length), decreasing = T)[j]-1)))[-train]
      if(NA_num[filter(mrm_index, length == sort(unique(mrm_index$length), decreasing = T)[j][1])$index[1],1] == 0){
        train = 1:155
        test = 156:203
        
        
      }else{
        train = 1:(156-(NA_num[filter(mrm_index, length == sort(unique(mrm_index$length), decreasing = T)[j][1])$index[1],1]+1))
        test = (156-(NA_num[filter(mrm_index, length == sort(unique(mrm_index$length), decreasing = T)[j][1])$index[1],1]+1)+1):(203-NA_num[filter(mrm_index, length == sort(unique(mrm_index$length), decreasing = T)[j][1])$index[1],1])
    
        
      }
      
      # if(NA_num[filter(mrm_index, length == sort(unique(mrm_index$length), decreasing = T)[j][1])$index[1],1] == 0){
      #   x_train = 1:155
      #   x_test = 156:203
      #   y_train = 1:155
      #   y_test = 156:203
      # }else{
      #   x_train = 1:(156-(NA_num[filter(mrm_index, length == sort(unique(mrm_index$length), decreasing = T)[j][1])$index[1],1]+1))
      #   x_test = (156-(NA_num[filter(mrm_index, length == sort(unique(mrm_index$length), decreasing = T)[j][1])$index[1],1]+1)):(203-NA_num[filter(mrm_index, length == sort(unique(mrm_index$length), decreasing = T)[j][1])$index[1],1])
      #   y_train = 1:(156-(NA_num[filter(mrm_index, length == sort(unique(mrm_index$length), decreasing = T)[j][1])$index[1],1]+1))
      #   y_test = (157-NA_num[filter(mrm_index, length == sort(unique(mrm_index$length), decreasing = T)[j][1])$index[1],1]+1):(204-(NA_num[filter(mrm_index, length == sort(unique(mrm_index$length), decreasing = T)[j][1])$index[1],1]+1))
      # }
      
      assign(paste(paste("index", i, sep = "_"), "RNN_model", j, sep = "_"),
             trainr(Y = y[,train,, drop = F],
                    X = x[,train,, drop = F],
                    learningrate = 0.035,
                    hidden_dim = 14,
                    batch_size = 1,
                    numepochs = epoch))
      
      assign(paste(paste("index", i, sep = "_"), "RNN_predict", j, sep = "_"),
             predictr(get(paste(paste("index", i, sep = "_"), "RNN_model", j, sep = "_")),
                      X = x[,test,, drop = F]),
             envir = .GlobalEnv)
      assign(paste(paste("index", i, sep = "_"), "evaluation", j, sep = "_"),
             data.frame(error = (y[,test,])-get(paste(paste("index", i, sep = "_"), "RNN_predict", j, sep = "_"))),
             envir = .GlobalEnv)
      print(paste("  j =", j, "완료"))
    }
    print(paste("i =", i, "완료"))
  }
}

system.time(RunRNN(3600))

# data merge by sector

 # error ( evaluation )

for(i in 1:length(short_sector_name)){
  mrm_index <- data.frame(length = apply(end_price_normalize, 2 , function(x){nrow(end_price_normalize)-sum(is.na(x))}),
                          name = item_strsplit$X1)
  mrm_index <- mutate(mrm_index, index = 1:120)
  mrm_index <- mrm_index[get(paste(short_sector_name[i], "index", sep = "_")),]
  for(j in 1:length(unique(mrm_index$length))){
    a <- filter(mrm_index, length == sort(unique(mrm_index$length), decreasing = T)[j])
    assign(paste(short_sector_name[i], "error", j, sep = "_"),
           mutate(get(paste(short_sector_name[i], "evaluation", j, sep = "_")), name = a$name, index = a$index))
  }
}

IT_evaluation_result <- bind_rows(IT_evaluation_1, IT_evaluation_2, IT_evaluation_3, IT_evaluation_4, IT_evaluation_5,
                                  IT_evaluation_6, IT_evaluation_7, IT_evaluation_8, IT_evaluation_9, IT_evaluation_10,
                                  IT_evaluation_11, IT_evaluation_12, IT_evaluation_13)
write.csv(IT_evaluation_result, "IT_error_result_iter3500.csv")
Finan_evaluation_result <- bind_rows(Finan_evaluation_1, Finan_evaluation_2, Finan_evaluation_3)
write.csv(Finan_evaluation_result, "Finan_error_result_iter3500.csv")
CD_evaluation_result <- bind_rows(CD_evaluation_1, CD_evaluation_2, CD_evaluation_3, CD_evaluation_4, CD_evaluation_5)
write.csv(CD_evaluation_result, "CD_error_result_iter3500.csv")
Health_evaluation_result <- bind_rows(Health_evaluation_1, Health_evaluation_2, Health_evaluation_3)
write.csv(Health_evaluation_result, "Health_error_result_iter3500.csv")
Energy_evaluation_result <- bind_rows(Energy_evaluation_1, Energy_evaluation_2)
write.csv(Energy_evaluation_result, "Energy_error_result_iter3500.csv")
Indust_evaluation_result <- bind_rows(Indust_evaluation_1, Indust_evaluation_2)
write.csv(Indust_evaluation_result, "Indust_error_result_iter3500.csv")
Telecom_evaluation_result <- bind_rows(Telecom_evaluation_1)
write.csv(Telecom_evaluation_result, "Telecom_error_result_iter3500.csv")
CS_evaluation_result <- bind_rows(CS_evaluation_1, CS_evaluation_2, CS_evaluation_3, CS_evaluation_4, CS_evaluation_5)
write.csv(CS_evaluation_result, "CS_error_result_iter3500.csv")
Material_evaluation_result <- bind_rows(Material_evaluation_1, Material_evaluation_2)
write.csv(Material_evaluation_result, "Material_error_result_iter3500.csv")
Utility_evaluation_result <- bind_rows(Utility_evaluation_1)
write.csv(Utility_evaluation_result, "Utility_error_result_iter3500.csv")
Estate_evaluation_result <- bind_rows(Estate_evaluation_1)
write.csv(Estate_evaluation_result, "Estate_error_result_iter3500.csv")

All_evaluation_result <- bind_rows(IT_evaluation_result, Finan_evaluation_result, CD_evaluation_result, Health_evaluation_result,
                                   Energy_evaluation_result, Indust_evaluation_result, Telecom_evaluation_result, CS_evaluation_result,
                                   Material_evaluation_result, Utility_evaluation_result, Estate_evaluation_result)
write.csv(All_evaluation_result, "All_error_result_iter3500.csv")
  
  # predict

for(i in 1:length(short_sector_name)){
  mrm_index <- data.frame(length = apply(end_price_normalize, 2 , function(x){nrow(end_price_normalize)-sum(is.na(x))}),
                          name = item_strsplit$X1)
  mrm_index <- mutate(mrm_index, index = 1:120)
  mrm_index <- mrm_index[get(paste(short_sector_name[i], "index", sep = "_")),]
  for(j in 1:length(unique(mrm_index$length))){
    a <- filter(mrm_index, length == sort(unique(mrm_index$length), decreasing = T)[j])
    assign(paste(short_sector_name[i], "RNN_predict", j, sep = "_"),
           mutate(as.data.frame(get(paste(short_sector_name[i], "RNN_predict", j, sep = "_"))), name = a$name, index = a$index))
  }
}

IT_RNN_predict_result <- bind_rows(IT_RNN_predict_1, IT_RNN_predict_2, IT_RNN_predict_3, IT_RNN_predict_4, IT_RNN_predict_5,
                                  IT_RNN_predict_6, IT_RNN_predict_7, IT_RNN_predict_8, IT_RNN_predict_9, IT_RNN_predict_10,
                                  IT_RNN_predict_11, IT_RNN_predict_12, IT_RNN_predict_13)
write.csv(IT_RNN_predict_result, "IT_RNN_predict_result_iter3500.csv")
Finan_RNN_predict_result <- bind_rows(Finan_RNN_predict_1, Finan_RNN_predict_2, Finan_RNN_predict_3)
write.csv(Finan_RNN_predict_result, "Finan_RNN_predict_result_iter3500.csv")
CD_RNN_predict_result <- bind_rows(CD_RNN_predict_1, CD_RNN_predict_2, CD_RNN_predict_3, CD_RNN_predict_4, CD_RNN_predict_5)
write.csv(CD_RNN_predict_result, "CD_RNN_predict_result_iter3500.csv")
Health_RNN_predict_result <- bind_rows(Health_RNN_predict_1, Health_RNN_predict_2, Health_RNN_predict_3)
write.csv(Health_RNN_predict_result, "Health_RNN_predict_result_iter3500.csv")
Energy_RNN_predict_result <- bind_rows(Energy_RNN_predict_1, Energy_RNN_predict_2)
write.csv(Energy_RNN_predict_result, "Energy_RNN_predict_result_iter3500.csv")
Indust_RNN_predict_result <- bind_rows(Indust_RNN_predict_1, Indust_RNN_predict_2)
write.csv(Indust_RNN_predict_result, "Indust_RNN_predict_result_iter3500.csv")
Telecom_RNN_predict_result <- bind_rows(Telecom_RNN_predict_1)
write.csv(Telecom_RNN_predict_result, "Telecom_RNN_predict_result_iter3500.csv")
CS_RNN_predict_result <- bind_rows(CS_RNN_predict_1, CS_RNN_predict_2, CS_RNN_predict_3, CS_RNN_predict_4, CS_RNN_predict_5)
write.csv(CS_RNN_predict_result, "CS_RNN_predict_result_iter3500.csv")
Material_RNN_predict_result <- bind_rows(Material_RNN_predict_1, Material_RNN_predict_2)
write.csv(Material_RNN_predict_result, "Material_RNN_predict_result_iter3500.csv")
Utility_RNN_predict_result <- bind_rows(Utility_RNN_predict_1)
write.csv(Utility_RNN_predict_result, "Utility_RNN_predict_result_iter3500.csv")
Estate_RNN_predict_result <- bind_rows(Estate_RNN_predict_1)
write.csv(Estate_RNN_predict_result, "Estate_RNN_predict_result_iter3500.csv")

All_RNN_predict_result <- bind_rows(IT_RNN_predict_result, Finan_RNN_predict_result, CD_RNN_predict_result, Health_RNN_predict_result,
                                   Energy_RNN_predict_result, Indust_RNN_predict_result, Telecom_RNN_predict_result, CS_RNN_predict_result,
                                   Material_RNN_predict_result, Utility_RNN_predict_result, Estate_RNN_predict_result)
write.csv(All_RNN_predict_result, "All_RNN_predict_result_iter3500.csv")


  # denormalize (max/min data)
end_max_min <- data.frame(max = apply(end_price[2:121], 2, max, na.rm = TRUE), min = apply(end_price[2:121], 2, min, na.rm = TRUE))
high_max_min <- data.frame(max = apply(high_price[2:121], 2, max, na.rm = TRUE), min = apply(high_price[2:121], 2, min, na.rm = TRUE))
low_max_min <- data.frame(max = apply(low_price[2:121], 2, max, na.rm = TRUE), min = apply(low_price[2:121], 2, min, na.rm = TRUE))
volume_max_min <- data.frame(max = apply(volume[2:121], 2, max, na.rm = TRUE), min = apply(volume[2:121], 2, min, na.rm = TRUE))
market_max_min <- data.frame(max = apply(market_capital[2:121], 2, max, na.rm = TRUE), min = apply(market_capital[2:121], 2, min, na.rm = TRUE))


  # index_i data result

for(p in 1:length(unique(item_names$cluster))){
  mrm_index <- data.frame(length = apply(end_price_normalize, 2 , function(x){nrow(end_price_normalize)-sum(is.na(x))}),
                          name = item_strsplit$X1)
  mrm_index <- mutate(mrm_index, index = 1:120)
  mrm_index <- mrm_index[get(paste("index", p, sep = "_")),]
  for(q in 1:length(unique(mrm_index$length))){
    if(sort(unique(mrm_index$length), decreasing = T)[q] > 47){
      a <- filter(mrm_index, length == sort(unique(mrm_index$length), decreasing = T)[q])
      assign(paste("index", p, "RNN_predict", q, sep = "_"),
             transform(get(paste("index", p, "RNN_predict", q, sep = "_")), name = a$name, index = a$index))
    }
  }
}


 # convert 'predict' file type : matrix to data.frame

for(p in 1:length(sort(unique(item_names$cluster)))){
  mrm_index <- data.frame(length = apply(end_price_normalize, 2 , function(x){nrow(end_price_normalize)-sum(is.na(x))}),
                          name = item_strsplit$X1)
  mrm_index <- mutate(mrm_index, index = 1:120)
  mrm_index <- mrm_index[get(paste("index", p, sep = "_")),]
  for(q in 1:length(unique(mrm_index$length))){
    if(sort(unique(mrm_index$length), decreasing = T)[q] > 47){
      assign(paste("index", p, "RNN_predict", q, sep = "_"),
             as.data.frame(get(paste("index", p, "RNN_predict", q, sep = "_"))))
      }
  }
}


# result analysis
  # Select "data length > 47"
mrm_index <- data.frame(length = apply(end_price_normalize, 2 , function(x){nrow(end_price_normalize)-sum(is.na(x))}),
                        name = item_strsplit$X1)
mrm_index <- mutate(mrm_index, index = 1:120)
Over47length <- filter(mrm_index, length > 47)
end_max_min <- mutate(end_max_min, index = 1:120)
Over47length<- merge(Over47length, end_max_min, by = "index")
write.csv(Over47length, "Over47length.csv")

# 
# 
# index_1_predict <- bind_rows(index_1_RNN_predict_1)
# write.csv(index_1_predict, "index_1_predict.csv")
# index_2_predict <- bind_rows(index_2_RNN_predict_1, index_2_RNN_predict_2)
# write.csv(index_2_predict, "index_2_predict.csv")
# index_3_predict <- bind_rows(index_3_RNN_predict_1, index_3_RNN_predict_2)
# write.csv(index_3_predict, "index_3_predict.csv")
# index_4_predict <- bind_rows(index_4_RNN_predict_1)
# write.csv(index_4_predict, "index_4_predict.csv")
# index_5_predict <- bind_rows(index_5_RNN_predict_1)
# write.csv(index_5_predict, "index_5_predict.csv")
# index_6_predict <- bind_rows(index_6_RNN_predict_1)
# write.csv(index_6_predict, "index_6_predict.csv")
# index_7_predict <- bind_rows(index_7_RNN_predict_1)
# write.csv(index_7_predict, "index_7_predict.csv")
# index_8_predict <- bind_rows(index_8_RNN_predict_1, index_8_RNN_predict_2, index_8_RNN_predict_3, index_8_RNN_predict_4)
# write.csv(index_8_predict, "index_8_predict.csv")
# index_9_predict <- bind_rows(index_9_RNN_predict_1)
# write.csv(index_9_predict, "index_9_predict.csv")
# index_10_predict <- bind_rows(index_10_RNN_predict_1, index_10_RNN_predict_2)
# write.csv(index_10_predict, "index_10_predict.csv")
# index_11_predict <- bind_rows(index_11_RNN_predict_1)
# write.csv(index_11_predict, "index_11_predict.csv")
# index_12_predict <- bind_rows(index_12_RNN_predict_1)
# write.csv(index_12_predict, "index_12_predict.csv")
# index_13_predict <- bind_rows(index_13_RNN_predict_1, index_13_RNN_predict_2, index_13_RNN_predict_3, index_13_RNN_predict_4, index_13_RNN_predict_5)
# write.csv(index_13_predict, "index_13_predict.csv")
# index_14_predict <- bind_rows(index_14_RNN_predict_1)
# write.csv(index_14_predict, "index_14_predict.csv")
# index_15_predict <- bind_rows(index_15_RNN_predict_1, index_15_RNN_predict_2)
# write.csv(index_15_predict, "index_15_predict.csv")
# index_16_predict <- bind_rows(index_16_RNN_predict_1, index_16_RNN_predict_2)
# write.csv(index_16_predict, "index_16_predict.csv")
# index_17_predict <- bind_rows(index_17_RNN_predict_1)
# write.csv(index_17_predict, "index_17_predict.csv")
# index_18_predict <- bind_rows(index_18_RNN_predict_1, index_18_RNN_predict_2, index_18_RNN_predict_3, index_18_RNN_predict_4, index_18_RNN_predict_5)
# write.csv(index_18_predict, "index_18_predict.csv")
# index_19_predict <- bind_rows(index_19_RNN_predict_1)
# write.csv(index_19_predict, "index_19_predict.csv")
# index_20_predict <- bind_rows(index_20_RNN_predict_1, index_20_RNN_predict_2, index_20_RNN_predict_3, index_20_RNN_predict_4)
# write.csv(index_20_predict, "index_20_predict.csv")
# index_21_predict <- bind_rows(index_21_RNN_predict_1, index_21_RNN_predict_2)
# write.csv(index_21_predict, "index_21_predict.csv")


index_1_predict <- bind_rows(index_1_RNN_predict_1)
write.csv(index_1_predict, "index_1_predict.csv")
index_2_predict <- bind_rows(index_2_RNN_predict_1, index_2_RNN_predict_2)
write.csv(index_2_predict, "index_2_predict.csv")
index_3_predict <- bind_rows(index_3_RNN_predict_1, index_3_RNN_predict_2)
write.csv(index_3_predict, "index_3_predict.csv")
# index_4_predict <- bind_rows(index_4_RNN_predict_1)
# write.csv(index_4_predict, "index_4_predict.csv")
index_5_predict <- bind_rows(index_5_RNN_predict_1)
write.csv(index_5_predict, "index_5_predict.csv")
index_6_predict <- bind_rows(index_6_RNN_predict_1)
write.csv(index_6_predict, "index_6_predict.csv")
index_7_predict <- bind_rows(index_7_RNN_predict_1)
write.csv(index_7_predict, "index_7_predict.csv")
index_8_predict <- bind_rows(index_8_RNN_predict_1, index_8_RNN_predict_2, index_8_RNN_predict_3, index_8_RNN_predict_4)
write.csv(index_8_predict, "index_8_predict.csv")
index_9_predict <- bind_rows(index_9_RNN_predict_1)
write.csv(index_9_predict, "index_9_predict.csv")
index_10_predict <- bind_rows(index_10_RNN_predict_1, index_10_RNN_predict_2)
write.csv(index_10_predict, "index_10_predict.csv")
# index_11_predict <- bind_rows(index_11_RNN_predict_1)
# write.csv(index_11_predict, "index_11_predict.csv")
index_12_predict <- bind_rows(index_12_RNN_predict_1)
write.csv(index_12_predict, "index_12_predict.csv")
index_13_predict <- bind_rows(index_13_RNN_predict_1, index_13_RNN_predict_2)
write.csv(index_13_predict, "index_13_predict.csv")
index_14_predict <- bind_rows(index_14_RNN_predict_1)
write.csv(index_14_predict, "index_14_predict.csv")
index_15_predict <- bind_rows(index_15_RNN_predict_1, index_15_RNN_predict_2)
write.csv(index_15_predict, "index_15_predict.csv")
index_16_predict <- bind_rows(index_16_RNN_predict_1, index_16_RNN_predict_2)
write.csv(index_16_predict, "index_16_predict.csv")
index_17_predict <- bind_rows(index_17_RNN_predict_1)
write.csv(index_17_predict, "index_17_predict.csv")
index_18_predict <- bind_rows(index_18_RNN_predict_1, index_18_RNN_predict_2, index_18_RNN_predict_3, index_18_RNN_predict_4, index_18_RNN_predict_5)
write.csv(index_18_predict, "index_18_predict.csv")
# index_19_predict <- bind_rows(index_19_RNN_predict_1)
# write.csv(index_19_predict, "index_19_predict.csv")
index_20_predict <- bind_rows(index_20_RNN_predict_1, index_20_RNN_predict_2, index_20_RNN_predict_3, index_20_RNN_predict_4)
write.csv(index_20_predict, "index_20_predict.csv")
index_21_predict <- bind_rows(index_21_RNN_predict_1, index_21_RNN_predict_2)
write.csv(index_21_predict, "index_21_predict.csv")


# 
# All_index_predict <- bind_rows(index_1_predict, index_2_predict, index_3_predict, index_4_predict, index_5_predict, index_6_predict,
#                                index_7_predict, index_8_predict, index_9_predict, index_10_predict, index_11_predict, index_12_predict,
#                                index_13_predict, index_14_predict, index_15_predict, index_16_predict, index_17_predict, index_18_predict,
#                                index_19_predict, index_20_predict, index_21_predict)
# write.csv(All_index_predict, "All_index_predict.csv")


All_index_predict <- bind_rows(index_1_predict, index_2_predict, index_3_predict, index_5_predict, index_6_predict,
                               index_7_predict, index_8_predict, index_9_predict, index_10_predict, index_12_predict,
                               index_13_predict, index_14_predict, index_15_predict, index_16_predict, index_17_predict, index_18_predict,
                               index_20_predict, index_21_predict)
write.csv(All_index_predict, "All_index_predict.csv")


end_max_min <- data.frame(max = apply(end_price[2:121], 2, max, na.rm = TRUE), min = apply(end_price[2:121], 2, min, na.rm = TRUE))


all <- fread("All_index_result.csv")

write.csv(end_max_min, "end_max_min.csv")








# data_View
  # 
  # 
  # ggplot(data = data_table, aes(x=time, y=apple))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
  # ggplot(data = data, aes(x=data[,1], y=data[,2], group = 1))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
  # ggplot(data = data, aes(x=data[,1], y=data[,3]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
  # ggplot(data = data, aes(x=data[,1], y=data[,4]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
  # ggplot(data = data, aes(x=data[,1], y=data[,5]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
  # ggplot(data = data, aes(x=data[,1], y=data[,6]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
  # ggplot(data = data, aes(x=data[,1], y=data[,7]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
  # ggplot(data = data, aes(x=data[,1], y=data[,8]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
  # ggplot(data = data, aes(x=data[,1], y=data[,9]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
  # ggplot(data = data, aes(x=data[,1], y=data[,10]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
  # ggplot(data = data, aes(x=data[,1], y=data[,11]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
  # ggplot(data = data, aes(x=data[,1], y=data[,12]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
  # ggplot(data = data, aes(x=data[,1], y=data[,13]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
  # ggplot(data = data, aes(x=data[,1], y=data[,14]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
  # ggplot(data = data, aes(x=data[,1], y=data[,15]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
  # ggplot(data = data, aes(x=data[,1], y=data[,16]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
  # ggplot(data = data, aes(x=data[,1], y=data[,17]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
  # ggplot(data = data, aes(x=data[,1], y=data[,18]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
  # ggplot(data = data, aes(x=data[,1], y=data[,19]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
  # ggplot(data = data, aes(x=data[,1], y=data[,20]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
  # ggplot(data = data, aes(x=data[,1], y=data[,21]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
  # ggplot(data = data, aes(x=data[,1], y=data[,22]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
  # ggplot(data = data, aes(x=data[,1], y=data[,23]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
  # ggplot(data = data, aes(x=data[,1], y=data[,24]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
  # ggplot(data = data, aes(x=data[,1], y=data[,25]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
  # ggplot(data = data, aes(x=data[,1], y=data[,26]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
  # ggplot(data = data, aes(x=data[,1], y=data[,27]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
  # ggplot(data = data, aes(x=data[,1], y=data[,28]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
  # ggplot(data = data, aes(x=data[,1], y=data[,29]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
  # ggplot(data = data, aes(x=data[,1], y=data[,30]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
  # ggplot(data = data, aes(x=data[,1], y=data[,31]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
  # ggplot(data = data, aes(x=data[,1], y=data[,32]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
  # ggplot(data = data, aes(x=data[,1], y=data[,33]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
  # ggplot(data = data, aes(x=data[,1], y=data[,34]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
  # ggplot(data = data, aes(x=data[,1], y=data[,35]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
  # ggplot(data = data, aes(x=data[,1], y=data[,36]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
  # ggplot(data = data, aes(x=data[,1], y=data[,37]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
  # ggplot(data = data, aes(x=data[,1], y=data[,38]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
  # ggplot(data = data, aes(x=data[,1], y=data[,39]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
  # ggplot(data = data, aes(x=data[,1], y=data[,40]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
  # 
  # ggplot(data = data, aes(x=data[,1], y=data[,94]))+geom_point()+geom_line()+theme(axis.text.x = element_text(angle = 45, size = 5))
  # ggplot(data = data, aes(x=data[,1], y=data[,94]))+geom_line()+theme(axis.text.x = element_text(angle = 45, size = 5))

################################################################################
############ item-resource correlation analysis_by GooYoung ####################
################################################################################

resource <- fread("resource.csv")
resource$time <- substr(resource$time,1,7)
resource$time <- gsub("-","/",resource$time)
resource <- as.data.frame(resource)

### resource index 추가, 안쓰는 resource 제거
Material <- resource[c(1:40,80,81)]
Material_code <- data.frame("Material_name" = colnames(Material[-1]),
                            "codeNum" = 1:41)

Material_end_price <- merge(x=Material,y=end_price,by="time",all=TRUE)
# View(resource_end_price)

### rcorr(as.matrix(resource_item[,-c(1)]), type="pearson")$r
Material_end_price_cor <- data.frame(rcorr(as.matrix(Material_end_price[-1]), type="pearson")$r)[1:41]
Material_end_price_P <- data.frame(rcorr(as.matrix(Material_end_price[-1]), type="pearson")$P)[1:41]
Material_end_price_cor <- as.data.frame(t(Material_end_price_cor[-c(1:41),]))
Material_end_price_P <- as.data.frame(t(Material_end_price_P[-c(1:41),]))

### function cut off 1 : 원자재 - 종목 상관관계 절대값 매트릭스 (p, r)
Material_end_price_abs_cor <- as.data.frame(matrix(nrow = 41, ncol = 120))
rownames(Material_end_price_abs_cor) <- rownames(Material_end_price_cor)
colnames(Material_end_price_abs_cor) <- item_strsplit$X1

abs_cut_matrix <- function(p,r){
  for(i in 1:length(Material_end_price_P$end_price_AAPL.UW.Equity))(
    for(j in 1:length(Material_end_price_P))(
      if (Material_end_price_P[i,j]>p|abs(Material_end_price_cor[i,j])<r)(
        Material_end_price_abs_cor[i,j] <<- 0
      ) else (
        Material_end_price_abs_cor[i,j] <<- abs(Material_end_price_cor[i,j])
      )
    )
  )
}
# cut off value (p,r) 입력하여 절댓값 매트릭스 만들기
abs_cut_matrix(0.05,0.7)
# View(Material_end_price_abs_cor)
########### k-means clustering ##############

# calculate sum of square function
wssplot <- function(data, nc=30, seed = 1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
}

### 절대값 매트릭스

wssplot(t(Material_end_price_abs_cor))
set.seed(1234)
fit.km <- kmeans(t(Material_end_price_abs_cor), 21, nstart = 25)
fit.km$tot.withinss
fit.km$cluster

### 각 군집별 변수의 요약값 (mean)
Mean_cor_of_cluster <- aggregate(t(Material_end_price_abs_cor), by=list(cluster=fit.km$cluster), mean)
### View(aggregate(t(Material_end_price_abs_cor), by=list(cluster=fit.km_21$cluster), mean))
write.csv(Mean_cor_of_cluster, file = "cluster_mean.csv")


### 각 군집에 포함되는 종목 csv파일
sort(fit.km$cluster)
write.csv(sort(fit.km$cluster), file = "cluster.csv")

### 군집별로 고려해야 할 원자재는 ?

### cut-off 2

what_Material_to_cluster <- as.data.frame(matrix(nrow = length(unique(fit.km$cluster)),ncol = 41))   # 행 갯수 = 군집 수 / 열 갯수 = 원자재 갯수
rownames(what_Material_to_cluster) <- sort(unique(fit.km$cluster))
colnames(what_Material_to_cluster) <- Material_code$Material_name

Material_r_cut <- function(x){
  for (i in 1:21){
    for (j in 2:length(Mean_cor_of_cluster)){
      if(Mean_cor_of_cluster[i,j]>x){
        what_Material_to_cluster[i,j-1] <<- 1
      } else {
        what_Material_to_cluster[i,j-1] <<- 0
      } 
    }
  }
}

# cut-off 2. 0을 포함하지 않은 셀 별 평균 상관계수
# 군집별 0 보다 큰 원자재 상관계수 갯수(r>0) = 286 개
# sum(data.frame("count"=apply(t(Mean_cor_of_cluster), 2, function(x){result <<- sum(abs(x)>0)})))
# 군집별 원자재 상관계수의 총 합 = sum(r) =  177.8258
# sum(Mean_cor_of_cluster[-1])
# 각 셀별 평균 상관계수 = 177.8258 / 286 = 0.6217685

### cut하고 싶은 value 입력 - 지금은 0이 아닌 셀들의 평균 입력함
Material_r_cut(0.6217685)
# View(what_Material_to_cluster)
# View(data.frame(apply(what_Material_to_cluster, 2, function(x){sum(x)})))
##### 군집별 종목 - 원자재 

### 1. 군집 index 추가
item_names <- cbind(item_names, as.data.frame(fit.km$cluster))
colnames(item_names)[4] <- "cluster"

### 2. 군집 - 종목 - 원자재 list
cluster_item_Material <- list()
for(i in 1:21){
  cluster_item_Material[[i]] <- list(item = which(fit.km$cluster == i),
                                     material = which(what_Material_to_cluster[i,] == 1))
}
cluster_item_Material
cluster_item_Material[[13]][2]
cluster_item_Material[[21]][1]
cluster_item_Material[[21]][2]


for (i in 1:length(Material_code$codeNum)){
  assign(paste("Material",i,sep = "_"),
         as.data.frame(matrix(rep(unlist(Material[i+1]),120), ncol = 120, nrow = 204,
                              dimnames = list(1:204, as.character(item_strsplit$X1)))))
}

for (i in 1:length(Material_code$codeNum)){
  assign(paste("Material",i,"normalize", sep = "_"),
         as.data.frame(apply(get(paste("Material",i,sep = "_")),2,normalize)))
}

# 원자재 변수들 NA -> 0 변환
for(k in 1:length(Material_code$codeNum)){
  b <- paste("Material",k,"normalize", sep = "_")
  a <- get(paste("Material",k,"normalize", sep = "_"))
  for(i in 1:120){
    for(j in 1:203){
      if(is.na(a[j,i])){
        a[j,i] <- 0
      }
    }
  }
  assign(b, a)
  print(paste(k,"번쨰 완료"))
}


cluster1_resource <- which(what_resource_to_cluster[1,] == 1)
cluster2_resource <- which(what_resource_to_cluster[2,] == 1)
cluster3_resource <- which(what_resource_to_cluster[3,] == 1)
cluster4_resource <- which(what_resource_to_cluster[4,] == 1)
cluster5_resource <- which(what_resource_to_cluster[5,] == 1)
cluster6_resource <- which(what_resource_to_cluster[6,] == 1)
cluster7_resource <- which(what_resource_to_cluster[7,] == 1)
cluster8_resource <- which(what_resource_to_cluster[8,] == 1)
cluster9_resource <- which(what_resource_to_cluster[9,] == 1)
cluster10_resource <- which(what_resource_to_cluster[10,] == 1)
cluster11_resource <- which(what_resource_to_cluster[11,] == 1)
cluster12_resource <- which(what_resource_to_cluster[12,] == 1)
cluster13_resource <- which(what_resource_to_cluster[13,] == 1)
cluster14_resource <- which(what_resource_to_cluster[14,] == 1)
cluster15_resource <- which(what_resource_to_cluster[15,] == 1)
cluster16_resource <- which(what_resource_to_cluster[16,] == 1)
cluster17_resource <- which(what_resource_to_cluster[17,] == 1)
cluster18_resource <- which(what_resource_to_cluster[18,] == 1)
cluster19_resource <- which(what_resource_to_cluster[19,] == 1)
cluster20_resource <- which(what_resource_to_cluster[20,] == 1)
cluster21_resource <- which(what_resource_to_cluster[21,] == 1)

cluster1_item <- which(fit.km_21$cluster == 1)
cluster2_item <- which(fit.km_21$cluster == 2)
cluster3_item <- which(fit.km_21$cluster == 3)
cluster4_item <- which(fit.km_21$cluster == 4)
cluster5_item <- which(fit.km_21$cluster == 5)
cluster6_item <- which(fit.km_21$cluster == 6)
cluster7_item <- which(fit.km_21$cluster == 7)
cluster8_item <- which(fit.km_21$cluster == 8)
cluster9_item <- which(fit.km_21$cluster == 9)
cluster10_item <- which(fit.km_21$cluster == 10)
cluster11_item <- which(fit.km_21$cluster == 11)
cluster12_item <- which(fit.km_21$cluster == 12)
cluster13_item <- which(fit.km_21$cluster == 13)
cluster14_item <- which(fit.km_21$cluster == 14)
cluster15_item <- which(fit.km_21$cluster == 15)
cluster16_item <- which(fit.km_21$cluster == 16)
cluster17_item <- which(fit.km_21$cluster == 17)
cluster18_item <- which(fit.km_21$cluster == 18)
cluster19_item <- which(fit.km_21$cluster == 19)
cluster20_item <- which(fit.km_21$cluster == 20)
cluster21_item <- which(fit.km_21$cluster == 21)

cluster01_end_price <- t(end_price_normalize[cluster01]) # y
cluster02_end_price <- t(end_price_normalize[cluster02])


cluster01_resource <- t(end_price_normalize[cluster01]) # y
# resource_item_count$summary <- apply(resource_end_price_cor,1,function(x){summary(x)})
# apply(resource_end_price_cor,1,function(x){summary(x)})
# apply(resource_end_price_cor,2,function(x){head(order(x))})
# apply(resource_end_price_cor,2,function(x){head(order(x,decreasing = FALSE,na.last = TRUE))})
# 
# View(resource_item_count)
# View(resource_end_price_cor)
# 
# ggplot(data = resource, aes(x=resource[,1], y=resource[,2]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
# ggplot(data = resource, aes(x=resource[,1], y=resource[,3]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
# ggplot(data = resource, aes(x=resource[,1], y=resource[,4]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
# ggplot(data = resource, aes(x=resource[,1], y=resource[,5]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
# ggplot(data = resource, aes(x=resource[,1], y=resource[,6]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
# ggplot(data = resource, aes(x=resource[,1], y=resource[,7]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))
# ggplot(data = resource, aes(x=resource[,1], y=resource[,8]))+geom_point()+theme(axis.text.x = element_text(angle = 45, size = 5))