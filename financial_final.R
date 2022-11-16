setwd("/Users/tengyuan/financial")
source("function_FDA.R")
setwd("/Users/tengyuan/financial/final")
#-------
data_1216<-read.table("1216.TW.csv", sep= ",", na.strings = "null", header = T)
data_1301<-read.table("1301.TW.csv", sep= ",", na.strings = "null", header = T)
data_1303<-read.table("1303.TW.csv", sep= ",", na.strings = "null", header = T)
data_2002<-read.table("2002.TW.csv", sep= ",", na.strings = "null", header = T)
data_2303<-read.table("2303.TW.csv", sep= ",", na.strings = "null", header = T)
data_2308<-read.table("2308.TW.csv", sep= ",", na.strings = "null", header = T)
data_2317<-read.table("2317.TW.csv", sep= ",", na.strings = "null", header = T)
data_2330<-read.table("2330.TW.csv", sep= ",", na.strings = "null", header = T)
data_2412<-read.table("2412.TW.csv", sep= ",", na.strings = "null", header = T)
data_2454<-read.table("2454.TW.csv", sep= ",", na.strings = "null", header = T)
data_2603<-read.table("2603.TW.csv", sep= ",", na.strings = "null", header = T)
data_2881<-read.table("2881.TW.csv", sep= ",", na.strings = "null", header = T)
data_2882<-read.table("2882.TW.csv", sep= ",", na.strings = "null", header = T)
data_2883<-read.table("2883.TW.csv", sep= ",", na.strings = "null", header = T)
data_2884<-read.table("2884.TW.csv", sep= ",", na.strings = "null", header = T)
data_2886<-read.table("2886.TW.csv", sep= ",", na.strings = "null", header = T)
data_2891<-read.table("2891.TW.csv", sep= ",", na.strings = "null", header = T)
data_3037<-read.table("3037.TW.csv", sep= ",", na.strings = "null", header = T)
data_3711<-read.table("3711.TW.csv", sep= ",", na.strings = "null", header = T)
data_5871<-read.table("5871.TW.csv", sep= ",", na.strings = "null", header = T)

data_1216[,1]<-as.Date(data_1216[,1])
data_1301[,1]<-as.Date(data_1301[,1])
data_1303[,1]<-as.Date(data_1303[,1])
data_2002[,1]<-as.Date(data_2002[,1])
data_2303[,1]<-as.Date(data_2303[,1])
data_2308[,1]<-as.Date(data_2308[,1])
data_2317[,1]<-as.Date(data_2317[,1])
data_2330[,1]<-as.Date(data_2330[,1])
data_2412[,1]<-as.Date(data_2412[,1])
data_2454[,1]<-as.Date(data_2454[,1])
data_2603[,1]<-as.Date(data_2603[,1])
data_2881[,1]<-as.Date(data_2881[,1])
data_2882[,1]<-as.Date(data_2882[,1])
data_2883[,1]<-as.Date(data_2883[,1])
data_2884[,1]<-as.Date(data_2884[,1])
data_2886[,1]<-as.Date(data_2886[,1])
data_2891[,1]<-as.Date(data_2891[,1])
data_3037[,1]<-as.Date(data_3037[,1])
data_3711[,1]<-as.Date(data_3711[,1])
data_5871[,1]<-as.Date(data_5871[,1])

NA_rep<-function(x, colx){ 
  
  ind_miss<-which(is.na(x[, colx]))  
  
  for(i in ind_miss){           
    x[i, colx]<-x[(i-1), colx]
    
  }
  
  return(x)
  
}

data_1216<-NA_rep(data_1216, 6) #把Adj.Close用前一天的填上
data_1301<-NA_rep(data_1301, 6)
data_1303<-NA_rep(data_1303, 6)
data_2002<-NA_rep(data_2002, 6)
data_2303<-NA_rep(data_2303, 6)
data_2308<-NA_rep(data_2308, 6)
data_2317<-NA_rep(data_2317, 6)
data_2330<-NA_rep(data_2330, 6)
data_2412<-NA_rep(data_2412, 6)
data_2454<-NA_rep(data_2454, 6)
data_2603<-NA_rep(data_2603, 6)
data_2881<-NA_rep(data_2881, 6)
data_2882<-NA_rep(data_2882, 6)
data_2883<-NA_rep(data_2883, 6)
data_2884<-NA_rep(data_2884, 6)
data_2886<-NA_rep(data_2886, 6)
data_2891<-NA_rep(data_2891, 6)
data_3037<-NA_rep(data_3037, 6)
data_3711<-NA_rep(data_3711, 6)
data_5871<-NA_rep(data_5871, 6)

#報酬率---------
data_1216$ret<-c(NA, retx(as.numeric(data_1216$Adj.Close)))
data_1301$ret<-c(NA, retx(as.numeric(data_1301$Adj.Close)))
data_1303$ret<-c(NA, retx(as.numeric(data_1303$Adj.Close)))
data_2002$ret<-c(NA, retx(as.numeric(data_2002$Adj.Close)))
data_2303$ret<-c(NA, retx(as.numeric(data_2303$Adj.Close)))
data_2308$ret<-c(NA, retx(as.numeric(data_2308$Adj.Close)))
data_2317$ret<-c(NA, retx(as.numeric(data_2317$Adj.Close)))
data_2330$ret<-c(NA, retx(as.numeric(data_2330$Adj.Close)))
data_2412$ret<-c(NA, retx(as.numeric(data_2412$Adj.Close)))
data_2454$ret<-c(NA, retx(as.numeric(data_2454$Adj.Close)))
data_2603$ret<-c(NA, retx(as.numeric(data_2603$Adj.Close)))
data_2881$ret<-c(NA, retx(as.numeric(data_2881$Adj.Close)))
data_2882$ret<-c(NA, retx(as.numeric(data_2882$Adj.Close)))
data_2883$ret<-c(NA, retx(as.numeric(data_2883$Adj.Close)))
data_2884$ret<-c(NA, retx(as.numeric(data_2884$Adj.Close)))
data_2886$ret<-c(NA, retx(as.numeric(data_2886$Adj.Close)))
data_2891$ret<-c(NA, retx(as.numeric(data_2891$Adj.Close)))
data_3037$ret<-c(NA, retx(as.numeric(data_3037$Adj.Close)))
data_3711$ret<-c(NA, retx(as.numeric(data_3711$Adj.Close)))
data_5871$ret<-c(NA, retx(as.numeric(data_5871$Adj.Close)))

data_ret<-data.frame(matrix(0, nrow(data_1216), 21))  
data_ret[,1]<-data_1216$Date
data_ret[,2:21]<-cbind(data_1216$ret, data_1301$ret, data_1303$ret, data_2002$ret,
                       data_2303$ret, data_2308$ret, data_2317$ret, data_2330$ret,
                       data_2412$ret, data_2454$ret, data_2603$ret, data_2881$ret,
                       data_2882$ret, data_2883$ret, data_2884$ret, data_2886$ret,
                       data_2891$ret, data_3037$ret, data_3711$ret, data_5871$ret)
colnames(data_ret)<-c("Date", "x1216", "x1301", "x1303", "x2002",
                      "x2303", "x2308", "x2317", "x2330",
                      "x2412", "x2454", "x2603", "x2881",
                      "x2882", "x2883", "x2884", "x2886",
                      "x2891", "x3037", "x3711", "x5871")
head(data_ret) #20支股票的報酬率
data_ret <- data_ret[-1,]

## 表1 個別資產（年化）報酬之敘述統計量（單位：%）---------
# summary_<-rbind(apply(data_ret[,2:21],2, summary),
#                      apply(data_ret[,2:21],2, sd),
#                      apply(data_ret[,2:21],2, my_skewness),
#                      apply(data_ret[,2:21],2, my_kurtosis),
#                      apply(data_ret[,2:21],2, my_acf1)
# )
# rownames(summary_)[7:nrow(summary_)]<-c("Std.","SKewness","Kurtosis","ACF1")
# summary_<-t(summary_) #轉置
# summary_<-data.frame(summary_)
# summary_[,1:10]<-summary_[,1:10]*c(rep(25200,6),sqrt(252)*100,1/sqrt(252),1/252,1) #年化
# round(summary_,3)
# write.table(summary_, "summary1.csv", sep = ",") #輸出csv

#
s <- round(apply(data_ret[,-1]*100,2,summary)*252,3)
Std. <- round(apply(data_ret[,-1]*100, 2, sd)*sqrt(252),3)
SKewness <- round(apply(data_ret[,-1]*100, 2, my_skewness)/sqrt(252),3)
Kurtosis <- round(apply(data_ret[,-1]*100, 2, my_kurtosis)/252,3)
ACF1 <- round(apply(data_ret[,-1]*100, 2, my_acf1),3)
SUMMARY <- rbind(s,Std.,SKewness,Kurtosis,ACF1)
SUMMARY <- data.frame(SUMMARY)
SUMMARY <- t(SUMMARY)
write.table(SUMMARY, "summary.csv", sep = ",") #輸出csv

##calculate gross (cumulative) returns---------
data_1216$cum.ret<-c(NA, cumprod((1+data_1216$ret[-1])))
data_1301$cum.ret<-c(NA, cumprod((1+data_1301$ret[-1])))
data_1303$cum.ret<-c(NA, cumprod((1+data_1303$ret[-1])))
data_2002$cum.ret<-c(NA, cumprod((1+data_2002$ret[-1])))
data_2303$cum.ret<-c(NA, cumprod((1+data_2303$ret[-1])))
data_2308$cum.ret<-c(NA, cumprod((1+data_2308$ret[-1])))
data_2317$cum.ret<-c(NA, cumprod((1+data_2317$ret[-1])))
data_2330$cum.ret<-c(NA, cumprod((1+data_2330$ret[-1])))
data_2412$cum.ret<-c(NA, cumprod((1+data_2412$ret[-1])))
data_2454$cum.ret<-c(NA, cumprod((1+data_2454$ret[-1])))
data_2603$cum.ret<-c(NA, cumprod((1+data_2603$ret[-1])))
data_2881$cum.ret<-c(NA, cumprod((1+data_2881$ret[-1])))
data_2882$cum.ret<-c(NA, cumprod((1+data_2882$ret[-1])))
data_2883$cum.ret<-c(NA, cumprod((1+data_2883$ret[-1])))
data_2884$cum.ret<-c(NA, cumprod((1+data_2884$ret[-1])))
data_2886$cum.ret<-c(NA, cumprod((1+data_2886$ret[-1])))
data_2891$cum.ret<-c(NA, cumprod((1+data_2891$ret[-1])))
data_3037$cum.ret<-c(NA, cumprod((1+data_3037$ret[-1])))
data_3711$cum.ret<-c(NA, cumprod((1+data_3711$ret[-1])))
data_5871$cum.ret<-c(NA, cumprod((1+data_5871$ret[-1])))

data_cum.ret<-data.frame(matrix(0, nrow(data_1216), 21))  
data_cum.ret[,1]<-data_1216$Date
data_cum.ret[,2:21]<-cbind(data_1216$cum.ret, data_1301$cum.ret, data_1303$cum.ret, data_2002$cum.ret,
                       data_2303$cum.ret, data_2308$cum.ret, data_2317$cum.ret, data_2330$cum.ret,
                       data_2412$cum.ret, data_2454$cum.ret, data_2603$cum.ret, data_2881$cum.ret,
                       data_2882$cum.ret, data_2883$cum.ret, data_2884$cum.ret, data_2886$cum.ret,
                       data_2891$cum.ret, data_3037$cum.ret, data_3711$cum.ret, data_5871$cum.ret)
colnames(data_cum.ret)<-c("Date", "x1216", "x1301", "x1303", "x2002",
                      "x2303", "x2308", "x2317", "x2330",
                      "x2412", "x2454", "x2603", "x2881",
                      "x2882", "x2883", "x2884", "x2886",
                      "x2891", "x3037", "x3711", "x5871")
head(data_cum.ret) #20支股票的累積報酬率
data_cum.ret <- data_cum.ret[-1,]


##plot returns---------
showtext_auto(enable = TRUE)
par(mfrow = c(2,2))

r.range<-range(data_ret[,2:21])
plot(x =data_1216$Date, y = data_1216$ret, main = "統一報酬率\nJan-03-2012 - Dec-30-2021",
     xlab = "Date", ylab = "Return", type="l", lwd = 1, col = "gray",
     ylim = r.range)
plot(x =data_1301$Date, y = data_1301$ret, main = "台塑報酬率\nJan-03-2012 - Dec-30-2021",xlab = "Date", ylab = "Return", type="l", lwd = 1, col = "gray",ylim = r.range)
plot(x =data_1303$Date, y = data_1303$ret, main = "南亞報酬率\nJan-03-2012 - Dec-30-2021",xlab = "Date", ylab = "Return", type="l", lwd = 1, col = "gray",ylim = r.range)
plot(x =data_2002$Date, y = data_2002$ret, main = "中鋼報酬率\nJan-03-2012 - Dec-30-2021",xlab = "Date", ylab = "Return", type="l", lwd = 1, col = "gray",ylim = r.range)
plot(x =data_2303$Date, y = data_2303$ret, main = "聯電報酬率\nJan-03-2012 - Dec-30-2021",xlab = "Date", ylab = "Return", type="l", lwd = 2, col = "gray",ylim = r.range)
plot(x =data_2308$Date, y = data_2308$ret, main = "台達電報酬率\nJan-03-2012 - Dec-30-2021",xlab = "Date", ylab = "Return", type="l", lwd = 2, col = "gray",ylim = r.range)
plot(x =data_2317$Date, y = data_2317$ret, main = "鴻海報酬率\nJan-03-2012 - Dec-30-2021",xlab = "Date", ylab = "Return", type="l", lwd = 2, col = "gray",ylim = r.range)
plot(x =data_2330$Date, y = data_2330$ret, main = "台積電報酬率\nJan-03-2012 - Dec-30-2021",xlab = "Date", ylab = "Return", type="l", lwd = 2, col = "gray",ylim = r.range)
plot(x =data_2412$Date, y = data_2412$ret, main = "中華電報酬率\nJan-03-2012 - Dec-30-2021",xlab = "Date", ylab = "Return", type="l", lwd = 2, col = "gray",ylim = r.range)
plot(x =data_2454$Date, y = data_2454$ret, main = "聯發科報酬率\nJan-03-2012 - Dec-30-2021",xlab = "Date", ylab = "Return", type="l", lwd = 2, col = "gray",ylim = r.range)
plot(x =data_2603$Date, y = data_2603$ret, main = "長榮報酬率\nJan-03-2012 - Dec-30-2021",xlab = "Date", ylab = "Return", type="l", lwd = 2, col = "gray",ylim = r.range)
plot(x =data_2881$Date, y = data_2881$ret, main = "富邦金報酬率\nJan-03-2012 - Dec-30-2021",xlab = "Date", ylab = "Return", type="l", lwd = 2, col = "gray",ylim = r.range)
plot(x =data_2882$Date, y = data_2882$ret, main = "國泰金報酬率\nJan-03-2012 - Dec-30-2021",xlab = "Date", ylab = "Return", type="l", lwd = 2, col = "gray",ylim = r.range)
plot(x =data_2883$Date, y = data_2883$ret, main = "開發金報酬率\nJan-03-2012 - Dec-30-2021",xlab = "Date", ylab = "Return", type="l", lwd = 2, col = "gray",ylim = r.range)
plot(x =data_2884$Date, y = data_2884$ret, main = "玉山金報酬率\nJan-03-2012 - Dec-30-2021",xlab = "Date", ylab = "Return", type="l", lwd = 2, col = "gray",ylim = r.range)
plot(x =data_2886$Date, y = data_2886$ret, main = "兆豐金報酬率\nJan-03-2012 - Dec-30-2021",xlab = "Date", ylab = "Return", type="l", lwd = 2, col = "gray",ylim = r.range)
plot(x =data_2891$Date, y = data_2891$ret, main = "中信金報酬率\nJan-03-2012 - Dec-30-2021",xlab = "Date", ylab = "Return", type="l", lwd = 2, col = "gray",ylim = r.range)
plot(x =data_3037$Date, y = data_3037$ret, main = "欣興報酬率\nJan-03-2012 - Dec-30-2021",xlab = "Date", ylab = "Return", type="l", lwd = 2, col = "gray",ylim = r.range)
plot(x =data_3711$Date, y = data_3711$ret, main = "日月光投控報酬率\nJan-03-2012 - Dec-30-2021",xlab = "Date", ylab = "Return", type="l", lwd = 2, col = "gray",ylim = r.range)
plot(x =data_5871$Date, y = data_5871$ret, main = "中租-KY 報酬率\nJan-03-2012 - Dec-30-2021",xlab = "Date", ylab = "Return", type="l", lwd = 2, col = "gray",ylim = r.range)


##plot cumulative gross returns---------
par(mfrow = c(2,2))

cr.range<-range(data_cum.ret[,2:20])
cr.range1<-range(data_cum.ret[,21])
plot(x =data_cum.ret$Date, y = data_cum.ret$x1216, main = "x1216 Cumulative Gross Return\nJan-03-2012 - Dec-30-2021",xlab = "Date", ylab = "Cumulative Gross Return", type="l", lwd = 2, col = "gray",ylim = cr.range)
plot(x =data_cum.ret$Date, y = data_cum.ret$x1301, main = "x1301 Cumulative Gross Return\nJan-03-2012 - Dec-30-2021",xlab = "Date", ylab = "Cumulative Gross Return", type="l", lwd = 2, col = "gray",ylim = cr.range)
plot(x =data_cum.ret$Date, y = data_cum.ret$x1303, main = "x1303 Cumulative Gross Return\nJan-03-2012 - Dec-30-2021",xlab = "Date", ylab = "Cumulative Gross Return", type="l", lwd = 2, col = "gray",ylim = cr.range)
plot(x =data_cum.ret$Date, y = data_cum.ret$x2002, main = "x2002 Cumulative Gross Return\nJan-03-2012 - Dec-30-2021",xlab = "Date", ylab = "Cumulative Gross Return", type="l", lwd = 2, col = "gray",ylim = cr.range)
plot(x =data_cum.ret$Date, y = data_cum.ret$x2303, main = "x2303 Cumulative Gross Return\nJan-03-2012 - Dec-30-2021",xlab = "Date", ylab = "Cumulative Gross Return", type="l", lwd = 2, col = "gray",ylim = cr.range)
plot(x =data_cum.ret$Date, y = data_cum.ret$x2308, main = "x2308 Cumulative Gross Return\nJan-03-2012 - Dec-30-2021",xlab = "Date", ylab = "Cumulative Gross Return", type="l", lwd = 2, col = "gray",ylim = cr.range)
plot(x =data_cum.ret$Date, y = data_cum.ret$x2317, main = "x2317 Cumulative Gross Return\nJan-03-2012 - Dec-30-2021",xlab = "Date", ylab = "Cumulative Gross Return", type="l", lwd = 2, col = "gray",ylim = cr.range)
plot(x =data_cum.ret$Date, y = data_cum.ret$x2330, main = "x2330 Cumulative Gross Return\nJan-03-2012 - Dec-30-2021",xlab = "Date", ylab = "Cumulative Gross Return", type="l", lwd = 2, col = "gray",ylim = cr.range)
plot(x =data_cum.ret$Date, y = data_cum.ret$x2412, main = "x2412 Cumulative Gross Return\nJan-03-2012 - Dec-30-2021",xlab = "Date", ylab = "Cumulative Gross Return", type="l", lwd = 2, col = "gray",ylim = cr.range)
plot(x =data_cum.ret$Date, y = data_cum.ret$x2454, main = "x2454 Cumulative Gross Return\nJan-03-2012 - Dec-30-2021",xlab = "Date", ylab = "Cumulative Gross Return", type="l", lwd = 2, col = "gray",ylim = cr.range)
plot(x =data_cum.ret$Date, y = data_cum.ret$x2603, main = "x2603 Cumulative Gross Return\nJan-03-2012 - Dec-30-2021",xlab = "Date", ylab = "Cumulative Gross Return", type="l", lwd = 2, col = "gray",ylim = cr.range)
plot(x =data_cum.ret$Date, y = data_cum.ret$x2881, main = "x2881 Cumulative Gross Return\nJan-03-2012 - Dec-30-2021",xlab = "Date", ylab = "Cumulative Gross Return", type="l", lwd = 2, col = "gray",ylim = cr.range)
plot(x =data_cum.ret$Date, y = data_cum.ret$x2882, main = "x2882 Cumulative Gross Return\nJan-03-2012 - Dec-30-2021",xlab = "Date", ylab = "Cumulative Gross Return", type="l", lwd = 2, col = "gray",ylim = cr.range)
plot(x =data_cum.ret$Date, y = data_cum.ret$x2883, main = "x2883 Cumulative Gross Return\nJan-03-2012 - Dec-30-2021",xlab = "Date", ylab = "Cumulative Gross Return", type="l", lwd = 2, col = "gray",ylim = cr.range)
plot(x =data_cum.ret$Date, y = data_cum.ret$x2884, main = "x2884 Cumulative Gross Return\nJan-03-2012 - Dec-30-2021",xlab = "Date", ylab = "Cumulative Gross Return", type="l", lwd = 2, col = "gray",ylim = cr.range)
plot(x =data_cum.ret$Date, y = data_cum.ret$x2886, main = "x2886 Cumulative Gross Return\nJan-03-2012 - Dec-30-2021",xlab = "Date", ylab = "Cumulative Gross Return", type="l", lwd = 2, col = "gray",ylim = cr.range)
plot(x =data_cum.ret$Date, y = data_cum.ret$x2891, main = "x2891 Cumulative Gross Return\nJan-03-2012 - Dec-30-2021",xlab = "Date", ylab = "Cumulative Gross Return", type="l", lwd = 2, col = "gray",ylim = cr.range)
plot(x =data_cum.ret$Date, y = data_cum.ret$x3037, main = "x3037 Cumulative Gross Return\nJan-03-2012 - Dec-30-2021",xlab = "Date", ylab = "Cumulative Gross Return", type="l", lwd = 2, col = "gray",ylim = cr.range)
plot(x =data_cum.ret$Date, y = data_cum.ret$x3711, main = "x3711 Cumulative Gross Return\nJan-03-2012 - Dec-30-2021",xlab = "Date", ylab = "Cumulative Gross Return", type="l", lwd = 2, col = "gray",ylim = cr.range)
plot(x =data_cum.ret$Date, y = data_cum.ret$x5871, main = "x5871 Cumulative Gross Return\nJan-03-2012 - Dec-30-2021",xlab = "Date", ylab = "Cumulative Gross Return", type="l", lwd = 2, col = "gray",ylim = cr.range1)




price.data <- data.frame(matrix(0,nrow(data_1216),21))
price.data[,1] <- data_1216$Date
price.data[,2:ncol(price.data)] <- cbind(data_1216$Adj.Close,
                                         data_1301$Adj.Close,
                                         data_1303$Adj.Close,
                                         data_2002$Adj.Close,
                                         data_2303$Adj.Close,
                                         data_2308$Adj.Close,
                                         data_2317$Adj.Close,
                                         data_2330$Adj.Close,
                                         data_2412$Adj.Close,
                                         data_2454$Adj.Close,
                                         data_2603$Adj.Close,
                                         data_2881$Adj.Close,
                                         data_2882$Adj.Close,
                                         data_2883$Adj.Close,
                                         data_2884$Adj.Close,
                                         data_2886$Adj.Close,
                                         data_2891$Adj.Close,
                                         data_3037$Adj.Close,
                                         data_3711$Adj.Close,
                                         data_5871$Adj.Close)
colnames(price.data) <- c('Date',"x1216","x1301","x1303","x2002","x2303",
                          "x2308","x2317","x2330","x2412","x2454","x2603",
                          "x2881","x2882","x2883","x2884","x2886","x2891",
                          "x3037","x3711","x5871")
price.data <- price.data[-1,]
price.data[,-1] <- apply(price.data[,-1],2,as.numeric)
#### 投資組合 ####-------
library(quadprog)


WX1 <- matrix(0, hx+1, ncol(data_ret)-1)
WX2 <- matrix(0, hx+1, ncol(data_ret)-1)
WX3 <- matrix(0, hx+1, ncol(data_ret)-1)
WX4 <- matrix(0, hx+1, ncol(data_ret)-1)
WX5 <- matrix(0, hx+1, ncol(data_ret)-1)
WX6 <- matrix(0, hx+1, ncol(data_ret)-1)
WX7 <- matrix(0, hx+1, ncol(data_ret)-1)


por_ret1 <- numeric(hx)
por_ret2 <- numeric(hx)
por_ret3 <- numeric(hx)
por_ret4 <- numeric(hx)
por_ret5 <- numeric(hx)
por_ret6 <- numeric(hx)
por_ret7 <- numeric(hx)

por_netrx1<-numeric(hx)
por_netrx2<-numeric(hx)
por_netrx3<-numeric(hx)
por_netrx4<-numeric(hx)
por_netrx5<-numeric(hx)
por_netrx6<-numeric(hx)
por_netrx7<-numeric(hx)

tor1<-numeric(hx)
tor2<-numeric(hx)
tor3<-numeric(hx)
tor4<-numeric(hx)
tor5<-numeric(hx)
tor6<-numeric(hx)
tor7<-numeric(hx)

hhi1<-numeric(hx)
hhi2<-numeric(hx)
hhi3<-numeric(hx)
hhi4<-numeric(hx)
hhi5<-numeric(hx)
hhi6<-numeric(hx)
hhi7<-numeric(hx)

slr1<-numeric(hx)
slr2<-numeric(hx)
slr3<-numeric(hx)
slr4<-numeric(hx)
slr5<-numeric(hx)
slr6<-numeric(hx)
slr7<-numeric(hx)


#

aaa = ABC_mvp(data_ret[,-1])
aaa$B/aaa$C #要求報酬率

kx <- 252*2
hx <- nrow(data_ret) - kx
epx<-3.5/1000 ##transaction cost：交易成本（台股固定手需費）
mu_targ<-0.08/100 ##daily return, 0.07%
rf <- 0.025/360 ##（年化的）無風險報酬率（不是％）

for (i in 1:hx){
  datax = data_ret[i:(i+kx-1),2:ncol(data_ret)]
  datap = price.data[i:(i+kx-1),2:ncol(price.data)]
  mu_targ = mean(apply(datax,2,mean))
  
  rx = data_ret[i+kx, 2:ncol(data_ret)]
  rx_lag = datax[nrow(datax),]
  price = as.numeric(datap[nrow(datap),])
  
  
  wx1 = as.vector(mvp_wx(datax, mu_targ)) #mvp
  wx2 = round(nsmvp_wx_quad(datax,mu_targ)$solution,8) #nsmvp
  wx3 = round(nsgmvp_wx_quad(datax)$solution, 8) #nsgmvp
  wx4 = gmvp_wx(datax) #gmvp
  wx5 = tan_wx(datax, rf) #tangency
  wx6 = price/sum(price) #price weighted
  wx7 = 1/ncol(datax) #fixed weighted
  
  
  # individual turnover rate
  tor1_ind = wx1 - WX1[i,]*(1 + rx_lag)/(1 + sum(WX1[i,]*rx_lag))
  tor2_ind = wx2 - WX2[i,]*(1 + rx_lag)/(1 + sum(WX2[i,]*rx_lag))
  tor3_ind = wx3 - WX3[i,]*(1 + rx_lag)/(1 + sum(WX3[i,]*rx_lag))
  tor4_ind = wx4 - WX4[i,]*(1 + rx_lag)/(1 + sum(WX4[i,]*rx_lag))
  tor5_ind = wx5 - WX5[i,]*(1 + rx_lag)/(1 + sum(WX5[i,]*rx_lag))
  tor6_ind = wx6 - WX6[i,]*(1 + rx_lag)/(1 + sum(WX6[i,]*rx_lag))
  tor7_ind = wx7 - WX7[i,]*(1 + rx_lag)/(1 + sum(WX7[i,]*rx_lag))
  
  # portfolio turnover rate
  tor1[i] = sum(abs(tor1_ind))
  tor2[i] = sum(abs(tor2_ind))
  tor3[i] = sum(abs(tor3_ind))
  tor4[i] = sum(abs(tor4_ind))
  tor5[i] = sum(abs(tor5_ind))
  tor6[i] = sum(abs(tor6_ind))
  tor7[i] = sum(abs(tor7_ind))
  
  # portfolio Return
  por_ret1[i] = sum(wx1*rx)
  por_ret2[i] = sum(wx2*rx)
  por_ret3[i] = sum(wx3*rx)
  por_ret4[i] = sum(wx4*rx)
  por_ret5[i] = sum(wx5*rx)
  por_ret6[i] = sum(wx6*rx)
  por_ret7[i] = sum(wx7*rx)
  
  # portfolio Net Return
  por_netrx1[i] <- (1+sum(wx1*rx))*(1-epx*tor1[i])-1
  por_netrx2[i] <- (1+sum(wx2*rx))*(1-epx*tor2[i])-1
  por_netrx3[i] <- (1+sum(wx3*rx))*(1-epx*tor3[i])-1
  por_netrx4[i] <- (1+sum(wx4*rx))*(1-epx*tor4[i])-1
  por_netrx5[i] <- (1+sum(wx5*rx))*(1-epx*tor5[i])-1
  por_netrx6[i] <- (1+sum(wx6*rx))*(1-epx*tor6[i])-1
  por_netrx7[i] <- (1+sum(wx7*rx))*(1-epx*tor7[i])-1
  
  # HHI
  hhi1[i]<-sum(wx1^2)/(sum(abs(wx1))^2)
  hhi2[i]<-sum(wx2^2)/(sum(abs(wx2))^2)
  hhi3[i]<-sum(wx3^2)/(sum(abs(wx3))^2)
  hhi4[i]<-sum(wx4^2)/(sum(abs(wx4))^2)
  hhi5[i]<-sum(wx5^2)/(sum(abs(wx5))^2)
  hhi6[i]<-sum(wx6^2)/(sum(abs(wx6))^2)
  hhi7[i]<-sum(wx7^2)/(sum(abs(wx7))^2)
  
  # SLR
  slr1[i]<-sum(abs(wx1[wx1<0]))/sum(abs(wx1[wx1>0]))
  slr2[i]<-sum(abs(wx2[wx2<0]))/sum(abs(wx2[wx2>0]))
  slr3[i]<-sum(abs(wx3[wx3<0]))/sum(abs(wx3[wx3>0]))
  slr4[i]<-sum(abs(wx4[wx4<0]))/sum(abs(wx4[wx4>0]))
  slr5[i]<-sum(abs(wx5[wx5<0]))/sum(abs(wx5[wx5>0]))
  slr6[i]<-sum(abs(wx6[wx6<0]))/sum(abs(wx6[wx6>0]))
  slr7[i]<-sum(abs(wx7[wx7<0]))/sum(abs(wx7[wx7>0]))
  
  WX1[i+1,] <- wx1
  WX2[i+1,] <- wx2
  WX3[i+1,] <- wx3
  WX4[i+1,] <- wx4
  WX5[i+1,] <- wx5
  WX6[i+1,] <- wx6
  WX7[i+1,] <- wx7
  
  print(i)
}


OoS.return.data <- cbind(por_netrx1,por_netrx2,por_netrx3,por_netrx4,
                         por_netrx5,por_netrx6,por_netrx7)

OoS.return.data <- OoS.return.data*100
colnames(OoS.return.data) <- c('mvp','nsmvp','nsgmvp','gmvp','tan','price weighted','fixed weighted')
OoS.return.data <- as.data.frame(OoS.return.data)


#### 投資組合樣本外績效 時間序列圖 ----
par(mfrow = c(3,2))
plot(x = data_ret[(kx+1):nrow(data_ret),]$Date,
     y = por_netrx1,
     type = 'l',
     lwd = 2,
     main = 'Out-of-sample portfolio return (mvp)',xlab = "Date",ylab = 'Return',
     col = 2)

plot(x = data_ret[(kx+1):nrow(data_ret),]$Date,
     y = por_netrx2,
     type = 'l',
     lwd = 2,
     main = 'Out-of-sample portfolio return (nsmvp)',xlab = "Date",ylab = 'Return',
     col = 4)

plot(x = data_ret[(kx+1):nrow(data_ret),]$Date,
     y = por_netrx3,
     type = 'l',
     lwd = 2,
     main = 'Out-of-sample portfolio return (nsgmvp)',xlab = "Date",ylab = 'Return',
     col = 5)
plot(x = data_ret[(kx+1):nrow(data_ret),]$Date,
     y = por_netrx4,
     type = 'l',
     lwd = 2,
     main = 'Out-of-sample portfolio return (gmvp)',xlab = "Date",ylab = 'Return',
     col = 6)
por_netrx5[8] <- 0 #因為他=-17
plot(x = data_ret[(kx+1):nrow(data_ret),]$Date,
     y = por_netrx5,
     type = 'l',
     lwd = 2,
     main = 'Out-of-sample portfolio return (tangency)',xlab = "Date",ylab = 'Return',
     col = 7)
plot(x = data_ret[(kx+1):nrow(data_ret),]$Date,
     y = por_netrx6,
     type = 'l',
     lwd = 2,
     main = 'Out-of-sample portfolio return (price weighted)',xlab = "Date",ylab = 'Return',
     col = 8)
plot(x = data_ret[(kx+1):nrow(data_ret),]$Date,
     y = por_netrx7,
     type = 'l',
     lwd = 2,
     main = 'Out-of-sample portfolio return (fixed weighted)',xlab = "Date",ylab = 'Return',
     col = 1)

#### 投資組合樣本外 累積報酬 時間序列圖 ----
par(mfrow = c(1,2))
# MVP
cumr_mvp <- cumprod(1+por_netrx1)
cumr_nsmvp <- cumprod(1+por_netrx2)
cumr_nsgmvp <- cumprod(1+por_netrx3)
cumr_gmvp <- cumprod(1+por_netrx4)
cumr_tan <- cumprod(1+por_netrx5)
cumr_pw <- cumprod(1+por_netrx6)
cumr_fw <- cumprod(1+por_ret7)

plot(x = data_ret[(kx+1):nrow(data_ret),]$Date,
     y = cumr_mvp,
     type = 'l',
     lwd = 2,col = 2,
     main = 'Cumulative net return of the portfolios ',
     xlab = "Date",ylab = "Cumulative return",
     ylim = range(c(cumr_mvp,cumr_nsgmvp,cumr_gmvp,cumr_tan,
                    cumr_pw,cumr_fw)))
lines(x = data_ret[(kx+1):nrow(data_ret),]$Date,
      y = cumr_nsmvp,
      type = 'l',col = 4,lwd = 2)
lines(x = data_ret[(kx+1):nrow(data_ret),]$Date,
      y = cumr_nsgmvp,
      type = 'l',col = 5,lwd = 2)
lines(x = data_ret[(kx+1):nrow(data_ret),]$Date,
      y = cumr_gmvp,
      type = 'l',col = 6,lwd = 2)
lines(x = data_ret[(kx+1):nrow(data_ret),]$Date,
      y = cumr_tan,
      type = 'l',col = 7,lwd = 2)
lines(x = data_ret[(kx+1):nrow(data_ret),]$Date,
      y = cumr_pw,
      type = 'l',col = 8,lwd = 2)
lines(x = data_ret[(kx+1):nrow(data_ret),]$Date,
      y = cumr_fw,
      type = 'l',col = 1,lwd = 2)
legend('topleft',
       lwd = 2,col=c(2,4,5,6,7,8,1),
       legend = c('mvp','nsmvp','nsgmvp','gmvp','tan','price weighted','fixed weighted'),
       bty = 'n')

# Descriptive statistics of the oos portfolio returns, inclduing ----
#                  Number of observations, min, median, max, mean, std, skewness, kurtosis, acf1,
#                  (annualized and percentage)

s2 <- apply(OoS.return.data,2,summary)*252
std2 <- apply(OoS.return.data, 2, sd)*sqrt(252)
skew2 <- apply(OoS.return.data, 2, my_skewness)/sqrt(252)
kurt2 <- apply(OoS.return.data, 2, my_kurtosis)/252
acf2 <- apply(OoS.return.data, 2, my_acf1)
number.observation2 <- apply(OoS.return.data,2,length)

SUMMARY2 <- rbind(number.observation2,s2,std2,skew2,kurt2,acf2)
SUMMARY2 <- round(SUMMARY2,3)
SUMMARY2 <- SUMMARY2[c(-3,-6),]
SUMMARY2 <- t(SUMMARY2)
write.table(SUMMARY2, "summary.portfolio.csv", sep = ",") #輸出csv

# The risk free rate (used for calculating the Sharpe ratio) ----

# Sharpe ratio (calculated with the out-of-sample portfolio returns) ----
rf = rf

sharpeRatio_1 <- round((mean(por_netrx1)-rf)/(sd(por_netrx1))*sqrt(252),3)
sharpeRatio_2 <- round((mean(por_netrx2)-rf)/(sd(por_netrx2))*sqrt(252),3)
sharpeRatio_3 <- round((mean(por_netrx3)-rf)/(sd(por_netrx3))*sqrt(252),3)
sharpeRatio_4 <- round((mean(por_netrx4)-rf)/(sd(por_netrx4))*sqrt(252),3)
sharpeRatio_5 <- round((mean(por_netrx5)-rf)/(sd(por_netrx5))*sqrt(252),3)
sharpeRatio_6 <- round((mean(por_netrx6)-rf)/(sd(por_netrx6))*sqrt(252),3)
sharpeRatio_7 <- round((mean(por_netrx7)-rf)/(sd(por_netrx7))*sqrt(252),3)

SHARPERATIO <- cbind(sharpeRatio_1,sharpeRatio_2,sharpeRatio_3,sharpeRatio_4,
                     sharpeRatio_5,sharpeRatio_6,sharpeRatio_7)
colnames(SHARPERATIO) <- c('mvp','nsmvp','nsgmvp','gmvp','tan','price weighted','fixed weighted')
rownames(SHARPERATIO) <- paste('Sharpe Ratio  with rf = ',round(rf*100,5),'(%)')
# Min, median, max, mean and std of turnover rate, HHI and SLR ----
TUNROVER.RATE <- cbind(c(summary(tor1),sd(tor1)),
                       c(summary(tor2),sd(tor2)),
                       c(summary(tor3),sd(tor3)),
                       c(summary(tor4),sd(tor4)),
                       c(summary(tor5),sd(tor5)),
                       c(summary(tor6),sd(tor6)),
                       c(summary(tor7),sd(tor7)))

colnames(TUNROVER.RATE) <- c('mvp','nsmvp','nsgmvp','gmvp','tan','price weighted','fixed weighted')
rownames(TUNROVER.RATE) <- c(rownames(TUNROVER.RATE)[-7],'sd')
TUNROVER.RATE <- round(TUNROVER.RATE,3)
TUNROVER.RATE <- t(TUNROVER.RATE)
write.table(TUNROVER.RATE, "TUNROVER.RATE.csv", sep = ",") #輸出csv


HHI <- cbind(c(summary(hhi1),sd(hhi1)),
             c(summary(hhi2),sd(hhi2)),
             c(summary(hhi3),sd(hhi3)),
             c(summary(hhi4),sd(hhi4)),
             c(summary(hhi5),sd(hhi5)),
             c(summary(hhi6),sd(hhi6)),
             c(summary(hhi7),sd(hhi7)))

colnames(HHI) <- c('mvp','nsmvp','nsgmvp','gmvp','tan','price weighted','fixed weighted')
rownames(HHI) <- c(rownames(HHI)[-7],'sd')
HHI <- round(HHI,3)
HHI <- t(HHI)
write.table(HHI, "HHI.csv", sep = ",") #輸出csv



SLR <- cbind(c(summary(slr1),sd(slr1)),
             c(summary(slr2),sd(slr2)),
             c(summary(slr3),sd(slr3)),
             c(summary(slr4),sd(slr4)),
             c(summary(slr5),sd(slr5)),
             c(summary(slr6),sd(slr6)),
             c(summary(slr7),sd(slr7)))

colnames(SLR) <- c('mvp','nsmvp','nsgmvp','gmvp','tan','price weighted','fixed weighted')
rownames(SLR) <- c(rownames(SLR)[-7],'sd')
SLR <- round(SLR,3)
SLR <- t(SLR)
write.table(SLR, "SLR.csv", sep = ",") #輸出csv

# VaR, ES, LPSD of the out-of-sample portfolio returns ----

#date <- as.Date(return.data[(kx+1):nrow(return.data),]$Date)
#OoS.return.data$Date <- date


VaR <- apply(OoS.return.data,2,VaR_samplex,amount = 1,alpha = 0.05)
VaR <- round(VaR,3)

ES <-  apply(OoS.return.data,2,ES_samplex,amount = 1,alpha = 0.05)
ES <- round(ES,3)

LPSD <- apply(OoS.return.data,2,LPSDx,rfx = rf*100)
LPSD <- round(LPSD,3)

rbind(VaR,ES,LPSD)
V <- t(rbind(VaR,ES,LPSD))
write.table(V, "V.csv", sep = ",") #輸出csv

