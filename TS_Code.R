# 导入需要的包

packages = c("ggplot2", "dplyr", "tidyr", "data.table", 'corrplot', 'forecast', 'tseries',
             'TSA', 'tibble', 'TTR', 'xts', 'assertthat','dygraphs')
#  不存在包'gridExtra'，

my.install <- function(pkg, ...){
if (!(pkg %in% installed.packages()[,1])) {
install.packages(pkg)
}
return (library(pkg, ...))
}
purrr::walk(packages, my.install, character.only = TRUE, warn.conflicts =
                FALSE)

# 读取数据，经检查不存在缺失值------------------------------------------------------------------
all_index <- read.csv(file ="Indexes201201-202305.csv",sep=",")[1:2751,] # 剔除“数据来源：wind”
all_index$Date <- as.Date(all_index$Date, format = "%Y/%m/%d")

# 以时间为索引并表（其实没必要用这个包，只是为了记录这种用法，以备不时之需）
xts_list <- vector(mode="list", length=3)
xts50 = xts(all_index$SSE50, order.by=all_index$Date)
xts_list[['SSE50']] <- xts50
xts300 = xts(all_index$CSI300, order.by=all_index$Date)
xts_list[['CSI300']] <- xts300
xts500 = xts(all_index$CSI500, order.by=all_index$Date)
xts_list[['CSI500']] <- xts500
xts_table= do.call(cbind, xts_list)

dygraph(xts_table, xlab = "Time", ylab = "High value", main = "Time Series
") %>% dyRangeSelector()

# 指数、收益率作图---------------------------------------------------------------------
# SSE50、收益率作图
d<-all_index$SSE50[1:2751];d1<-all_index$SSE50[2:2751]; d2<-all_index$SSE50[1:2750]
ps.options(horizontal=F, width=5, height=6, pointsize=14)
par(mar=c(2,4,1,1),mfrow=c(2,1), mgp=c(1.8,0.5,0))
plot(d, type='l', ylab="Daily price", xaxt="n",xlab='',col="chartreuse4")
axis(1, at=c(100, 350, 600, 850, 1100, 1350, 1600, 1850, 2100, 2350, 2600, 2850),
labels=c("2012","2013","2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021","2022","2023"))
plot((d1-d2)/d2, type='l', ylab="Daily log return", xaxt="n",xlab='year',col="cornflowerblue",ylim=c(-0.1,0.1))
axis(1, at=c(100, 350, 600, 850, 1100, 1350, 1600, 1850, 2100, 2350, 2600, 2850),
labels=c("2012","2013","2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021","2022","2023"))

# CSI300、收益率作图
d<-all_index$CSI300[1:2751];d1<-all_index$CSI300[2:2751]; d2<-all_index$CSI300[1:2750]
ps.options(horizontal=F, width=5, height=6, pointsize=14)
par(mar=c(2,4,1,1),mfrow=c(2,1), mgp=c(1.8,0.5,0))
plot(d, type='l', ylab="Daily price", xaxt="n",xlab='',col="cyan4")
axis(1, at=c(100, 350, 600, 850, 1100, 1350, 1600, 1850, 2100, 2350, 2600, 2850),
     labels=c("2012","2013","2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021","2022","2023"))
plot((d1-d2)/d2, type='l', ylab="Daily return", xaxt="n",xlab='year',col="cornflowerblue",ylim=c(-0.1,0.1))
axis(1, at=c(100, 350, 600, 850, 1100, 1350, 1600, 1850, 2100, 2350, 2600, 2850),
     labels=c("2012","2013","2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021","2022","2023"))

# CSI500、收益率作图
d<-all_index$CSI500[1:2751];d1<-all_index$CSI500[2:2751]; d2<-all_index$CSI500[1:2750]
ps.options(horizontal=F, width=5, height=6, pointsize=14)
par(mar=c(2,4,1,1),mfrow=c(2,1), mgp=c(1.8,0.5,0))
plot(d, type='l', ylab="Daily price", xaxt="n",xlab='',col="purple4")
axis(1, at=c(100, 350, 600, 850, 1100, 1350, 1600, 1850, 2100, 2350, 2600, 2850),
     labels=c("2012","2013","2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021","2022","2023"))
plot((d1-d2)/d2, type='l', ylab="Daily return", xaxt="n",xlab='year',col="cornflowerblue",ylim=c(-0.1,0.1))
axis(1, at=c(100, 350, 600, 850, 1100, 1350, 1600, 1850, 2100, 2350, 2600, 2850),
     labels=c("2012","2013","2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021","2022","2023"))

# 正态性检验----------------------------------------------------------------------------
d<-all_index$SSE50[1:2751];d1<-all_index$SSE50[2:2751]; d2<-all_index$SSE50[1:2750]
e<-all_index$CSI300[1:2751];e1<-all_index$CSI300[2:2751]; e2<-all_index$CSI300[1:2750]
f<-all_index$CSI500[1:2751];f1<-all_index$CSI500[2:2751]; f2<-all_index$CSI500[1:2750]

ps.options(horizontal=F, width=5, height=4, pointsize=12)
par(mar=c(3,3,2,1),mfrow=c(2,3), mgp=c(1.8,0.5,0))

hist(d1/d2-1, probability=T, nclass=35, xlab='',ylim=c(0,45),
col="chartreuse4",main="SSE50 Daily returns")
x<-seq(-0.5, 0.3, 0.01)
lines(x, dnorm(x, mean(d1/d2-1), sd(d1/d2-1)), col="blue")

hist(e1/e2-1, probability=T, nclass=35, xlab='',ylim=c(0,45),
col="purple4",main="CSI300 Daily returns")
x<-seq(-0.5, 0.3, 0.01)
lines(x, dnorm(x, mean(e1/e2-1), sd(e1/e2-1)), col="blue")

hist(f1/f2-1, probability=T, nclass=35, xlab='',ylim=c(0,45),
col="cyan4",main="CSI500 Daily returns")
x<-seq(-0.5, 0.3, 0.01)
lines(x, dnorm(x, mean(f1/f2-1), sd(f1/f2-1)), col="blue")

qqnorm(d1/d2-1,xlab='Normal quantile', ylab='Quantile of daily returns',
col="brown",ylim=c(-0.1,0.1))
qqline(d1/d2-1, col="blue")


qqnorm(e1/e2-1,xlab='Normal quantile', ylab='Quantile of daily returns',
col="brown",ylim=c(-0.1,0.1))
qqline(e1/e2-1, col="blue")

qqnorm(f1/f2-1,xlab='Normal quantile', ylab='Quantile of daily returns',
col="brown",ylim=c(-0.1,0.1))
qqline(f1/f2-1, col="blue")

jarque.bera.test(all_index$SSE50)
jarque.bera.test(all_index$CSI300)
jarque.bera.test(all_index$CSI500)

# 收益率的平稳性检验，全部通过，lag=14-----------------------------------------------
adf.test(d1/d2-1) # 平稳
adf.test(e1/e2-1) # 平稳
adf.test(f1/f2-1) # 平稳

d_rate = d1/d2-1
e_rate = e1/e2-1
f_rate = f1/f2-1

# 白噪声检验-------------------------------------------------------------------------
Box.test(d_rate[703:2750], type = "Box-Pierce",fitdf = 0)
Box.test(d_rate[703:2750], type = "Ljung",fitdf = 0)
Box.test(d_rate, type = "Box-Pierce",fitdf = 0)# 白噪声，不需要进一步分析
Box.test(e_rate[703:2750])# 白噪声，不需要进一步分析
Box.test(f_rate[703:2750]) 

library(hwwntest)
hwwn.test(d_rate[703:2750],plot.it=TRUE,mc.method = p.adjust.methods)
hywn.test(d_rate[703:2750],filter.number = 10,family = "DaubExPhase")
hywn.test(e_rate[703:2750],filter.number = 10,family = "DaubExPhase")
hywn.test(f_rate[703:2750],filter.number = 10,family = "DaubExPhase")

### ACF、PACF图-----------------------------------------------------------------------

# 中证500收益率的ACF和PACF图
n=14; N=2750
D <- all_index$Date[1:N]
Y <- f_rate[1:N]

ps.options(horizontal=F, width=16, height=6, pointsize=5)

par(mar=c(1,3,1,1),mfrow=c(3,1), mgp=c(1.8,0.8,0))
plot(D[n:N], Y[n:N], type='l', main='Daily Return of CSI500', xlab='', ylab='Return Rate',
         col='chartreuse4')
abline(0,0)
acf(Y[1:N], lag=14, ci.type='ma', main='', ylab='ACF')
acf(Y[n:N], type='partial', lag=30, main='', ylab='PACF')

### 模型建立-----------------------------------------------------------------

## ----------------------------------中证500的ARMA模型
# 参数估计：AR模型
max_lag=15
datanew=f_rate
N=length(datanew)
b=20
# BIC选择模型阶数
BICs=rep(0,max_lag)
ytrain=datanew[1:(N-b)]
ytest=datanew[(N-b+1):N]

for(p in 0:max_lag){
  fit.ar <- stats::arima(ytrain, order = c(p,0,0),method = "ML")
  BICs[p] <- BIC(fit.ar)
}
p_bic=which.min(BICs)
fit.ar <- stats::arima(ytrain, order = c(p_bic,0,0),method = "ML")
fit.ar
# 白噪声检验
forecast_ar <- forecast(fit.ar,h=b,level=c(95))# h表示向前预测b步
checkresiduals(forecast_arma)


# 参数估计：MA模型

# 参数估计：ARIMA模型
max_lag=8 # adf检验lag_order=14，所以取15，但是运行时间太久了，就先选个5吧
datanew=f_rate
N=length(datanew)
b=20 # 先前预测的数量
BICs=rep(0,max_lag)# BIC选择模型阶数
ytrain=datanew[1:(N-b)]
ytest=datanew[(N-b+1):N]

BICs=matrix(0,max_lag,max_lag)
for(p in 0:max_lag){
  for(q in 0:max_lag){
    fit.arma <- stats::arima(ytrain, order = c(p,0,q),method = "ML")
    BICs[p,q] <- BIC(fit.arma)
  }
}
which(BICs==min(BICs),arr.ind = TRUE) # 最后的结果是ARMA(2,3)
fit.arma <- stats::arima(ytrain, order = c(2,0,3),method = "ML")
fit.arma

# 预测
forecast_arma <- forecast(fit.arma,h=b,level=c(95))
forecast_mean=meanf(ytrain,h=b) #均值预测
forecast_naive=naive(ytrain,h=b) #朴素预测
# 预测精度
accuracy_arma=accuracy(forecast_arma,ytest)[2,]
accuracy_mean=accuracy(forecast_mean,ytest)[2,]
accuracy_naive=accuracy(forecast_naive,ytest)[2,]
# 精度合并起来
# 多种方法预测结果并列在一起
data_sz50=rbind(accuracy_arma[1:3],accuracy_mean[1:3],accuracy_naive[1:3])
# 对结果进行白噪声检验
checkresiduals(forecast_arma)

# 宏观经济指标--------------------------------
econdata = read.csv("EconData201201-202305.csv")[1:136,]
econdata[,"中国.CGPI.当月同比"] = econdata[,"中国.CGPI.当月同比"]-100

# Convert columns to numeric data type and handle missing values
columns = c("中国.CPI.环比",	"中国.PPI.全部工业品.环比",	"中国.PMI", "中国.M0.同比",	"中国.M1.同比",	"中国.M2.同比",	"中国.社会消费品零售总额.当月同比","中国.金融机构.各项贷款余额.同比")
df = econdata
df[,columns] <- 
  sapply(df[,columns], as.numeric)

filled_df = zoo::na.approx(df[, columns], method = "linear")
# Create an empty dataframe to store the Box test results
adf_test_results <- data.frame(
  Variable = character(),
  Original = numeric(),
  First_Difference = numeric(),
  #Second_Difference = numeric(),
  stringsAsFactors = FALSE
)

# Iterate over each column and calculate the Box test p-values
for (col in colnames(filled_df)) {
  variable <- df[[col]]
  # Remove missing values
  variable <- na.omit(variable)

  adf_test_original <- adf.test(variable)
  adf_test_diff1 <- adf.test(diff(variable))
  adf_test_diff2 <- adf.test(diff(diff(variable)))
  
  # Append the results to the dataframe
  adf_test_results <- rbind(adf_test_results, data.frame(
    Variable = col,
    Original = adf_test_original$p.value,
    First_Difference = adf_test_diff1$p.value
    #,Second_Difference = adf_test_diff2$p.value
  ))
}

# Print the resulting table
print(adf_test_results)

# Create a correlation matrix
correlation_matrix <- cor(filled_df)

# 设置字符编码为中文（简体）
Sys.setlocale("LC_CTYPE", "zh_CN.UTF-8")
par(family = "微软雅黑")

# Create a heatmap of the correlation matrix
heatmap(correlation_matrix, 
        main = "Correlation Heatmap")

