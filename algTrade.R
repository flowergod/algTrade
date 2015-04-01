library('zoo')
library('xts')
library('TTR')
library('quantmod')
# 获取数据
qData <- get(getSymbols("^SSEC", from='2012-01-01', to='2013-01-01'))
# qData <- get(getSymbols("600000.SS", from='2011-01-01', to='2014-7-31'))
chartSeries(qData, up.col='red', dn.col='green', TA="addVo(); addMACD()")

# 添加指标
ma_10 <- runMean(qData[,4], n=10)
ma_20 <- runMean(qData[,4], n=20)
ma_60 <- runMean(qData[,4], n=60)
addTA(ma_10,on=1,col="white")
addTA(ma_20,on=1,col="yellow")
addTA(ma_60,on=1,col="blue")

# 策略执行
# 10日均线上穿20日均线
p <- ifelse(ma_20>ma_10, 1, 0)
p_l <- Lag(p)
buy <- p==0 & p!=p_l
sell <- p==1 & p!=p_l
buy[is.na(buy)] <- FALSE
sell[is.na(sell)] <- FALSE
trade <- Cl(qData)*as.numeric(buy)-Cl(qData)*as.numeric(sell)


# 计算收益
t <- trade[trade!=0]
# 卖信号先出现的话，去除
ifelse(t[1]>0, t<-t, t<-t[2:length(t)])
# 最后是买信号的话，以最后收盘价作为最后一期计算收益的结算价
ifelse(t[length(t)]>0, t<-append(t, -last(Cl(qData))),)
n<-seq(to=ifelse(length(t)%%2==0,length(t),(length(t)-1)), by=2)
r_alg_ser <- cumprod(-as.numeric(t[n+1])/as.numeric(t[n]))
r_alg <- r_alg_ser[length(r_alg_ser)]
r_bh <- as.numeric(last(Cl(qData))) / as.numeric(first(Cl(qData)))

t
r_alg
r_bh



