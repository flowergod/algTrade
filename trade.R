library('zoo')
library('xts')
library('TTR')
library('quantmod')
setwd('d:/R')
source('tradeStr.R')

vr_alg <- as.vector(NULL)
vr_bh <- as.vector(NULL)


# 获取数据
for (i in 1997:2015) {
  d_from <- paste(i,'-01-01',sep='')
  d_to <- paste(i+1,'-01-01',sep='')
  
  qData <- get(getSymbols("^SSEC", from=d_from, to=d_to))

  # 做图
  chartSeries(qData, up.col='red', dn.col='green', TA="addVo(); addMACD(); addSMA(n=10, on=1, col='white'); addSMA(n=20, on=1, col='yellow'); addSMA(n=30, on=1, col='green'); addSMA(n=60, on=1, col='blue')")
    
  # 策略执行
  t <- strMAfastGtMAslow(20, 10, qData)
    
  # 计算收益
  n<-seq(to=ifelse(length(t)%%2==0,length(t),(length(t)-1)), by=2)
  r_alg_ser <- cumprod(-as.numeric(t[n+1])/as.numeric(t[n]))
  r_alg <- r_alg_ser[length(r_alg_ser)]
  r_bh <- as.numeric(last(Cl(qData))) / as.numeric(first(Cl(qData)))
  
  vr_alg <- append(vr_alg, r_alg)
  vr_bh <- append(vr_bh, r_bh)
  write.csv(cbind(vr_alg, vr_bh), file='./Output/vr_alg.csv', row.names=FALSE)
  # t
  # r_alg
  # r_bh  
  
  # 文件输出
#   title <- paste('From:', d_from, as.numeric(first(Cl(qData))), '    To:', d_to, as.numeric(last(Cl(qData))))
#   write.table(title, file='./Output/algTradeRec.txt', append=TRUE, quote=FALSE,row.names=FALSE)
#   write.table(data.frame(index(t), as.vector(t)), file='./Output/algTradeRec.txt', sep='\t', append=TRUE, quote=FALSE,row.names=FALSE)
#   write.table(paste('r alg:', r_alg), file='./Output/algTradeRec.txt', append=TRUE, quote=FALSE,row.names=FALSE)
#   write.table(paste('r bh:', r_bh), file='./Output/algTradeRec.txt', append=TRUE, quote=FALSE,row.names=FALSE)

}





