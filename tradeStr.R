# 短期均线上穿长期均线
strMAfastGtMAslow <- function(dSlow, dFast, anData) {
  ma_fast <- runMean(anData[,4], n=dFast)
  ma_slow <- runMean(anData[,4], n=dSlow)
  # 长期均线大于短期均线，则1，否则0
  p <- ifelse(ma_slow>ma_fast, 1, 0)
  p_l <- Lag(p)
  # 当日短期均线大于长期均线，上日长期均线大于短期均线，买入
  buy <- p==0 & p!=p_l
  # 当日短期均线小于长期均线，上日长期均线小于短期均线，卖出
  sell <- p==1 & p!=p_l
  buy[is.na(buy)] <- FALSE
  sell[is.na(sell)] <- FALSE
  trade <- Cl(anData)*as.numeric(buy)-Cl(anData)*as.numeric(sell)
  trade <- trade[trade!=0]
  # 卖信号先出现的话，去除
  ifelse(trade[1]>0, trade<-trade, trade<-trade[2:length(trade)])
  # 最后是买信号的话，以最后收盘价作为最后一期计算收益的结算价
  if(trade[length(trade)]>0) trade<-append(trade, -last(Cl(anData)))
  trade
}
