# quantmod是R平台用于金融建模的扩展包主要功能有：从多个数据源获取历史数据、绘制金融数据图表、在金融数据图表

# 中添加技术指标、计算不同时间尺度的收益率、金融时间序列分析、金融模型拟合与计算等等

# 使用R平台做金融大数据处理几乎必用此扩展软件包

library(quantmod)


#### quantmod 第1课 ####
{
  
  getSymbols("^GSPC",from="2010-1-1",to="2014-1-1")
  save(GSPC,file="GSPC.RDATA")
  
  getSymbols("MSFT",from="2010-1-1",to="2014-1-1")
  save(MSFT,file="MSFT.RDATA")
  
  getSymbols("ORCL",from="2010-1-1",to="2014-1-1")
  save(ORCL,file="ORCL.RDATA")
  
  getSymbols("BIDU",from="2010-1-1",to="2014-1-1")
  save(BIDU,file="BIDU.RDATA")
  
  getSymbols("AAPL",from="2010-1-1",to="2014-1-1")
  save(AAPL,file="AAPL.RDATA")
  
  load("GSPC.RDATA")
  load("BIDU.RDATA")
  load("AAPL.RDATA")
  load("ORCL.RDATA")
  load("MSFT.RDATA")
  
  ls()
  removeSymbols("AAPL")
  ls()
  
  load("AAPL.RDATA")
  
  
  # 提取时间序列中的指定部分
  head(Op(GSPC))
  head(Hi(GSPC))
  head(Lo(GSPC))
  head(Cl(GSPC))
  head(Vo(GSPC))
  head(Ad(GSPC))
  
  
  head(GSPC)
  tail(GSPC)
  
  
  # 返回整个时间序列中最早、最晚的一条记录
  seriesLo(GSPC)
  seriesHi(GSPC)
  
  
  
  # 获取市场上涨/下跌序列的逻辑值
  head(Cl(GSPC),10)
  tail(seriesIncr(Cl(GSPC), thresh=0, diff.=1L),10)
  tail(seriesDecr(Cl(GSPC), thresh=0, diff.=1L),10)
  
  
  
  # 获取市场持续上涨/下跌序列的逻辑值
  tail(seriesAccel(Cl(GSPC)),10)
  tail(seriesDecel(Cl(GSPC)),10)
  
  
  
  tail(OpCl(GSPC))
  tail(ClCl(GSPC))
  tail(HiCl(GSPC))
  tail(LoCl(GSPC))
  tail(LoHi(GSPC))
  tail(OpHi(GSPC))
  tail(OpLo(GSPC))
  tail(OpOp(GSPC))
  
  
  # 提取OHLCVA数据中的指定部分
  tail(HLC(GSPC))
  tail(OHLC(GSPC))
  tail(OHLCV(GSPC))
  
}

#### quantmod 第2课 ####
{
  
  Stock.Close <- c(102.12,102.62,100.12,103.00,103.87,103.12,105.12)
  Close.Dates <- as.Date(c(10660,10661,10662,10665,10666,10667,10668),origin="1970-01-01")
  Stock.Close <- zoo(Stock.Close,Close.Dates);
  
  Stock.Close
  
  Next(Stock.Close)       # 向前1个时间周期
  Next(Stock.Close,k=1)   # 同上
  
  # k必须为非负数
  Next(Stock.Close,k=-1)
  
  
  # 合并两个时间序列的数据
  # 实战中可以将一些经济指标、技术指标、其他交易品种数据和待分析数据做个合并
  merge(Stock.Close,Next(Stock.Close))
  
  
  # addTA()在现有图形上加入指标、成交量、函数运算结果等信息
  # on：绘制图形的窗口
  windows()
  chartSeries(GSPC,TA=NULL)      # 绘制完全干净的数据图
  addTA(SMA(Cl(GSPC),n=20), on=1, col="red")
  
  
  
  # 将均线绘制在第二个窗口失败
  # 关键是TA=NULL测试
  windows()
  chartSeries(GSPC,TA=NULL)      
  addTA(SMA(Cl(GSPC),n=20), on=2, col="red")
  
  
  
  # 把均线绘制在第二个成交量窗口
  # on参数只能设置在已经存在的窗口绘制图形，并不能增加一个新的窗口
  windows()
  chartSeries(GSPC)      
  addTA(SMA(Cl(GSPC),n=20), on=2, col="blue")
  
  
  
  # 不指定窗口则自动增加一个新窗口并在新窗口绘制图形
  windows()
  chartSeries(GSPC)      
  addTA(SMA(Cl(GSPC),n=20),col="blue")
  
  
  
  # 绘制2012-2013年历史数据的走势图并加入双均线
  # 时间序列的[]中同样可以使用ISO时间序列字符串形式获取数据
  tmp <- GSPC["2012/2013",]
  windows()
  chartSeries(tmp,TA=NULL)
  addTA(SMA(Cl(tmp),n=5), on=1, col="red")
  Sys.sleep(5)
  addTA(SMA(Cl(tmp),n=20), on=1, col="blue")
  
  
  
  # 不只是现有指标其他函数或自定义函数也可以
  windows()
  chartSeries(GSPC)
  addTA(OpCl(GSPC), col="blue", type="l", lwd=1)
  
  
  
  # 成交量数据占用一个窗口较为不便，可以只抽取OHLC数据绘图
  windows()
  chartSeries(OHLC(GSPC))
  addTA(OpCl(GSPC), col="blue", type="l", lwd=1)
  
  
  # 自定义函数
  myfun <- function(x) { return(log(Cl(x))) }
  
  windows();chartSeries(GSPC,TA=NULL)
  
  # addTA()可以调用自定义函数计算计算数据并绘制图形
  addTA(myfun(GSPC), col="white", type="l", lwd=1)
  
  
  
}

#### quantmod 第3课 ####
{
  
  # 加入波动率曲线
  windows()
  chartSeries(GSPC,TA=NULL)
  addVolatility()
  
  
  
  # 计算不同时间周期计算出的收益率
  
  tail(dailyReturn(GSPC))
  
  tail(dailyReturn(Cl(GSPC)))
  
  tail(weeklyReturn(GSPC))
  
  tail(yearlyReturn(GSPC))
  
  
  
  # 计算百分比变化
  
  # 不能直接使用OHLC数据
  tail(Delt(GSPC))
  
  # 只计算收盘价的百分比变化，默认第二个参数为NULL，k=0
  # 只取收盘价的结算结果和dailyReturn()的结算结果一致
  tail(Delt(Cl(GSPC)))
  
  
  
  # 只计算收盘价的百分比变化，默认第二个参数为NULL，k=0
  xts01 <- tail(Cl(GSPC),10)
  xts02 <- tail(Cl(AAPL),10)
  Delt(xts01,xts02)
  
  Delt(xts01,xts02,k=5)
  
  Delt(xts01,xts02,type="log")  
  
  # 判断数据类型
  
  is.OHLC(GSPC)
  
  is.OHLCV(GSPC)
  
  
  # findPeaks()/findValleys 查找时间序列的波峰、波谷
  # 特别注意：函数返回的是上涨或下跌趋势出现改变后的第一个时间周期的位置
  
  tmp <- tail(Cl(GSPC),100)
  
  # 获得的结果是数据峰值之后一个数值在原向量中的索引值
  findPeaks(tmp)
  
  # 
  windows();plot(1:100,tmp,type="l")
  points(findPeaks(tmp),tmp[findPeaks(tmp),],col="red",lwd=2)
  
  
  # 获得的结果是数据谷值之后一个数值在原向量中的索引值
  findValleys(tmp)
  
  points(findValleys(tmp),tmp[findValleys(tmp),],col="blue",lwd=2)
  
  
  
  # 获取期权或期货的到期数据信息
  options.expiry(AAPL)
  futures.expiry(AAPL)
  
  AAPL[options.expiry(AAPL)]
  
}

#### 第4课  quantmod 拟合模型做模拟交易 ####
{
  
  # 
  
  # Recursive Partitioning and Regression Trees
  # 递归分割与回归树
  
  library(rpart)
  
  # rpart扩展包是一个极端重要的使用决策树作分析的工具。在数据挖掘、机器学习领域有广泛用途。该扩展包有专门课程做详细介绍
  
  # 生成单个可以复用的模型用于后期的buildModel()函数调用
  
  # 第1个参数为：formula
  # 这是公式对象。该对象包含交易品名称和待拟合模型的公式
  # 模型是通过标准的公式机制来指定
  # 一个金融模型可能会包含多个金融/经济指标。每种指标的数据源、频率、类型可能不同
  # 调用specifyModel()函数来设定他们的关系
  # 现在：quantmod.OHLC,zoo,ts,its等数据类型都可以被支持
  # 交易品数据默认会从全局环境中获取
  # 如果交易品数据不存在函数会调用getSymbols()函数并使用默认的数据源配置下载
  
  # 第2个参数为：na.rm=TRUE
  m <- specifyModel(Next(OpCl(SPY)) ~ Lag(OpHi(SPY)))
  
  class(m)
  mode(m)
  str(m)
  
  
  # 从quantmod对象中提取用于建模的函数
  # specifyModel()函数正常运行之后就会生成一个S4类型的对象。该对象包含一组 model.data 数据
  modelData(m)
  
  # 再举个生成模型，建立数据集合并提取数据的例子
  m1 <- specifyModel(Next(OpCl(SPY)) ~ Cl(SPY) + OpHi(SPY) + Lag(Cl(SPY)))
  modelData(m1) 
  
  
  
  # 输入参数为specifyModel()函数生成的对象
  # 返回对象的描述信息
  getModelData(m)
  
  
  # buildModel()函数在生成的quantmod对象上附加一个拟合模型
  # buildModel(x, method, training.per, ...)
  # x: specifyModel()函数生成的quantmod对象
  # method:由字符串命名的拟合模型
  # training.per:字符串表示的ISO 8601类型日期数据。表示起始和结束日期
  
  
  # method字符串表示的模型是整个函数的核心：
  
  # 目前支持的模型有：lm, glm, loess, step, ppr, rpart[rpart], tree[tree], randomForest[randomForest],
  # mars[mda], polymars[polspline], lars[lars], rq[quantreg], lqs[MASS], rlm[MASS], svm[e1071], 
  # nnet[nnet].
  
  # 虽然不是什么模型都可以用，但作者已经将很多很实用的模型放入其中有极高的实用价值
  # 后面我们会发布专项研究和案例研究类课程介绍这些模型的使用
  
  # 用户也可以将其他模型封装起来在这个函数中使用
  # 要求是：构建这些封装函数时必须使用quantmod，training.data参数
  # 封装函数必须返回你和模型并且必须有一个预测方法(method)
  # buildModel.skeleton() 函数可以用于构建新的method
  m.built <- buildModel(m, method = "rpart", training.per = c("2013-01-01", "2014-01-01"))
  
  
  class(m.built)
  mode(m.built)
  str(m.built)
  
  
  # 在构件好模型之后同样可以调用getModelDate()函数来查看模型详细信息
  # 比使用str()直接查看对象的内部信息还是轻松很多
  getModelData(m.built)
  
  
  # 根据拟合的quantmod对象进行模拟交易
  # 给定拟合模型，tradeModel()在给定的历史时间周期中计算产生的交易信号
  # 然后使用指定的trade.rule来计算并返回tradeLog对象
  # 可以调用其他method来计算模型策略的性能
  t <- tradeModel(m.built)
  
  # 返回的结果是一个quantmodResults对象，本质上还是一个列表
  class(t)
  mode(t)
  str(t)
  
  print(t)
  
  # 设置不同杠杆比例做计算
  
  tradeModel(m.built,leverage=1)
  
  tradeModel(m.built,leverage=2)
  
}


