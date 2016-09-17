############################################################
# Part 1 - Define trading signals and construct portfolios
############################################################

require(PortfolioEffectHFT)

# Moving average
MA=function(x,order){
  result = x
  x1 = c(0,x)
  result[(order):NROW(x)] = (cumsum(x1)[-(1:(order))]-cumsum(x1)[-((NROW(x1)-order+1):NROW(x1))])/order
  result[1:(order-1)] = cumsum(x[1:(order-1)])/(1:(order-1))
  return(result-0.0000000001)
}

# Lag function padded with zeroes
lagpad=function(x, k=1) {
  i = is.vector(x)
  if(is.vector(x)) x = matrix(x) else x = matrix(x,nrow(x))
  if(k>0) {
    x = rbind(matrix(rep(0, k*ncol(x)),ncol=ncol(x)), matrix(x[1:(nrow(x)-k),], ncol=ncol(x)))
  }
  else {
    x = rbind(matrix(x[(-k+1):(nrow(x)),], ncol=ncol(x)),matrix(rep(0, -k*ncol(x)),ncol=ncol(x)))
  }
  if(i) x[1:length(x)] else x
}


symbol = "GOOG"
dateStart = "2014-10-13 09:30:00"
dateEnd = "2014-10-30 16:00:00"
portfolio = portfolio_create(dateStart,dateEnd)

position=position_add(portfolio,symbol,1)
price = compute(price(position))[[1]]
printTime = price[,1]

# Create two strategy by moving average differents length
highFrequencyStrategy = array(0,dim=NROW(price))
highFrequencyStrategy[price[,"value"]<MA(price[,"value"],200)]=100
highFrequencyStrategy[(NROW(price)/2):NROW(price)]=0
highFrequencyStrategy=lagpad(highFrequencyStrategy)

lowFrequencyStrategy=array(0,dim=NROW(price))
lowFrequencyStrategy[price[,"value"]<MA(price[,"value"],800)]=100
lowFrequencyStrategy=lagpad(lowFrequencyStrategy)

############################################################
# Part 2 - Holding Times
############################################################

highFrequencyStrategyPlot = NULL
highFrequencyStrategyPlot = rbind(highFrequencyStrategyPlot,data.frame(positions=sum(highFrequencyStrategy>0)/60,Legends="Has position"))
highFrequencyStrategyPlot = rbind(highFrequencyStrategyPlot,data.frame(positions=sum(highFrequencyStrategy==0)/60,Legends="No position"))
lowFrequencyStrategyPlot = NULL
lowFrequencyStrategyPlot = rbind(lowFrequencyStrategyPlot,data.frame(positions=sum(lowFrequencyStrategy>0)/60,Legends="Has position"))
lowFrequencyStrategyPlot = rbind(lowFrequencyStrategyPlot,data.frame(positions=sum(lowFrequencyStrategy==0)/60,Legends="No position"))

xlabel = ""
ylabel = "In minutes"
p1 = ggplot(highFrequencyStrategyPlot, aes(x = "", y = positions, fill = Legends))+xlab(xlabel)+ylab(ylabel) +  geom_bar(stat = "identity")+
  geom_text(aes(x= 1,y = positions/2 + c(0, cumsum(positions)[-length(positions)]), label = paste(round(positions,digits =1)," minutes",sep="")), size=7,col="#d5e4eb")
p1 = p1+coord_polar("y")+ ggtitle("Intraday holding period for\n high-frequency strategy")+
  util_plotTheme()+util_fillScheme()
p2 = ggplot(lowFrequencyStrategyPlot, aes(x = "", y = positions, fill = Legends))+xlab(xlabel)+ylab(ylabel) +  geom_bar(stat = "identity")+
  geom_text(aes(x= 1,y = positions/2 + c(0, cumsum(positions)[-length(positions)]), label = paste(round(positions,digits =1)," minutes",sep="")), size=7,col="#d5e4eb")
p2 = p2+coord_polar("y")+ ggtitle("Intraday holding period for\n low-frequency strategy")+
  util_plotTheme()+util_fillScheme()
util_multiplot(p1,p2,cols=2)
# util_screenshot('R-holding1.jpg')
############################################################
# Part 3 - Holding Intervals
############################################################

highFrequencyPortfolioHoldOnly = portfolio_create(dateStart,dateEnd)
portfolio_settings(highFrequencyPortfolioHoldOnly,holdingPeriodsOnly=TRUE,resultsNAFilter=F)
positionHFHO=position_add(highFrequencyPortfolioHoldOnly,symbol,quantity=as.numeric(highFrequencyStrategy),time=printTime)
highFrequencyPortfolioHoldOnly

highFrequencyPortfolioAllDay = portfolio_create(dateStart,dateEnd)
portfolio_settings(highFrequencyPortfolioAllDay,holdingPeriodsOnly=FALSE,resultsNAFilter=F)
positionHFAD=position_add(highFrequencyPortfolioAllDay,symbol,quantity=as.numeric(highFrequencyStrategy),time=printTime)
highFrequencyPortfolioAllDay

lowFrequencyPortfolioHoldOnly = portfolio_create(dateStart,dateEnd)
portfolio_settings(lowFrequencyPortfolioHoldOnly,holdingPeriodsOnly=TRUE,resultsNAFilter=F)
positionLFHO=position_add(lowFrequencyPortfolioHoldOnly,symbol,quantity=as.numeric(lowFrequencyStrategy),time=printTime)
lowFrequencyPortfolioHoldOnly

lowFrequencyPortfolioAllDay = portfolio_create(dateStart,dateEnd)
portfolio_settings(lowFrequencyPortfolioAllDay,holdingPeriodsOnly=FALSE,resultsNAFilter=F)
positionLFAD=position_add(lowFrequencyPortfolioAllDay,symbol,quantity=as.numeric(lowFrequencyStrategy),time=printTime)
lowFrequencyPortfolioAllDay

plot1<-util_ggplot(plot(quantity(positionHFHO),title="High Frequency Portfolio Strategy",line_size=0.6))
plot2<-util_ggplot(plot(quantity(positionLFHO),title="Low Frequency Portfolio Strategy",line_size=0.6))
util_multiplot(plot1,plot2,cols=1)
# util_screenshot('R-holding2.jpg')
############################################################
# Part 4 - Trading strategy variance
############################################################
plot(variance(highFrequencyPortfolioHoldOnly),variance(highFrequencyPortfolioAllDay),
     variance(lowFrequencyPortfolioHoldOnly),variance(lowFrequencyPortfolioAllDay),
     title="Variance, daily",legend=c("HF HoldOnly","HF AllDay","LF HoldOnly","LF AllDay"))
# util_screenshot('R-holding3.jpg')
############################################################
# Part 5 - Trading strategy Value-at-Risk
############################################################
plot(value_at_risk(highFrequencyPortfolioHoldOnly,0.95),value_at_risk(highFrequencyPortfolioAllDay,0.95),
     value_at_risk(lowFrequencyPortfolioHoldOnly,0.95),value_at_risk(lowFrequencyPortfolioAllDay,0.95),
     title="VaR, daily",legend=c("HF HoldOnly","HF AllDay","LF HoldOnly","LF AllDay"))
# util_screenshot('R-holding4.jpg')
############################################################
# Part 6 - Trading strategy expected return
############################################################
plot(expected_return(highFrequencyPortfolioHoldOnly),expected_return(highFrequencyPortfolioAllDay),
     expected_return(lowFrequencyPortfolioHoldOnly),expected_return(lowFrequencyPortfolioAllDay),
     title="Expected Return, daily",legend=c("HF HoldOnly","HF AllDay","LF HoldOnly","LF AllDay"))
# util_screenshot('R-holding5.jpg')
############################################################
# Part 7 - Trading strategy Sharpe ratio
############################################################
plot(sharpe_ratio(highFrequencyPortfolioHoldOnly),sharpe_ratio(highFrequencyPortfolioAllDay),
     sharpe_ratio(lowFrequencyPortfolioHoldOnly),sharpe_ratio(lowFrequencyPortfolioAllDay),
     title="Sharpe Ratio, daily",legend=c("HF HoldOnly","HF AllDay","LF HoldOnly","LF AllDay"))

# util_screenshot('R-holding6.jpg')