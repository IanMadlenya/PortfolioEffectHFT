############################################################
# Part 1 - Define trading signals and construct portfolios
############################################################
require(gridExtra)
require(PortfolioEffectHFT)

symbol = "GOOG"
dateStart = "2014-10-13 09:30:00"
dateEnd = "2014-10-14 16:00:00"

# Create function of moving average
MA=function(x,order){
  result<-x
  x1<-c(0,x)
  result[(order):NROW(x)]<-(cumsum(x1)[-(1:(order))]-cumsum(x1)[-((NROW(x1)-order+1):NROW(x1))])/order
  result[1:(order-1)]<-cumsum(x[1:(order-1)])/(1:(order-1))
  return(result-0.0000000001)
}

highFrequencyPortfolio=portfolio_create(fromTime=dateStart,toTime=dateEnd)
lowFrequencyportfolio=portfolio_create(fromTime=dateStart,toTime=dateEnd)

position=position_add(highFrequencyPortfolio,symbol,1)
price=compute(price(position))[[1]]
printTime=price[,1]

highFrequencyStrategy=array(0,dim=NROW(price))
highFrequencyStrategy[price[,"value"]>MA(price[,"value"],150)]<-100
lowFrequencyStrategy=array(0,dim=NROW(price))
lowFrequencyStrategy[price[,"value"]>MA(price[,"value"],800)]<-100

# Add position GOOG to portfolios
positionHF=position_add(portfolio=highFrequencyPortfolio,symbol=symbol,quantity=highFrequencyStrategy,time=printTime)
positionLF=position_add(lowFrequencyportfolio,symbol=symbol,quantity=lowFrequencyStrategy,time=printTime)

# Display general information about the portfolio at the end of a dataset
print(highFrequencyPortfolio)
print(lowFrequencyportfolio)
plot(lowFrequencyportfolio)
# util_screenshot('R-HFLF1.jpg')
############################################################
# Part 2 - Holding intervals visualization
############################################################
 
plot1<-util_ggplot(plot(quantity(positionHF),title="High Frequency Portfolio Strategy",line_size=0.6))
plot2<-util_ggplot(plot(quantity(positionLF),title="Low Frequency Portfolio Strategy",line_size=0.6))
grid.arrange(plot1,plot2,ncol=1)
# util_screenshot('R-HFLF2.jpg')
############################################################
# Part 3 - Trading strategy variance
############################################################

plot(variance(highFrequencyPortfolio),variance(lowFrequencyportfolio),title="Variance, daily",legend=c("HF Portfolio","LF Portfolio"))
# util_screenshot('R-HFLF3.jpg')
############################################################
# Part 4 - Trading strategy Value-at-Risk (daily, 95% c.i.)
############################################################

plot(value_at_risk(highFrequencyPortfolio,0.95),value_at_risk(lowFrequencyportfolio,0.95),title="Value at Risk in %, daily (95% c.i.)",legend=c("HF Portfolio","LF Portfolio"))
# util_screenshot('R-HFLF4.jpg')
############################################################
# Part 5 - Trading strategy Sharpe ratio (daily)
############################################################

plot(sharpe_ratio(highFrequencyPortfolio),sharpe_ratio(lowFrequencyportfolio),title="Sharpe Ratio, daily",legend=c("HF Portfolio","LF Portfolio"))
# util_screenshot('R-HFLF5.jpg')