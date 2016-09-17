############################################################
# Part 1 - Define trading signals and construct portfolios
############################################################

require(PortfolioEffectHFT)

# Create function of moving average
MA<-function(x,order){
  result<-x
  x1<-c(0,x)
  result[(order):NROW(x)]<-(cumsum(x1)[-(1:(order))]-cumsum(x1)[-((NROW(x1)-order+1):NROW(x1))])/order
  result[1:(order-1)]<-cumsum(x[1:(order-1)])/(1:(order-1))
  return(result-0.0000000001)
}

portfolio<-portfolio_create("2014-10-10 09:30:00","2014-10-10 16:00:00")

position_GOOG=position_add(portfolio,"GOOG",1)
goog<-compute(price(position_GOOG))[[1]]
printTime<-goog[,1]

# Create two strategy by moving average differents length
highFrequencyStrategy<-array(0,dim=NROW(goog))
highFrequencyStrategy[goog[,"value"]>MA(goog[,"value"],150)]<-100
lowFrequencyStrategy<-array(0,dim=NROW(goog))
lowFrequencyStrategy[goog[,"value"]>MA(goog[,"value"],800)]<-100


numberOfTransaction<-NULL
numberOfTransaction<-rbind(numberOfTransaction,data.frame(number=NROW(diff(highFrequencyStrategy)[diff(highFrequencyStrategy)!=0]),Legends="High Frequency Strategy"))
numberOfTransaction<-rbind(numberOfTransaction,data.frame(number=NROW(diff(lowFrequencyStrategy)[diff(lowFrequencyStrategy)!=0]),Legends="Low Frequency Strategy"))

xlabel<-""
ylabel<-"Number of transaction"
ggplot(numberOfTransaction, aes(x =Legends,y = number))+geom_bar(stat = "identity",fill="#014d64",width=.5)+xlab(xlabel)+ylab(ylabel)+  util_plotTheme()+util_fillScheme()+ggtitle("Number of Transaction")
# util_screenshot('R-transactionCosts1.jpg')
############################################################
# Part 2 - Holding Intervals Visualization
############################################################

highFrequencyPortfolioWithTransactionCosts<-portfolio_create("SPY","2014-10-10 09:30:00","2014-10-10 16:00:00")
portfolio_settings(highFrequencyPortfolioWithTransactionCosts,txnCostPerShare=0.02)
highFrequencyPortfolioWithTransactionCosts_GOOG=position_add(highFrequencyPortfolioWithTransactionCosts,"GOOG",quantity=as.numeric(highFrequencyStrategy),time=printTime)
highFrequencyPortfolioWithTransactionCosts

highFrequencyPortfolioWithoutTransactionCosts<-portfolio_create("SPY","2014-10-10 09:30:00","2014-10-10 16:00:00")
highFrequencyPortfolioWithoutTransactionCosts_GOOG<-position_add(highFrequencyPortfolioWithoutTransactionCosts,"GOOG",quantity=as.numeric(highFrequencyStrategy),time=printTime)
highFrequencyPortfolioWithoutTransactionCosts

lowFrequencyPortfolioWithTransactionCosts<-portfolio_create("SPY","2014-10-10 09:30:00","2014-10-10 16:00:00")
portfolio_settings(lowFrequencyPortfolioWithTransactionCosts,txnCostPerShare=0.02)
lowFrequencyPortfolioWithTransactionCosts_GOOG<-position_add(lowFrequencyPortfolioWithTransactionCosts,"GOOG",quantity=as.numeric(lowFrequencyStrategy),time=printTime)
lowFrequencyPortfolioWithTransactionCosts

lowFrequencyPortfolioWithoutTransactionCosts<-portfolio_create("SPY","2014-10-10 09:30:00","2014-10-10 16:00:00")
lowFrequencyPortfolioWithoutTransactionCosts_GOOG<-position_add(lowFrequencyPortfolioWithoutTransactionCosts,"GOOG",quantity=as.numeric(lowFrequencyStrategy),time=printTime)
lowFrequencyPortfolioWithoutTransactionCosts

# Plot portfolio strategy buy and sell changing over time 
plot1<-util_ggplot(plot(quantity(highFrequencyPortfolioWithTransactionCosts_GOOG),title="High Frequency Portfolio Strategy",line_size=0.6))
plot2<-util_ggplot(plot(quantity(lowFrequencyPortfolioWithTransactionCosts_GOOG),title="Low Frequency Portfolio Strategy",line_size=0.6))
 util_multiplot(plot1,plot2,cols=1)
 # util_screenshot('R-transactionCosts2.jpg')
############################################################
# Part 3 - Trading strategy variance
############################################################

# portfolio and position variance over time
plot(variance(highFrequencyPortfolioWithTransactionCosts),
     variance(highFrequencyPortfolioWithoutTransactionCosts)+1/700000,
     variance(lowFrequencyPortfolioWithTransactionCosts),
     variance(lowFrequencyPortfolioWithoutTransactionCosts)+1/700000,
     title="Variance, daily",legend=c("HF With Transaction Costs","HF Without Transaction Costs","LF With Transaction Costs","LF Without Transaction Costs"))
# util_screenshot('R-transactionCosts3.jpg')
############################################################
# Part 4 - Trading strategy expected return
############################################################

# portfolio and position return over time
plot(expected_return(highFrequencyPortfolioWithTransactionCosts),expected_return(highFrequencyPortfolioWithoutTransactionCosts),
     expected_return(lowFrequencyPortfolioWithTransactionCosts),expected_return(lowFrequencyPortfolioWithoutTransactionCosts),title="Expected Return, daily",
     legend=c("HF With Transaction Costs","HF Without Transaction Costs","LF With Transaction Costs","LF Without Transaction Costs"))
# util_screenshot('R-transactionCosts4.jpg')
############################################################
# Part 5 - Trading strategy Transactional Costs
############################################################

# portfolio and position Sharpe Ratio over time
plot(txn_costs(highFrequencyPortfolioWithTransactionCosts),
     txn_costs(highFrequencyPortfolioWithoutTransactionCosts),
     txn_costs(lowFrequencyPortfolioWithTransactionCosts),
     txn_costs(lowFrequencyPortfolioWithoutTransactionCosts)+10,
     title="Transactional Costs",legend=c("HF With Transaction Costs","HF Without Transaction Costs","LF With Transaction Costs","LF Without Transaction Costs"))

# util_screenshot('R-transactionCosts5.jpg')