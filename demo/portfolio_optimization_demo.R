############################################################
# Part 1 - Construct buy-and-hold portfolio
############################################################

require(PortfolioEffectHFT)

portfolio<-portfolio_create("SPY", "2014-11-19 09:30:00", "2014-11-19 16:00:00")
portfolio_settings(portfolio,portfolioMetricsMode="price",resultsSamplingInterval='1m')
position_AAPL=position_add(portfolio,"AAPL",1000)
position_GOOG=position_add(portfolio,"GOOG",1000)
position_SPY=position_add(portfolio,"SPY",1000)


plot(portfolio)
# util_screenshot('R-simpleOptimization1.jpg')

############################################################
# Part 2 - Run portfolio optimization with one constraint
############################################################

optimizer=optimization_goal(variance(portfolio),direction="min")
optimizer=optimization_constraint(optimizer,value(portfolio),'=',10^9)
optimizer=optimization_constraint(optimizer,expected_return(portfolio),">=",0)
optimPortfolioOneConstraints=optimization_run(optimizer)

plot(optimPortfolioOneConstraints)
# util_screenshot('R-simpleOptimization2.jpg')

############################################################
# Part 3 - Run portfolio optimization with two constraints
############################################################

optimizer=optimization_constraint(optimizer,weight_transform(portfolio,"sum_abs_weight",c(position_AAPL,position_GOOG)),">=",0.5)

optimPortfolioTwoConstraints=optimization_run(optimizer)
plot(optimPortfolioTwoConstraints)
# util_screenshot('R-simpleOptimization3.jpg')

############################################################
# Part 4 - Compute optimal portfolio expected return
############################################################

plot(expected_return(optimPortfolioOneConstraints),"Portfolio Expected Return",legend="Optimal Portfolio, with Constraints:\nReturn>=0")+
  plot(expected_return(optimPortfolioTwoConstraints),legend="Optimal Portfolio, with Constraints:\nReturn>=0, Sum of Abs Weights AAPL and GOOG >=0.5")
# util_screenshot('R-simpleOptimization4.jpg')
############################################################
# Part 5 - Compute optimal portfolio variance
############################################################

plot(variance(optimPortfolioOneConstraints),"Portfolio Variance",legend="Optimal Portfolio, with Constraints:\nReturn>=0")+
  plot(variance(optimPortfolioTwoConstraints),legend="Optimal Portfolio, with Constraints:\nReturn>=0, \nSum of Abs Weights AAPL and GOOG >=0.5")
# util_screenshot('R-simpleOptimization5.jpg')
############################################################
# Part 6 - Compute optimal portfolio sum of absolute weights
############################################################


sumOfAbsWeightsOptimPortfolio=abs(compute(weight(portfolio_getPosition(optimPortfolioOneConstraints,"AAPL")))[[1]][,2])+abs(compute(weight(portfolio_getPosition(optimPortfolioOneConstraints,"GOOG")))[[1]][,2])
sumOfAbsWeightsOptimPortfolioTwoConstraints=abs(compute(weight(portfolio_getPosition(optimPortfolioTwoConstraints,"AAPL")))[[1]][,2])+abs(compute(weight(portfolio_getPosition(optimPortfolioTwoConstraints,"GOOG")))[[1]][,2])
timeUTC <- compute(weight(portfolio_getPosition(optimPortfolioOneConstraints,"AAPL")))[[1]][,1]

util_plot2d(cbind(timeUTC,sumOfAbsWeightsOptimPortfolio),"Portfolio Sum Of Abs Weigth AAPL and GOOG",legend="Optimal Portfolio, with Constraints:\nReturn>=0")+
  util_line2d(cbind(timeUTC,sumOfAbsWeightsOptimPortfolioTwoConstraints),legend="Optimal Portfolio, with Constraints:\nReturn>=0, \nSum of Abs Weights AAPL and GOOG >=0.5")
# util_screenshot('R-simpleOptimization6.jpg')