############################################################
# Part 1 - Construct portfolio for optimization
############################################################

require(PortfolioEffectHFT)

portfolio=portfolio_create(fromTime="2014-04-13 9:30:01",
			   toTime="2014-04-16 16:00:00")
portfolio_settings(portfolio,windowLength = '360m',portfolioMetricsMode='price')
positionGOOG=position_add(portfolio,'GOOG',1)
positionAAPL=position_add(portfolio,'AAPL',1)
positionC=position_add(portfolio,'C',1)


plot(portfolio)
# util_screenshot('R-efficient_frontier1.jpg')
############################################################
# Part 2 - Compute theoretical efficient frontier
############################################################

portfolio_settings(portfolio,resultsSamplingInterval='last')

resultLintner=data.frame(Variance=0,ExpectedReturn=0)

for(x in seq(0.004,0.016,0.004)){
  optimizer=optimization_goal(variance(portfolio),"min")     
  optimizer=optimization_constraint(optimizer,value(portfolio),'=',10^9)
  optimizer=optimization_constraint(optimizer,expected_return(portfolio),"=",x)
  optimPortfolio=optimization_run(optimizer)
  resultLintner=rbind(resultLintner,c(compute(variance(optimPortfolio))[[1]][2],compute(expected_return(optimPortfolio))[[1]][2]))
}

resultLintner=resultLintner[-1,]

resultLintner=data.frame(Variance=spline(resultLintner$Variance, n=100)$y,
                         ExpectedReturn=spline(resultLintner$ExpectedReturn, n=100)$y)


ggplot()+geom_path(data=resultLintner, aes(x=Variance,y=ExpectedReturn),size=1.2)+util_plotTheme()+ggtitle("Efficient Frontier")+ylab("Expected Return")
# util_screenshot('R-efficient_frontier2.jpg')
##############################################################
# Part 3 - Compute efficient frontiers of realistic portfolios
##############################################################

resultLintner3000Portfolio=data.frame(Variance=0,ExpectedReturn=0)

for(x in seq(0.004,0.016,0.004)){
  optimizer=optimization_goal(variance(portfolio),"min")   
  optimizer=optimization_constraint(optimizer,value(portfolio),'=',3000)
  optimizer=optimization_constraint(optimizer,expected_return(portfolio),"=",x)
  optimPortfolio=optimization_run(optimizer)
  resultLintner3000Portfolio=rbind(resultLintner3000Portfolio,c(compute(variance(optimPortfolio))[[1]][2],compute(expected_return(optimPortfolio))[[1]][2]))
}

resultLintner3000Portfolio=resultLintner3000Portfolio[-1,]

resultLintner20000Portfolio=data.frame(Variance=0,ExpectedReturn=0)

for(x in seq(0.004,0.016,0.004)){
  optimizer=optimization_goal(variance(portfolio),"min")   
  optimizer=optimization_constraint(optimizer,value(portfolio),'=',20000)
  optimizer=optimization_constraint(optimizer,expected_return(portfolio),"=",x)
  optimPortfolio=optimization_run(optimizer)
  resultLintner20000Portfolio=rbind(resultLintner20000Portfolio,c(compute(variance(optimPortfolio))[[1]][2],compute(expected_return(optimPortfolio))[[1]][2]))
}

resultLintner20000Portfolio=resultLintner20000Portfolio[-1,]

resultLintner3000Portfolio$legend="$3000 Portfolio"
resultLintner20000Portfolio$legend="$20000 Portfolio"
resultLintner$legend="Theoretical Portfolio"
result=rbind(resultLintner3000Portfolio,resultLintner20000Portfolio,resultLintner)

ggplot()+geom_path(data=result, aes(x=Variance,y=ExpectedReturn,col=legend),size=1.2)+
util_plotTheme()+ggtitle("Efficient Frontier of Theoretical/$20000/$3000  portfolio")+
ylab("Expected Return")+util_colorScheme()
# util_screenshot('R-efficient_frontier3.jpg')
##############################################################
# Part 4 - Compare Markowitz and Lintner efficient frontiers
##############################################################

portfolio_settings(portfolio,
		   windowLength = '360m',
		   resultsSamplingInterval='last',
		   shortSalesMode = 'markowitz',portfolioMetricsMode='price')

resultMarkowitz=data.frame(Variance=0,ExpectedReturn=0)

for(x in seq(0.004,0.016,0.004)){
  optimizer=optimization_goal(variance(portfolio),"min")    
  optimizer=optimization_constraint(optimizer,value(portfolio),'=',10^9)
  optimizer=optimization_constraint(optimizer,expected_return(portfolio),"=",x)
  optimPortfolio=optimization_run(optimizer)
  resultMarkowitz=rbind(resultMarkowitz,c(compute(variance(optimPortfolio))[[1]][2],compute(expected_return(optimPortfolio))[[1]][2]))
}

resultMarkowitz=resultMarkowitz[-1,]

resultMarkowitz=data.frame(Variance=spline(resultMarkowitz$Variance, n=100)$y,
                           ExpectedReturn=spline(resultMarkowitz$ExpectedReturn, n=100)$y)

resultMarkowitz$legend="Markowitz"
resultLintner$legend="Lintner"
result=rbind(resultMarkowitz,resultLintner)

ggplot()+geom_path(data=result, aes(x=Variance,y=ExpectedReturn,col=legend),size=1.2)+
util_plotTheme()+ggtitle("Markowitz and Lintner Efficient Frontier")+ylab("Expected Return")+
util_colorScheme()
# util_screenshot('R-efficient_frontier4.jpg')