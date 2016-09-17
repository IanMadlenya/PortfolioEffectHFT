############################################################
# Part 1 - Compute optimal weights using Treynor-Black model
############################################################

require(PortfolioEffectHFT)

# create test portfolio
timeStart="2014-10-02 09:30:00"
timeEnd="2014-10-03 16:00:00"

portfolio=portfolio_create("SPY", timeStart, timeEnd)
portfolio_settings(portfolio,portfolioMetricsMode="price",jumpsModel='all',resultsNAFilter='false')
positionAAPL=position_add(portfolio,"AAPL",100)
positionGOOG=position_add(portfolio,"GOOG",100)
positionSPY=position_add(portfolio,"SPY",100)

plot(alpha_jensens(positionAAPL),alpha_jensens(positionGOOG), title="Jensen's Alpha",legend=c("AAPL","GOOG"))
# util_screenshot('R-alpha-decay1.jpg')
# compute optimal weights according to the Treynor-Black model
timeUTC=compute(alpha_jensens(positionAAPL))[[1]][,1]
alpha=cbind (compute(alpha_jensens(positionAAPL))[[1]][,2], compute(alpha_jensens(positionGOOG))[[1]][,2])
variance=cbind(compute(variance(positionAAPL))[[1]][,2], compute(variance(positionGOOG))[[1]][,2])

treynorBlack=alpha/variance
optimWeigth=treynorBlack/rowSums(abs(treynorBlack))

# plot optimal position weights
plot(create_metric(cbind(timeUTC[!(is.na(optimWeigth[,1]))],optimWeigth[!(is.na(optimWeigth[,1])),1]),"AAPL"),
     create_metric(cbind(timeUTC[!(is.na(optimWeigth[,2]))], optimWeigth[!(is.na(optimWeigth[,2])),2]),"GOOG"), title="Optimal Weight")
# util_screenshot('R-alpha-decay2.jpg')
################################################
# Part 2- Compare two portfolios of equal value
################################################

# compute optimal position quatities for a portfolio of given size
portfolioCash=10000000
optimPosition=portfolioCash*optimWeigth/cbind(compute(price(positionAAPL))[[1]][,2],compute(price(positionGOOG))[[1]][,2])

portfolioSimple=portfolio_create("SPY", timeStart, timeEnd)
portfolio_settings(portfolioSimple,portfolioMetricsMode="price",jumpsModel='all')
positionAAPLSimple=position_add(portfolioSimple, "AAPL", quantity = (portfolioCash/2)%/%(compute(price(positionAAPL))[[1]][,2]), time = timeUTC)
positionGOOGSimple=position_add(portfolioSimple, "GOOG", quantity = (portfolioCash/2)%/%(compute(price(positionGOOG))[[1]][,2]), time = timeUTC)

portfolioOptimal=portfolio_create("SPY", timeStart, timeEnd)
portfolio_settings(portfolioOptimal,portfolioMetricsMode="price",jumpsModel='all')
positionAAPLOptimal=position_add(portfolioOptimal,"AAPL", quantity = optimPosition[,1], time = timeUTC)
positionGOOGOptimal=position_add(portfolioOptimal,"GOOG", quantity = optimPosition[,2], time = timeUTC)

meanAAPL=mean(compute(price(positionAAPLOptimal))[[1]][,2])
meanGOOG=mean(compute(price(positionGOOGOptimal))[[1]][,2])

portfolioSimpleAlpha=compute(alpha_jensens(portfolioSimple))[[1]]
portfolioOptimAlpha=compute(alpha_jensens(portfolioOptimal))[[1]]

plot(alpha_jensens(portfolioSimple),alpha_jensens(portfolioOptimal), title="Jensen's Alpha",legend=c("Simple portfolio","Optimal portfolio"))+
util_line2d(cbind(timeUTC, mean(portfolioSimpleAlpha[,2])), legend="Avg. of simple")+
util_line2d(cbind(timeUTC, mean(portfolioOptimAlpha[,2])), legend="Avg. of optimal")
# util_screenshot('R-alpha-decay3.jpg')
############################################################
# Part 3 - Use ARMA-class model to forecast alpha-decay
############################################################
require(forecast)
meanSimple=NULL
forecastErrorsSimple=NULL

#forecast test for simple portfolio
#use ARIMA(1,1,0). 1 second foreacast.
for(x1 in seq(0,2,0.1)){
  x2=1-x1
  set_quantity(positionAAPLSimple, (portfolioCash*x1)%/%meanAAPL)
  set_quantity(asset=positionGOOGSimple, quantity=(portfolioCash*x2)%/%meanGOOG)
  
  alpha=compute(alpha_jensens(portfolioSimple))[[1]]
  meanSimple=c(meanSimple,mean(alpha[,2]))
  
  forecastErrors=array(0,dim=100)
  for(i in 1:100){
    if((i/21+x1*1000/21)%%5==0){
      print(paste(i/21+x1*1000/21,"%"))
    }
    fit=arima(alpha[(3200+i*400):(3400+i*400-1),2],c(1,1,0)) #ARIMA estimation 
    forecastErrors[i]=abs(forecast(fit,1)$mean-alpha[3400+i*400,2])/mean(abs(alpha[(3200+i*400):(3400+i*400-1),2])) #Calculation of forecast errors 
  } 
  forecastErrorsSimple=c(forecastErrorsSimple,mean(forecastErrors))
}

#forecast test for optimal portfolio
for(i in 1:100){
  if(i%%5==0){
    print(paste(i,"%"))
  }
  fit=arima(portfolioOptimAlpha[(3200+i*400):(3400+i*400-1),2],c(1,1,0)) #ARIMA estimation 
  forecastErrors[i]=abs(forecast(fit,1)$mean-portfolioOptimAlpha[3400+i*400,2])/abs(mean(portfolioOptimAlpha[(3200+i*400):(3400+i*400-1),2])) #Calculation of forecast errors 
} 
forecastErrorsOptim=mean(forecastErrors) #Calculation of average values 

resultdf=data.frame(err=c(forecastErrorsSimple,forecastErrorsOptim) ,mean=c(meanSimple,mean(portfolioOptimAlpha[,2])),legend=c(array("Portfolio Simple Alpha",dim=21),"Portfolio Optimal Alpha"))

ggplot() + geom_point(data=resultdf, aes(x=err, y=mean,colour=legend),size=5) +
  xlab("Forecast Error(%)") + 
  ylab("Alpha mean") +
  util_plotTheme(base_size = 15)+util_colorScheme()
# util_screenshot('R-alpha-decay4.jpg')