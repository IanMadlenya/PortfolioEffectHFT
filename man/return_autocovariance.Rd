\docType{methods}
\name{return_autocovariance}
\alias{return_autocovariance}
\alias{return_autocovariance,position-method}
\title{Return Autocovariance}
\usage{
return_autocovariance(asset, lag)
}
\arguments{
  \item{asset}{ Position object created using \link[=position_add]{position_add( )} function}
  \item{lag}{Time lag (in seconds) between observations in question.}
}

\value{Metric object}
\description{Computes autocovariance of position returns for a certain time lag.
}
\author{Kostin Andrey <andrey.kostin@portfolioeffect.com>}
\examples{
\dontrun{
data(aapl.data) 
data(goog.data) 
data(spy.data) 
portfolio=portfolio_create(priceDataIx=spy.data)
portfolio_settings(portfolio,windowLength = '3600s',resultsSamplingInterval='60s')
positionGOOG=position_add(portfolio,'GOOG',100,priceData=goog.data)   
positionAAPL=position_add(portfolio,'AAPL',300,priceData=aapl.data) 
result=compute(return_autocovariance(positionGOOG,10),return_autocovariance(positionAAPL,10)) 
plot(return_autocovariance(positionGOOG,10),return_autocovariance(positionAAPL,10),
legend=c('GOOG','AAPL'),title='Return Autocovariance')

dateStart = "2014-11-17 09:30:00"
dateEnd = "2014-11-17 16:00:00"
portfolio=portfolio_create(dateStart,dateEnd)
portfolio_settings(portfolio,portfolioMetricsMode="price",windowLength = '3600s',
resultsSamplingInterval='60s')
positionAAPL=position_add(portfolio,'AAPL',100)
positionC=position_add(portfolio,'C',300) 
positionGOOG=position_add(portfolio,'GOOG',150) 
result=compute(return_autocovariance(positionC,10),return_autocovariance(positionGOOG,10),
return_autocovariance(positionAAPL,10)) 
plot(return_autocovariance(positionC,10),return_autocovariance(positionGOOG,10),
return_autocovariance(positionAAPL,10),legend=c('C','GOOG','AAPL'),title='Return Autocovariance')
}}
\keyword{PortfolioEffectHFT}
%\concept{high frequency, intraday analytics, market data, portfolio, portfolio management,realtime analytics, risk, risk management, toolbox tools, trading, trading strategies}
\keyword{return_autocovariance}