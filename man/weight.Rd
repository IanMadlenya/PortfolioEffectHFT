\docType{methods}
\name{weight}
\alias{weight}
\alias{weight,portfolio-method}
\alias{weight,position-method}
\title{Weight}
\usage{
weight(asset)
}
\arguments{
  \item{asset}{Position object created using \link[=position_add]{position_add( )} function}
}
\value{Metric object}
\description{Computes ratio of a monetary position value to the monetary value of the whole portfolio. 
Expressed in decimal points of portfolio value.
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
result=compute(weight(positionGOOG),weight(positionAAPL)) 
plot(weight(positionGOOG),weight(positionAAPL),legend=c('GOOG','AAPL'),title='Weight')

dateStart = "2014-11-17 09:30:00"
dateEnd = "2014-11-17 16:00:00"
portfolio=portfolio_create(dateStart,dateEnd)
portfolio_settings(portfolio,portfolioMetricsMode="price",windowLength = '3600s',
resultsSamplingInterval='60s')
positionAAPL=position_add(portfolio,'AAPL',100)
positionC=position_add(portfolio,'C',300) 
positionGOOG=position_add(portfolio,'GOOG',150) 
result=compute(weight(positionC),weight(positionGOOG),weight(positionAAPL)) 
plot(weight(positionC),weight(positionGOOG),weight(positionAAPL),
legend=c('C','GOOG','AAPL'),title='Weight')
}}
\keyword{PortfolioEffectHFT}
%\concept{high frequency, intraday analytics, market data, portfolio, portfolio management,realtime analytics, risk, risk management, toolbox tools, trading, trading strategies}
\keyword{weight}