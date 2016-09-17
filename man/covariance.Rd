\docType{methods}
\name{covariance}
\alias{covariance}
\alias{covariance,portfolio,missing-method}
\alias{covariance,position,position-method}
\title{Covariance}
\usage{
%covariance(positionA,positionB)
\S4method{covariance}{portfolio,missing}(positionA)
\S4method{covariance}{position,position}(positionA,positionB)
}
\arguments{
  \item{positionA}{Position object created using \link[=position_add]{position_add( )} function}
  \item{positionB}{Position object created using \link[=position_add]{position_add( )} function}
}
\value{Metric object}
\description{Computes covariance between positionA and positionB.
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
result=compute(covariance(positionGOOG,positionAAPL))
plot(covariance(positionGOOG,positionAAPL),title='Covariance')

dateStart = "2014-11-17 09:30:00"
dateEnd = "2014-11-17 16:00:00"
portfolio=portfolio_create(dateStart,dateEnd)
portfolio_settings(portfolio,portfolioMetricsMode="price",windowLength = '3600s',
resultsSamplingInterval='60s')
positionAAPL=position_add(portfolio,'AAPL',100)
positionC=position_add(portfolio,'C',300) 
positionGOOG=position_add(portfolio,'GOOG',150) 
result=compute(covariance(positionC,positionAAPL))
plot(covariance(positionC,positionAAPL),title='Covariance')
}}
\keyword{PortfolioEffectHFT}
%\concept{high frequency, intraday analytics, market data, portfolio, portfolio management,realtime analytics, risk, risk management, toolbox tools, trading, trading strategies}
\keyword{covariance}
