\docType{methods}
\name{correlation}
\alias{correlation}
\alias{correlation,portfolio,missing-method}
\alias{correlation,position,position-method}
\title{Correlation}
\usage{
%correlation(positionA,positionB)
\S4method{correlation}{portfolio,missing}(positionA)
\S4method{correlation}{position,position}(positionA,positionB)
}
\arguments{
  \item{positionA}{Position object created using \link[=position_add]{position_add( )} function}
  \item{positionB}{Position object created using \link[=position_add]{position_add( )} function}
}
\value{Metric object}
\description{Computes correlation between positionA and positionB.
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
result=compute(correlation(positionGOOG,positionAAPL))
plot(correlation(positionGOOG,positionAAPL),title='Correlation')

dateStart = "2014-11-17 09:30:00"
dateEnd = "2014-11-17 16:00:00"
portfolio=portfolio_create(dateStart,dateEnd)
portfolio_settings(portfolio,portfolioMetricsMode="price",windowLength = '3600s',
resultsSamplingInterval='60s')
positionAAPL=position_add(portfolio,'AAPL',100)
positionC=position_add(portfolio,'C',300) 
positionGOOG=position_add(portfolio,'GOOG',150) 
result=compute(correlation(positionC,positionAAPL))
plot(correlation(positionC,positionAAPL),title='Correlation')
}}
\keyword{PortfolioEffectHFT}
%\concept{high frequency, intraday analytics, market data, portfolio, portfolio management,realtime analytics, risk, risk management, toolbox tools, trading, trading strategies}
\keyword{correlation}
