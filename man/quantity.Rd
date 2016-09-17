\docType{methods}
\name{quantity}
\alias{quantity}
\alias{quantity,position-method}
\title{Quantity}
\usage{
quantity(asset)
}
\arguments{
  \item{asset}{Position object created using \link[=position_add]{position_add( )} function}
}
\value{Metric object}
\description{Returns total number of shares associated with the given symbol in this portfolio.
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
result=compute(quantity(positionGOOG),quantity(positionAAPL)) 
plot(quantity(positionGOOG),quantity(positionAAPL),legend=c('GOOG','AAPL'),title='Quantity')

dateStart = "2014-11-17 09:30:00"
dateEnd = "2014-11-17 16:00:00"
portfolio=portfolio_create(dateStart,dateEnd)
portfolio_settings(portfolio,portfolioMetricsMode="price",windowLength = '3600s',
resultsSamplingInterval='60s')
positionAAPL=position_add(portfolio,'AAPL',100)
positionC=position_add(portfolio,'C',300) 
positionGOOG=position_add(portfolio,'GOOG',150) 
result=compute(quantity(positionC),quantity(positionGOOG),quantity(positionAAPL)) 
plot(quantity(positionC),quantity(positionGOOG),quantity(positionAAPL),
legend=c('C','GOOG','AAPL'),title='Quantity')
}}
\keyword{PortfolioEffectHFT}
%\concept{high frequency, intraday analytics, market data, portfolio, portfolio management,realtime analytics, risk, risk management, toolbox tools, trading, trading strategies}
\keyword{quantity}