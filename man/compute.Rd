\name{compute}
\alias{compute}
\title{Compute Metrics}
\usage{compute(...)
}
\arguments{
  \item{...}{One or multiple objects of class Metric.}
}
\value{One or multiple objects of class Metric.}
\description{Metric object is not evaluated until compute() method is called on it. 
Method would display calculation progress and would use Metric object's disk cache to store any computational results obtained in the process.}
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
result=compute(variance(portfolio),variance(positionGOOG),variance(positionAAPL)) 
result[[1]][1:10,]

result=compute(variance(portfolio)-variance(positionGOOG))
result[[1]][1:10,]

dateStart = "2014-11-17 09:30:00"
dateEnd = "2014-11-17 16:00:00"
portfolio=portfolio_create(dateStart,dateEnd)
portfolio_settings(portfolio,portfolioMetricsMode="price",windowLength = '3600s',
resultsSamplingInterval='60s')
positionAAPL=position_add(portfolio,'AAPL',100)
positionC=position_add(portfolio,'C',300) 
positionGOOG=position_add(portfolio,'GOOG',150) 
result=compute(variance(positionC),variance(positionGOOG),variance(positionAAPL)) 
result[[1]][1:10,]
}}
\keyword{PortfolioEffectHFT}
%\concept{high frequency, intraday analytics, market data, portfolio, portfolio management,realtime analytics, risk, risk management, toolbox tools, trading, trading strategies}
\keyword{compute}