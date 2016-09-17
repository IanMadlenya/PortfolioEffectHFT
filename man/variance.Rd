\docType{methods}
\name{variance}
\alias{variance}
\alias{variance,portfolio-method}
\alias{variance,position-method}
\title{Variance}
\usage{
variance(asset)
}
\arguments{
  \item{asset}{Portfolio or Position object created using \link[=portfolio_create]{portfolio_create( )} or \link[=position_add]{position_add( )} function}
}
\value{Metric object}
\description{Computes variance of portfolio returns.
}
\note{\url{https://www.portfolioeffect.com/docs/glossary/measures/absolute-risk-measures/variance}
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
result=compute(variance(portfolio),variance(positionGOOG),variance(positionAAPL)) 
plot(variance(portfolio),variance(positionGOOG),variance(positionAAPL),
legend=c('Portfolio','GOOG','AAPL'),title='Variance')

dateStart = "2014-11-17 09:30:00"
dateEnd = "2014-11-17 16:00:00"
portfolio=portfolio_create(dateStart,dateEnd)
portfolio_settings(portfolio,portfolioMetricsMode="price",windowLength = '3600s',
resultsSamplingInterval='60s')
positionAAPL=position_add(portfolio,'AAPL',100)
positionC=position_add(portfolio,'C',300) 
positionGOOG=position_add(portfolio,'GOOG',150) 
result=compute(variance(positionC),variance(positionGOOG),variance(positionAAPL)) 
plot(variance(positionC),variance(positionGOOG),variance(positionAAPL),
legend=c('C','GOOG','AAPL'),title='Variance')
}}
\keyword{PortfolioEffectHFT}
%\concept{high frequency, intraday analytics, market data, portfolio, portfolio management,realtime analytics, risk, risk management, toolbox tools, trading, trading strategies}
\keyword{variance}