\docType{methods}
\name{hurst_exponent}
\alias{hurst_exponent}
\alias{hurst_exponent,portfolio-method}
\alias{hurst_exponent,position-method}
\title{Hurst Exponent}
\usage{
hurst_exponent(asset)
}
\arguments{
  \item{asset}{Portfolio or Position object created using \link[=portfolio_create]{portfolio_create( )} or \link[=position_add]{position_add( )} function}
}
\value{Metric object}
\description{Computes portfolio Hurst exponent as a weighted sum of the Hurst exponents of its position returns.
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
result=compute(hurst_exponent(portfolio),hurst_exponent(positionGOOG),hurst_exponent(positionAAPL)) 
plot(hurst_exponent(portfolio),hurst_exponent(positionGOOG),hurst_exponent(positionAAPL),
legend=c('Portfolio','GOOG','AAPL'),title='Hurst Exponent')

dateStart = "2014-11-17 09:30:00"
dateEnd = "2014-11-17 16:00:00"
portfolio=portfolio_create(dateStart,dateEnd)
portfolio_settings(portfolio,portfolioMetricsMode="price",windowLength = '3600s',
resultsSamplingInterval='60s')
positionAAPL=position_add(portfolio,'AAPL',100)
positionC=position_add(portfolio,'C',300) 
positionGOOG=position_add(portfolio,'GOOG',150) 
result=compute(hurst_exponent(positionC),hurst_exponent(positionGOOG),hurst_exponent(positionAAPL)) 
plot(hurst_exponent(positionC),hurst_exponent(positionGOOG),hurst_exponent(positionAAPL),
legend=c('C','GOOG','AAPL'),title='Hurst Exponent')
}}
\keyword{PortfolioEffectHFT}
%\concept{high frequency, intraday analytics, market data, portfolio, portfolio management,realtime analytics, risk, risk management, toolbox tools, trading, trading strategies}
\keyword{hurst_exponent}