\docType{methods}
\name{kurtosis}
\alias{kurtosis}
\alias{kurtosis,portfolio-method}
\alias{kurtosis,position-method}
\title{Kurtosis}
\usage{
kurtosis(asset)
}
\arguments{
  \item{asset}{Portfolio or Position object created using \link[=portfolio_create]{portfolio_create( )} or \link[=position_add]{position_add( )} function}
}
\value{Metric object}
\description{Computes kurtosis of portfolio returns.
}
\note{\url{https://www.portfolioeffect.com/docs/glossary/measures/absolute-risk-measures/kurtosis}
}
\author{Kostin Andrey <andrey.kostin@portfolioeffect.com>}
\seealso{\code{\link{skewness}} }
\examples{
\dontrun{
data(aapl.data) 
data(goog.data) 
data(spy.data) 
portfolio=portfolio_create(priceDataIx=spy.data)
portfolio_settings(portfolio,windowLength = '3600s',resultsSamplingInterval='60s')
positionGOOG=position_add(portfolio,'GOOG',100,priceData=goog.data)   
positionAAPL=position_add(portfolio,'AAPL',300,priceData=aapl.data) 
result=compute(kurtosis(portfolio),kurtosis(positionGOOG),kurtosis(positionAAPL)) 
plot(kurtosis(portfolio),kurtosis(positionGOOG),kurtosis(positionAAPL),
legend=c('Portfolio','GOOG','AAPL'),title='Kurtosis')

dateStart = "2014-11-17 09:30:00"
dateEnd = "2014-11-17 16:00:00"
portfolio=portfolio_create(dateStart,dateEnd)
portfolio_settings(portfolio,portfolioMetricsMode="price",windowLength = '3600s',
resultsSamplingInterval='60s')
positionAAPL=position_add(portfolio,'AAPL',100)
positionC=position_add(portfolio,'C',300) 
positionGOOG=position_add(portfolio,'GOOG',150) 
result=compute(kurtosis(positionC),kurtosis(positionGOOG),kurtosis(positionAAPL)) 
plot(kurtosis(positionC),kurtosis(positionGOOG),kurtosis(positionAAPL),
legend=c('C','GOOG','AAPL'),title='Kurtosis')
}}
\keyword{PortfolioEffectHFT}
%\concept{high frequency, intraday analytics, market data, portfolio, portfolio management,realtime analytics, risk, risk management, toolbox tools, trading, trading strategies}
\keyword{kurtosis}

