\docType{methods}
\name{moment}
\alias{moment}
\alias{moment,portfolio-method}
\alias{moment,position-method}
\title{N-th Order Central Moment}
\usage{
moment(asset, order)
}
\arguments{
  \item{asset}{Portfolio or Position object created using \link[=portfolio_create]{portfolio_create( )} or \link[=position_add]{position_add( )} function}
  \item{order}{moment order (from 1 to 4)}
}
\value{Metric object}
\description{Computes N-th order central moment of portfolio return distribution.
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
result=compute(moment(portfolio, 3),moment(positionGOOG, 3),moment(positionAAPL, 3)) 
plot(moment(portfolio, 3),moment(positionGOOG, 3),moment(positionAAPL, 3),
legend=c('Portfolio','GOOG','AAPL'),title='3-th Order Central Moment')

dateStart = "2014-11-17 09:30:00"
dateEnd = "2014-11-17 16:00:00"
portfolio=portfolio_create(dateStart,dateEnd)
portfolio_settings(portfolio,portfolioMetricsMode="price",windowLength = '3600s',
resultsSamplingInterval='60s')
positionAAPL=position_add(portfolio,'AAPL',100)
positionC=position_add(portfolio,'C',300) 
positionGOOG=position_add(portfolio,'GOOG',150) 
result=compute(moment(positionC,4),moment(positionGOOG,4),moment(positionAAPL,4)) 
plot(moment(positionC,4),moment(positionGOOG,4),moment(positionAAPL,4),
legend=c('C','GOOG','AAPL'),title='4-th Order Central Moment')
}}
\keyword{PortfolioEffectHFT}
%\concept{high frequency, intraday analytics, market data, portfolio, portfolio management,realtime analytics, risk, risk management, toolbox tools, trading, trading strategies}
\keyword{moment}

