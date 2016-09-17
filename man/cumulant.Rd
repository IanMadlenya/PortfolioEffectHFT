\docType{methods}
\name{cumulant}
\alias{cumulant}
\alias{cumulant,portfolio-method}
\alias{cumulant,position-method}
\title{N-th Cumulant}
\usage{
cumulant(asset,order)
}
\arguments{
  \item{asset}{Portfolio or Position object created using \link[=portfolio_create]{portfolio_create( )} or \link[=position_add]{position_add( )} function}
  \item{order}{moment order (3 or 4).}
}
\value{Metric object}
\description{Computes N-th cumulant of portfolio return distribution.
}
\author{Kostin Andrey <andrey.kostin@portfolioeffect.com>}
\seealso{\code{\link{moment}} }
\examples{
\dontrun{
data(aapl.data) 
data(goog.data) 
data(spy.data) 
portfolio=portfolio_create(priceDataIx=spy.data)
portfolio_settings(portfolio,windowLength = '3600s',resultsSamplingInterval='60s')
positionGOOG=position_add(portfolio,'GOOG',100,priceData=goog.data)   
positionAAPL=position_add(portfolio,'AAPL',300,priceData=aapl.data) 
result=compute(cumulant(portfolio, 3),cumulant(positionGOOG, 3),cumulant(positionAAPL, 3)) 
plot(cumulant(portfolio, 3),cumulant(positionGOOG, 3),cumulant(positionAAPL, 3),
legend=c('Portfolio','GOOG','AAPL'),title='3-th Cumulant')

dateStart = "2014-11-17 09:30:00"
dateEnd = "2014-11-17 16:00:00"
portfolio=portfolio_create(dateStart,dateEnd)
portfolio_settings(portfolio,portfolioMetricsMode="price",windowLength = '3600s',
resultsSamplingInterval='60s')
positionAAPL=position_add(portfolio,'AAPL',100)
positionC=position_add(portfolio,'C',300) 
positionGOOG=position_add(portfolio,'GOOG',150) 
result=compute(cumulant(positionC, 4),cumulant(positionGOOG, 4),cumulant(positionAAPL, 4)) 
plot(cumulant(positionC, 4),cumulant(positionGOOG, 4),cumulant(positionAAPL, 4),
legend=c('C','GOOG','AAPL'),title='4-th Cumulant')
}}
\keyword{PortfolioEffectHFT}
%\concept{high frequency, intraday analytics, market data, portfolio, portfolio management,realtime analytics, risk, risk management, toolbox tools, trading, trading strategies}
\keyword{cumulant}
