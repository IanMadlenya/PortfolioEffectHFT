\docType{methods}
\name{alpha_exante}
\alias{alpha_exante}
\alias{alpha_exante,portfolio-method}
\alias{alpha_exante,position-method}
\title{Alpha}
\usage{
alpha_exante(asset)
}
\arguments{
  \item{asset}{Portfolio or Position object created using \link[=portfolio_create]{portfolio_create( )} or \link[=position_add]{position_add( )} function}
}
\value{Metric object}
\description{Computes monetary value of a portfolio or position from the beginning of the holding period.
}
\author{Kostin Andrey <andrey.kostin@portfolioeffect.com>}
\examples{
\dontrun{
data(aapl.data) 
data(goog.data) 
data(spy.data) 
portfolio=portfolio_create(priceDataIx=spy.data)
portfolio_settings(portfolio,windowLength = '600s',resultsSamplingInterval='60s')
positionGOOG=position_add(portfolio,'GOOG',100,priceData=goog.data)   
positionAAPL=position_add(portfolio,'AAPL',300,priceData=aapl.data)
result=compute(alpha_exante(portfolio),alpha_exante(positionGOOG),alpha_exante(positionAAPL))
plot(alpha_exante(portfolio),alpha_exante(positionGOOG),alpha_exante(positionAAPL),
legend=c('Portfolio','GOOG','AAPL'),title='Alpha')

dateStart = "2014-11-17 09:30:00"
dateEnd = "2014-11-17 16:00:00"
portfolio=portfolio_create(dateStart,dateEnd)
portfolio_settings(portfolio,portfolioMetricsMode="price",windowLength = '3600s',
resultsSamplingInterval='60s')
positionAAPL=position_add(portfolio,'AAPL',100)
positionC=position_add(portfolio,'C',300) 
positionGOOG=position_add(portfolio,'GOOG',150) 
result=compute(alpha_exante(positionC),alpha_exante(positionGOOG),alpha_exante(positionAAPL)) 
plot(alpha_exante(positionC),alpha_exante(positionGOOG),alpha_exante(positionAAPL),
legend=c('C','GOOG','AAPL'),title='Alpha')
}}
\keyword{PortfolioEffectHFT}
\keyword{alpha_exante}