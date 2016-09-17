\name{dist_density}
\alias{dist_density}
\alias{dist_density,portfolio-method}
\alias{dist_density,position-method}
\title{Probability Density Function of Portfolio Returns}
\usage{
dist_density(asset,pValueLeft,pValueRight,nPoints,addNormalDensity)
}
\arguments{
  \item{asset}{Portfolio or Position object created using \link[=portfolio_create]{portfolio_create( )} or \link[=position_add]{position_add( )} function}
  \item{pValueLeft}{Left limit of probability density value in decimals.}
  \item{pValueRight}{Right limit of probability density value in decimals.}
  \item{nPoints}{Number of approximation points for the PDF function.}
  \item{addNormalDensity}{Flag used to add normal density to the final result. Defaults to FALSE.}
}
\value{List of probability density values}
\description{Computes probability density of portfolio returns for a given interval (pValueLeft, pValueRight) at nPoints of approximation. 
Probability density is computed based on a "densityModel" specified in \link[=portfolio_settings]{portfolio_settings( )} method.}
\author{Kostin Andrey <andrey.kostin@portfolioeffect.com>}
\examples{
\dontrun{
%change
data(aapl.data) 
data(goog.data) 
data(spy.data) 
portfolio=portfolio_create(priceDataIx=spy.data)
portfolio_settings(portfolio,windowLength = '3600s',resultsSamplingInterval='60s')
positionGOOG=position_add(portfolio,'GOOG',100,priceData=goog.data)   
positionAAPL=position_add(portfolio,'AAPL',300,priceData=aapl.data) 
util_plotDensity(dist_density(portfolio,pValueLeft=0.2,pValueRight=0.8,nPoints=100,
addNormalDensity=TRUE))

dateStart = "2014-11-17 09:30:00"
dateEnd = "2014-11-17 16:00:00"
portfolio=portfolio_create(dateStart,dateEnd)
portfolio_settings(portfolio,portfolioMetricsMode="price",windowLength = '3600s',
resultsSamplingInterval='60s')
positionAAPL=position_add(portfolio,'AAPL',100)
positionC=position_add(portfolio,'C',300) 
positionGOOG=position_add(portfolio,'GOOG',150) 
util_plotDensity(dist_density(portfolio,pValueLeft=0,pValueRight=1,nPoints=100,
addNormalDensity=TRUE))
}}
\keyword{PortfolioEffectHFT}
%\concept{high frequency, intraday analytics, market data, portfolio, portfolio management,realtime analytics, risk, risk management, toolbox tools, trading, trading strategies}
\keyword{dist_density}