\name{util_POSIXTimeToDate}
\alias{util_POSIXTimeToDate}
\title{POSIX Time To Date}
\usage{util_POSIXTimeToDate(time)
}
\arguments{
  \item{time}{One dimensional vector of milliseconds since the beginning of epoch.}  
}
\value{One dimensional vector of time values in "yyyy-MM-dd hh:mm:ss" string format.
}
\description{Converts timestamps in milliseconds to corresponding date strings.}
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
util_POSIXTimeToDate(compute(kurtosis(portfolio))[[1]][,1])
}}
\keyword{PortfolioEffectHFT}
%\concept{ market data}
\keyword{util_POSIXTimeToDate}