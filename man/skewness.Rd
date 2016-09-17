\docType{methods}
\name{skewness}
\alias{skewness}
\alias{skewness,portfolio-method}
\alias{skewness,position-method}
\title{Skewness}
\usage{
skewness(asset)
}
\arguments{
  \item{asset}{Portfolio or Position object created using \link[=portfolio_create]{portfolio_create( )} or \link[=position_add]{position_add( )} function}
}
\value{Metric object}
\description{Computes skewness of portfolio returns.
}
\note{\url{https://www.portfolioeffect.com/docs/glossary/measures/absolute-risk-measures/skewness}
}
\author{Kostin Andrey <andrey.kostin@portfolioeffect.com>}
\seealso{\code{\link{kurtosis}} }
\examples{
\dontrun{
data(aapl.data) 
data(goog.data) 
data(spy.data) 
portfolio=portfolio_create(priceDataIx=spy.data)
portfolio_settings(portfolio,windowLength = '3600s',resultsSamplingInterval='60s')
positionGOOG=position_add(portfolio,'GOOG',100,priceData=goog.data)   
positionAAPL=position_add(portfolio,'AAPL',300,priceData=aapl.data) 
result=compute(skewness(portfolio),skewness(positionGOOG),skewness(positionAAPL)) 
plot(skewness(portfolio),skewness(positionGOOG),skewness(positionAAPL),
legend=c('Portfolio','GOOG','AAPL'),title='Skewness')

dateStart = "2014-11-17 09:30:00"
dateEnd = "2014-11-17 16:00:00"
portfolio=portfolio_create(dateStart,dateEnd)
portfolio_settings(portfolio,portfolioMetricsMode="price",windowLength = '3600s',
resultsSamplingInterval='60s')
positionAAPL=position_add(portfolio,'AAPL',100)
positionC=position_add(portfolio,'C',300) 
positionGOOG=position_add(portfolio,'GOOG',150) 
result=compute(skewness(positionC),skewness(positionGOOG),skewness(positionAAPL)) 
plot(skewness(positionC),skewness(positionGOOG),skewness(positionAAPL),
legend=c('C','GOOG','AAPL'),title='Skewness')
}}
\keyword{PortfolioEffectHFT}
%\concept{high frequency, intraday analytics, market data, portfolio, portfolio management,realtime analytics, risk, risk management, toolbox tools, trading, trading strategies}
\keyword{skewness}

