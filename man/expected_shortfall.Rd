\docType{methods}
\name{expected_shortfall}
\alias{expected_shortfall}
\alias{expected_shortfall,portfolio-method}
\alias{expected_shortfall,position-method}
\title{Expected Shortfall}
\usage{
expected_shortfall(asset, confidenceInterval)
}
\arguments{
  \item{asset}{Portfolio or Position object created using \link[=portfolio_create]{portfolio_create( )} or \link[=position_add]{position_add( )} function}
  \item{confidenceInterval}{ Confidence interval (in decimals) to be used as a cut-off point }
}
\value{Metric object}
\description{Computes portfolio conditional Value-at-Risk (Expected Tail Loss) at a given confidence interval.
Computation employs distribution's skewness and kurtosis to account for non-normality.
}
\note{\url{https://www.portfolioeffect.com/docs/glossary/measures/tail-risk-measures/cvar}
}
\author{Kostin Andrey <andrey.kostin@portfolioeffect.com>}
\seealso{\code{\link{value_at_risk}} }
\examples{
\dontrun{
data(aapl.data) 
data(goog.data) 
data(spy.data) 
portfolio=portfolio_create(priceDataIx=spy.data)
portfolio_settings(portfolio,windowLength = '3600s',resultsSamplingInterval='60s')
positionGOOG=position_add(portfolio,'GOOG',100,priceData=goog.data)   
positionAAPL=position_add(portfolio,'AAPL',300,priceData=aapl.data) 
result=compute(expected_shortfall(portfolio,0.95),expected_shortfall(positionGOOG,0.95),
expected_shortfall(positionAAPL,0.95)) 
plot(expected_shortfall(portfolio,0.95),expected_shortfall(positionGOOG,0.95),
expected_shortfall(positionAAPL,0.95),legend=c('Portfolio','GOOG','AAPL'),
title='Expected Shortfall')

dateStart = "2014-11-17 09:30:00"
dateEnd = "2014-11-17 16:00:00"
portfolio=portfolio_create(dateStart,dateEnd)
portfolio_settings(portfolio,portfolioMetricsMode="price",windowLength = '3600s',
resultsSamplingInterval='60s')
positionAAPL=position_add(portfolio,'AAPL',100)
positionC=position_add(portfolio,'C',300) 
positionGOOG=position_add(portfolio,'GOOG',150) 
result=compute(expected_shortfall(positionC,0.95),expected_shortfall(positionGOOG,0.95),
expected_shortfall(positionAAPL,0.95)) 
plot(expected_shortfall(positionC,0.95),expected_shortfall(positionGOOG,0.95),
expected_shortfall(positionAAPL,0.95),legend=c('C','GOOG','AAPL'),
title='Expected Shortfall')
}}
\keyword{PortfolioEffectHFT}
%\concept{high frequency, intraday analytics, market data, portfolio, portfolio management,realtime analytics, risk, risk management, toolbox tools, trading, trading strategies}
\keyword{expected_shortfall}

