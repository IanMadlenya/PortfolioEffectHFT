\docType{methods}
\name{rachev_ratio}
\alias{rachev_ratio}
\alias{rachev_ratio,portfolio-method}
\alias{rachev_ratio,position-method}
\title{Rachev Ratio}
\usage{
rachev_ratio(asset, confidenceIntervalA = 0.95,
  confidenceIntervalB = 0.95)
}
\arguments{
  \item{asset}{Portfolio or Position object created using \link[=portfolio_create]{portfolio_create( )} or \link[=position_add]{position_add( )} function}
  \item{confidenceIntervalA}{Confidence interval (in decimals) to be used as a cut-off point in the numerator}
  \item{confidenceIntervalB}{Confidence interval (in decimals) to be used as a cut-off point in the denominator}
}
\value{Metric object}
\description{
Computes Rachev ratio of a portfolio at given confidence intervals.
Computation employs distribution skewness and kurtosis to account for non-normality.
}
\note{\url{https://www.portfolioeffect.com/docs/glossary/measures/tail-risk-measures/rachev-ratio}
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
result=compute(rachev_ratio(portfolio,0.95,0.95),rachev_ratio(positionGOOG,0.95,0.95),
rachev_ratio(positionAAPL,0.95,0.95)) 
plot(rachev_ratio(portfolio,0.95,0.95),rachev_ratio(positionGOOG,0.95,0.95),
rachev_ratio(positionAAPL,0.95,0.95),legend=c('Portfolio','GOOG','AAPL'),title='Rachev Ratio')

dateStart = "2014-11-17 09:30:00"
dateEnd = "2014-11-17 16:00:00"
portfolio=portfolio_create(dateStart,dateEnd)
portfolio_settings(portfolio,portfolioMetricsMode="price",windowLength = '3600s',
resultsSamplingInterval='60s')
positionAAPL=position_add(portfolio,'AAPL',100)
positionC=position_add(portfolio,'C',300) 
positionGOOG=position_add(portfolio,'GOOG',150) 
result=compute(rachev_ratio(positionC,0.95,0.95),rachev_ratio(positionGOOG,0.95,0.95),
rachev_ratio(positionAAPL,0.95,0.95)) 
plot(rachev_ratio(positionC,0.95,0.95),rachev_ratio(positionGOOG,0.95,0.95),
rachev_ratio(positionAAPL,0.95,0.95),legend=c('C','GOOG','AAPL'),title='Rachev Ratio')
}}
\keyword{PortfolioEffectHFT}
%\concept{high frequency, intraday analytics, market data, portfolio, portfolio management,realtime analytics, risk, risk management, toolbox tools, trading, trading strategies}
\keyword{rachev_ratio}