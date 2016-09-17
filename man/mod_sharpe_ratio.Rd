\docType{methods}
\name{mod_sharpe_ratio}
\alias{mod_sharpe_ratio}
\alias{mod_sharpe_ratio,portfolio-method}
\alias{mod_sharpe_ratio,position-method}
\title{Modified Sharpe Ratio}
\usage{
mod_sharpe_ratio(asset, confidenceInterval)
}
\arguments{
  \item{asset}{Portfolio or Position object created using \link[=portfolio_create]{portfolio_create( )} or \link[=position_add]{position_add( )} function}
  \item{confidenceInterval}{ Confidence interval (in decimals) to be used as a cut-off point }
}
\value{Metric object}
\description{Computes Modified Sharpe Ratio of a portfolio at a given confidence interval. 
Computation employs distribution's skewness and kurtosis to account for non-normality.
}
\note{\url{https://www.portfolioeffect.com/docs/glossary/measures/tail-risk-measures/modified-sharpe-ratio.php}
}
\author{Kostin Andrey <andrey.kostin@portfolioeffect.com>}
\seealso{ \code{\link{sharpe_ratio}}}
\examples{
\dontrun{
data(aapl.data) 
data(goog.data) 
data(spy.data) 
portfolio=portfolio_create(priceDataIx=spy.data)
portfolio_settings(portfolio,windowLength = '3600s',resultsSamplingInterval='60s')
positionGOOG=position_add(portfolio,'GOOG',100,priceData=goog.data)   
positionAAPL=position_add(portfolio,'AAPL',300,priceData=aapl.data) 
result=compute(mod_sharpe_ratio(portfolio,0.95),mod_sharpe_ratio(positionGOOG,0.95),
mod_sharpe_ratio(positionAAPL,0.95)) 
plot(mod_sharpe_ratio(portfolio,0.95),mod_sharpe_ratio(positionGOOG,0.95),
mod_sharpe_ratio(positionAAPL,0.95),legend=c('Portfolio','GOOG','AAPL'),
title='Modified Sharpe Ratio')

dateStart = "2014-11-17 09:30:00"
dateEnd = "2014-11-17 16:00:00"
portfolio=portfolio_create(dateStart,dateEnd)
portfolio_settings(portfolio,portfolioMetricsMode="price",windowLength = '3600s',
resultsSamplingInterval='60s')
positionAAPL=position_add(portfolio,'AAPL',100)
positionC=position_add(portfolio,'C',300) 
positionGOOG=position_add(portfolio,'GOOG',150) 
result=compute(mod_sharpe_ratio(positionC,0.95),mod_sharpe_ratio(positionGOOG,0.95),
mod_sharpe_ratio(positionAAPL,0.95)) 
plot(mod_sharpe_ratio(positionC,0.95),mod_sharpe_ratio(positionGOOG,0.95),
mod_sharpe_ratio(positionAAPL,0.95),legend=c('C','GOOG','AAPL'),
title='Modified Sharpe Ratio')
}}
\keyword{PortfolioEffectHFT}
%\concept{high frequency, intraday analytics, market data, portfolio, portfolio management,realtime analytics, risk, risk management, toolbox tools, trading, trading strategies}
\keyword{mod_sharpe_ratio}
