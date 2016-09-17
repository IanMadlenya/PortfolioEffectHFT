\docType{methods}
\name{sortino_ratio}
\alias{sortino_ratio}
\alias{sortino_ratio,portfolio-method}
\alias{sortino_ratio,position-method}
\title{Sortina Ratio}
\usage{
sortino_ratio(asset, thresholdReturn)
}
\arguments{
  \item{asset}{Portfolio or Position object created using \link[=portfolio_create]{portfolio_create( )} or \link[=position_add]{position_add( )} function}
  \item{thresholdReturn}{Return value to be used as a cut-off point}
}
\value{Metric object}
\description{Computes Sortino ratio of a portfolio.
}
\note{\url{https://www.portfolioeffect.com/docs/glossary/measures/absolute-risk-adjusted-measures/sortino-ratio}
}
\author{Kostin Andrey <andrey.kostin@portfolioeffect.com>}
\seealso{\code{\link{sharpe_ratio}} \code{\link{calmar_ratio}} \code{\link{omega_ratio}} }
\examples{
\dontrun{
data(aapl.data) 
data(goog.data) 
data(spy.data) 
portfolio=portfolio_create(priceDataIx=spy.data)
portfolio_settings(portfolio,windowLength = '3600s',resultsSamplingInterval='60s')
positionGOOG=position_add(portfolio,'GOOG',100,priceData=goog.data)   
positionAAPL=position_add(portfolio,'AAPL',300,priceData=aapl.data) 
result=compute(sortino_ratio(portfolio,0.05),sortino_ratio(positionGOOG,0.05),
sortino_ratio(positionAAPL,0.05)) 
plot(sortino_ratio(portfolio,0.05),sortino_ratio(positionGOOG,0.05),
sortino_ratio(positionAAPL,0.05),legend=c('Portfolio','GOOG','AAPL'),title='Sortina Ratio')

dateStart = "2014-11-17 09:30:00"
dateEnd = "2014-11-17 16:00:00"
portfolio=portfolio_create(dateStart,dateEnd)
portfolio_settings(portfolio,portfolioMetricsMode="price",windowLength = '3600s',
resultsSamplingInterval='60s')
positionAAPL=position_add(portfolio,'AAPL',100)
positionC=position_add(portfolio,'C',300) 
positionGOOG=position_add(portfolio,'GOOG',150) 
result=compute(sortino_ratio(positionC,0.05),sortino_ratio(positionGOOG,0.05),
sortino_ratio(positionAAPL,0.05)) 
plot(sortino_ratio(positionC,0.05),sortino_ratio(positionGOOG,0.05),
sortino_ratio(positionAAPL,0.05),legend=c('C','GOOG','AAPL'),title='Sortina Ratio')
}}
\keyword{PortfolioEffectHFT}
%\concept{high frequency, intraday analytics, market data, portfolio, portfolio management,realtime analytics, risk, risk management, toolbox tools, trading, trading strategies}
\keyword{sortino_ratio}

