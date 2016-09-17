\docType{methods}
\name{omega_ratio}
\alias{omega_ratio}
\alias{omega_ratio,portfolio-method}
\alias{omega_ratio,position-method}
\title{Omega Ratio}
\usage{
omega_ratio(asset, thresholdReturn)
}
\arguments{
  \item{asset}{Portfolio or Position object created using \link[=portfolio_create]{portfolio_create( )} or \link[=position_add]{position_add( )} function}
  \item{thresholdReturn}{Return value to be used as a cut-off point}
}
\value{Metric object}
\description{Computes Omega Ratio of a portfolio.
Computation employs distribution's skewness and kurtosis to account for non-normality.
}
\note{\url{https://www.portfolioeffect.com/docs/glossary/measures/absolute-risk-adjusted-measures/omega-ratio}
}
\author{Kostin Andrey <andrey.kostin@portfolioeffect.com>}
\seealso{\code{\link{sharpe_ratio}} \code{\link{sortino_ratio}} \code{\link{calmar_ratio}} }
\examples{
\dontrun{
data(aapl.data) 
data(goog.data) 
data(spy.data) 
portfolio=portfolio_create(priceDataIx=spy.data)
portfolio_settings(portfolio,windowLength = '3600s',resultsSamplingInterval='60s')
positionGOOG=position_add(portfolio,'GOOG',100,priceData=goog.data)   
positionAAPL=position_add(portfolio,'AAPL',300,priceData=aapl.data) 
result=compute(omega_ratio(portfolio,0.05),omega_ratio(positionGOOG,0.05),
omega_ratio(positionAAPL,0.05)) 
plot(omega_ratio(portfolio,0.05),omega_ratio(positionGOOG,0.05),
omega_ratio(positionAAPL,0.05),legend=c('Portfolio','GOOG','AAPL'),title='Omega Ratio')

dateStart = "2014-11-17 09:30:00"
dateEnd = "2014-11-17 16:00:00"
portfolio=portfolio_create(dateStart,dateEnd)
portfolio_settings(portfolio,portfolioMetricsMode="price",windowLength = '3600s',
resultsSamplingInterval='60s')
positionAAPL=position_add(portfolio,'AAPL',100)
positionC=position_add(portfolio,'C',300) 
positionGOOG=position_add(portfolio,'GOOG',150) 
result=compute(omega_ratio(positionC,0.05),omega_ratio(positionGOOG,0.05),
omega_ratio(positionAAPL,0.05)) 
plot(omega_ratio(positionC,0.05),omega_ratio(positionGOOG,0.05),
omega_ratio(positionAAPL,0.05),legend=c('C','GOOG','AAPL'),title='Omega Ratio')
}}
\keyword{PortfolioEffectHFT}
%\concept{high frequency, intraday analytics, market data, portfolio, portfolio management,realtime analytics, risk, risk management, toolbox tools, trading, trading strategies}
\keyword{omega_ratio}