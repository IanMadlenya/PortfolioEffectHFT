\docType{methods}
\name{calmar_ratio}
\alias{calmar_ratio}
\alias{calmar_ratio,portfolio-method}
\alias{calmar_ratio,position-method}
\title{Calmar Ratio}
\usage{
calmar_ratio(asset)
}
\arguments{
  \item{asset}{Portfolio or Position object created using \link[=portfolio_create]{portfolio_create( )} or \link[=position_add]{position_add( )} function}
}
\value{Metric object}
\description{Computes Calmar ratio (cumulative return to maximum drawdown). }
\note{\url{https://www.portfolioeffect.com/docs/glossary/measures/absolute-risk-adjusted-measures/calmar-ratio}
}
\author{Kostin Andrey <andrey.kostin@portfolioeffect.com>}
\seealso{\code{\link{sharpe_ratio}} \code{\link{sortino_ratio}} \code{\link{omega_ratio}} }
\examples{
\dontrun{
data(aapl.data) 
data(goog.data) 
data(spy.data) 
portfolio=portfolio_create(priceDataIx=spy.data)
portfolio_settings(portfolio,windowLength = '3600s',resultsSamplingInterval='60s')
positionGOOG=position_add(portfolio,'GOOG',100,priceData=goog.data)   
positionAAPL=position_add(portfolio,'AAPL',300,priceData=aapl.data) 
result=compute(calmar_ratio(portfolio),calmar_ratio(positionGOOG),calmar_ratio(positionAAPL)) 
plot(calmar_ratio(portfolio),calmar_ratio(positionGOOG),calmar_ratio(positionAAPL),
legend=c('Portfolio','GOOG','AAPL'),title='Calmar Ratio')

dateStart = "2014-11-17 09:30:00"
dateEnd = "2014-11-19 16:00:00"
portfolio=portfolio_create(dateStart,dateEnd)
positionAAPL=position_add(portfolio,'AAPL',100)
positionC=position_add(portfolio,'C',300) 
positionGOOG=position_add(portfolio,'GOOG',150) 
result=compute(calmar_ratio(positionC),calmar_ratio(positionGOOG),calmar_ratio(positionAAPL)) 
plot(calmar_ratio(positionC),calmar_ratio(positionGOOG),calmar_ratio(positionAAPL),
legend=c('C','GOOG','AAPL'),title='Calmar Ratio')
}}
\keyword{PortfolioEffectHFT}
%\concept{high frequency, intraday analytics, market data, portfolio, portfolio management,realtime analytics, risk, risk management, toolbox tools, trading, trading strategies}
\keyword{calmar_ratio}

