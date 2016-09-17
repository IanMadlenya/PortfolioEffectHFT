\docType{methods}
\name{up_number_ratio}
\alias{up_number_ratio}
\alias{up_number_ratio,portfolio-method}
\alias{up_number_ratio,position-method}
\title{Up Number Ratio}
\usage{
up_number_ratio(asset)
}
\arguments{
  \item{asset}{Portfolio or Position object created using \link[=portfolio_create]{portfolio_create( )} or \link[=position_add]{position_add( )} function}
}
\value{Metric object}
\description{Computes up number ratio of a portfolio.
}
\note{\url{https://www.portfolioeffect.com/docs/glossary/measures/relative-return-measures/up-number-ratio}
}
\author{Kostin Andrey <andrey.kostin@portfolioeffect.com>}
\seealso{\code{\link{down_number_ratio}} }
\examples{
\dontrun{
data(aapl.data) 
data(goog.data) 
data(spy.data) 
portfolio=portfolio_create(priceDataIx=spy.data)
portfolio_settings(portfolio,windowLength = '3600s',resultsSamplingInterval='60s')
positionGOOG=position_add(portfolio,'GOOG',100,priceData=goog.data)   
positionAAPL=position_add(portfolio,'AAPL',300,priceData=aapl.data) 
result=compute(up_number_ratio(portfolio),up_number_ratio(positionGOOG),
up_number_ratio(positionAAPL)) 
plot(up_number_ratio(portfolio),up_number_ratio(positionGOOG),
up_number_ratio(positionAAPL),legend=c('Portfolio','GOOG','AAPL'),title='Up Number Ratio')

dateStart = "2014-11-17 09:30:00"
dateEnd = "2014-11-17 16:00:00"
portfolio=portfolio_create(dateStart,dateEnd)
portfolio_settings(portfolio,portfolioMetricsMode="price",windowLength = '3600s',
resultsSamplingInterval='60s')
positionAAPL=position_add(portfolio,'AAPL',100)
positionC=position_add(portfolio,'C',300) 
positionGOOG=position_add(portfolio,'GOOG',150) 
result=compute(up_number_ratio(positionC),up_number_ratio(positionGOOG),
up_number_ratio(positionAAPL)) 
plot(up_number_ratio(positionC),up_number_ratio(positionGOOG),
up_number_ratio(positionAAPL),legend=c('C','GOOG','AAPL'),title='Up Number Ratio')
}}
\keyword{PortfolioEffectHFT}
%\concept{high frequency, intraday analytics, market data, portfolio, portfolio management,realtime analytics, risk, risk management, toolbox tools, trading, trading strategies}
\keyword{up_number_ratio}
