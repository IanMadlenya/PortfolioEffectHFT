\docType{methods}
\name{expected_return}
\alias{expected_return}
\alias{expected_return,portfolio-method}
\alias{expected_return,position-method}
\title{Expected Return}
\usage{
expected_return(asset)
}
\arguments{
  \item{asset}{Portfolio or Position object created using \link[=portfolio_create]{portfolio_create( )} or \link[=position_add]{position_add( )} function}
}
\value{Metric object}
\description{Computes portfolio cumulative expected return.
}
\note{\url{https://www.portfolioeffect.com/docs/glossary/measures/absolute-return-measures/expected-return}
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
result=compute(expected_return(portfolio),expected_return(positionGOOG),
expected_return(positionAAPL)) 
plot(expected_return(portfolio),expected_return(positionGOOG),expected_return(positionAAPL),
legend=c('Portfolio','GOOG','AAPL'),title='Expected Return')

dateStart = "2014-11-17 09:30:00"
dateEnd = "2014-11-17 16:00:00"
portfolio=portfolio_create(dateStart,dateEnd)
portfolio_settings(portfolio,portfolioMetricsMode="price",windowLength = '3600s',
resultsSamplingInterval='60s')
positionAAPL=position_add(portfolio,'AAPL',100)
positionC=position_add(portfolio,'C',300) 
positionGOOG=position_add(portfolio,'GOOG',150) 
result=compute(expected_return(positionC),expected_return(positionGOOG),
expected_return(positionAAPL)) 
plot(expected_return(positionC),expected_return(positionGOOG),expected_return(positionAAPL),
legend=c('C','GOOG','AAPL'),title='Expected Return')
}}
\keyword{PortfolioEffectHFT}
%\concept{high frequency, intraday analytics, market data, portfolio, portfolio management,realtime analytics, risk, risk management, toolbox tools, trading, trading strategies}
\keyword{expected_return}
