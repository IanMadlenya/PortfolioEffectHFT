\docType{methods}
\name{log_return}
\alias{log_return}
\alias{log_return,portfolio-method}
\alias{log_return,position-method}
\title{Returns}
\usage{
log_return(asset)
}
\arguments{
  \item{asset}{Portfolio or Position object created using \link[=portfolio_create]{portfolio_create( )} or \link[=position_add]{position_add( )} function}
}
\value{Metric object}
\description{Computes portfolio log_return from the beginning of the holding period.
}
\note{\url{https://www.portfolioeffect.com/docs/glossary/measures/absolute-return-measures/return}
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
result=compute(log_return(portfolio),log_return(positionGOOG),log_return(positionAAPL)) 
plot(log_return(portfolio),log_return(positionGOOG),log_return(positionAAPL),
legend=c('Portfolio','GOOG','AAPL'),title='Returns')

dateStart = "2014-11-17 09:30:00"
dateEnd = "2014-11-17 16:00:00"
portfolio=portfolio_create(dateStart,dateEnd)
portfolio_settings(portfolio,portfolioMetricsMode="price",windowLength = '3600s',
resultsSamplingInterval='60s')
positionAAPL=position_add(portfolio,'AAPL',100)
positionC=position_add(portfolio,'C',300) 
positionGOOG=position_add(portfolio,'GOOG',150) 
result=compute(log_return(positionC),log_return(positionGOOG),log_return(positionAAPL)) 
plot(log_return(positionC),log_return(positionGOOG),log_return(positionAAPL),
legend=c('C','GOOG','AAPL'),title='Returns')
}}
\keyword{PortfolioEffectHFT}
%\concept{high frequency, intraday analytics, market data, portfolio, portfolio management,realtime analytics, risk, risk management, toolbox tools, trading, trading strategies}
\keyword{log_return}
