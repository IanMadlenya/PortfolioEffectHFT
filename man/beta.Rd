\docType{methods}
\name{beta}
\alias{beta}
\alias{beta,portfolio-method}
\alias{beta,position-method}
\title{Beta}
\usage{
%beta(a, b)
\S4method{beta}{portfolio}(a)
\S4method{beta}{position}(a)
}
\arguments{
  \item{a}{Portfolio or Position object created using \link[=portfolio_create]{portfolio_create( )} or \link[=position_add]{position_add( )} function}
}
\value{Metric object}
\description{Computes portfolio or position beta (market sensitivity) according to the Single Index Model.}
\note{\url{https://www.portfolioeffect.com/docs/glossary/measures/relative-risk-measures/beta.php}
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
result=compute(beta(portfolio),beta(positionGOOG),beta(positionAAPL)) 
plot(beta(portfolio),beta(positionGOOG),beta(positionAAPL),
legend=c('Portfolio','GOOG','AAPL'),title='Beta')

dateStart = "2014-11-17 09:30:00"
dateEnd = "2014-11-17 16:00:00"
portfolio=portfolio_create(dateStart,dateEnd)
portfolio_settings(portfolio,portfolioMetricsMode="price",windowLength = '3600s',
resultsSamplingInterval='60s')
positionAAPL=position_add(portfolio,'AAPL',100)
positionC=position_add(portfolio,'C',300) 
positionGOOG=position_add(portfolio,'GOOG',150) 
result=compute(beta(positionC),beta(positionGOOG),beta(positionAAPL)) 
plot(beta(positionC),beta(positionGOOG),beta(positionAAPL),
legend=c('C','GOOG','AAPL'),title='Beta')
}}
\keyword{PortfolioEffectHFT}
%\concept{high frequency, intraday analytics, market data, portfolio, portfolio management,realtime analytics, risk, risk management, toolbox tools, trading, trading strategies}
\keyword{beta}