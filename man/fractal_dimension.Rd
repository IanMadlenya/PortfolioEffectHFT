\docType{methods}
\name{fractal_dimension}
\alias{fractal_dimension}
\alias{fractal_dimension,portfolio-method}
\alias{fractal_dimension,position-method}
\title{Fractal Dimension}
\usage{
fractal_dimension(asset)
}
\arguments{
  \item{asset}{Portfolio or Position object created using \link[=portfolio_create]{portfolio_create( )} or \link[=position_add]{position_add( )} function}
}
\value{Metric object}
\description{Computes portfolio fractal dimension as a weighted sum of fractal dimensions of its position returns.
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
result=compute(fractal_dimension(portfolio),fractal_dimension(positionGOOG),
fractal_dimension(positionAAPL)) 
plot(fractal_dimension(portfolio),fractal_dimension(positionGOOG),
fractal_dimension(positionAAPL),legend=c('Portfolio','GOOG','AAPL'),title='Fractal Dimension')

dateStart = "2014-11-17 09:30:00"
dateEnd = "2014-11-17 16:00:00"
portfolio=portfolio_create(dateStart,dateEnd)
portfolio_settings(portfolio,portfolioMetricsMode="price",windowLength = '3600s',
resultsSamplingInterval='60s')
positionAAPL=position_add(portfolio,'AAPL',100)
positionC=position_add(portfolio,'C',300) 
positionGOOG=position_add(portfolio,'GOOG',150) 
result=compute(fractal_dimension(positionC),fractal_dimension(positionGOOG),
fractal_dimension(positionAAPL)) 
plot(fractal_dimension(positionC),fractal_dimension(positionGOOG),
fractal_dimension(positionAAPL),legend=c('C','GOOG','AAPL'),title='Fractal Dimension')
}}
\keyword{PortfolioEffectHFT}
%\concept{high frequency, intraday analytics, market data, portfolio, portfolio management,realtime analytics, risk, risk management, toolbox tools, trading, trading strategies}
\keyword{fractal_dimension}
