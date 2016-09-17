\docType{methods}
\name{expected_downside_return}
\alias{expected_downside_return}
\alias{expected_downside_return,portfolio-method}
\alias{expected_downside_return,position-method}
\title{Expected Downside Return}
\usage{
expected_downside_return(asset, thresholdReturn)
}
\arguments{
  \item{asset}{Portfolio or Position object created using \link[=portfolio_create]{portfolio_create( )} or \link[=position_add]{position_add( )} function}
  \item{thresholdReturn}{Return value to be used as a cut-off point}
}
\value{Metric object}
\description{Computes portfolio cumulative expected return below a certain threshold.
}
\note{\url{https://www.portfolioeffect.com/docs/glossary/measures/absolute-return-measures/expected-downside-return}
}
\author{Kostin Andrey <andrey.kostin@portfolioeffect.com>}
\seealso{\code{\link{expected_upside_return}} }
\examples{
\dontrun{
data(aapl.data) 
data(goog.data) 
data(spy.data) 
portfolio=portfolio_create(priceDataIx=spy.data)
portfolio_settings(portfolio,windowLength = '3600s',resultsSamplingInterval='60s')
positionGOOG=position_add(portfolio,'GOOG',100,priceData=goog.data)   
positionAAPL=position_add(portfolio,'AAPL',300,priceData=aapl.data) 
result=compute(expected_downside_return(portfolio,0.05),
expected_downside_return(positionGOOG,0.05),expected_downside_return(positionAAPL,0.05)) 
plot(expected_downside_return(portfolio,0.05),expected_downside_return(positionGOOG,0.05),
expected_downside_return(positionAAPL,0.05),legend=c('Portfolio','GOOG','AAPL'),
title='Expected Downside Return')

dateStart = "2014-11-17 09:30:00"
dateEnd = "2014-11-17 16:00:00"
portfolio=portfolio_create(dateStart,dateEnd)
portfolio_settings(portfolio,portfolioMetricsMode="price",windowLength = '3600s',
resultsSamplingInterval='60s')
positionAAPL=position_add(portfolio,'AAPL',100)
positionC=position_add(portfolio,'C',300) 
positionGOOG=position_add(portfolio,'GOOG',150) 
result=compute(expected_downside_return(positionC,0.05),
expected_downside_return(positionGOOG,0.05),expected_downside_return(positionAAPL,0.05)) 
plot(expected_downside_return(positionC,0.05),expected_downside_return(positionGOOG,0.05),
expected_downside_return(positionAAPL,0.05),legend=c('C','GOOG','AAPL'),
title='Expected Downside Return')
}}
\keyword{PortfolioEffectHFT}
%\concept{high frequency, intraday analytics, market data, portfolio, portfolio management,realtime analytics, risk, risk management, toolbox tools, trading, trading strategies}
\keyword{expected_downside_return}
