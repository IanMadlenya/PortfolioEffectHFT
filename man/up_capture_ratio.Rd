\docType{methods}
\name{up_capture_ratio}
\alias{up_capture_ratio}
\alias{up_capture_ratio,portfolio-method}
\alias{up_capture_ratio,position-method}
\title{Up Capture Ratio}
\usage{
up_capture_ratio(asset)
}
\arguments{
  \item{asset}{Portfolio or Position object created using \link[=portfolio_create]{portfolio_create( )} or \link[=position_add]{position_add( )} function}
}
\value{Metric object}
\description{Computes up capture ratio of a portfolio.
}
\note{\url{https://www.portfolioeffect.com/docs/glossary/measures/relative-return-measures/up-capture-ratio}
}
\author{Kostin Andrey <andrey.kostin@portfolioeffect.com>}
\seealso{\code{\link{down_capture_ratio}}}
\examples{
\dontrun{
data(aapl.data) 
data(goog.data) 
data(spy.data) 
portfolio=portfolio_create(priceDataIx=spy.data)
portfolio_settings(portfolio,windowLength = '3600s',resultsSamplingInterval='60s')
positionGOOG=position_add(portfolio,'GOOG',100,priceData=goog.data)   
positionAAPL=position_add(portfolio,'AAPL',300,priceData=aapl.data) 
result=compute(up_capture_ratio(portfolio),up_capture_ratio(positionGOOG),
up_capture_ratio(positionAAPL)) 
plot(up_capture_ratio(portfolio),up_capture_ratio(positionGOOG),
up_capture_ratio(positionAAPL),legend=c('Portfolio','GOOG','AAPL'),title='Up Capture Ratio')

dateStart = "2014-11-17 09:30:00"
dateEnd = "2014-11-17 16:00:00"
portfolio=portfolio_create(dateStart,dateEnd)
positionAAPL=position_add(portfolio,'AAPL',100)
positionC=position_add(portfolio,'C',300) 
positionGOOG=position_add(portfolio,'GOOG',150) 
result=compute(up_capture_ratio(positionC),up_capture_ratio(positionGOOG),
up_capture_ratio(positionAAPL)) 
plot(up_capture_ratio(positionC),up_capture_ratio(positionGOOG),
up_capture_ratio(positionAAPL),legend=c('C','GOOG','AAPL'),title='Up Capture Ratio')
}}
\keyword{PortfolioEffectHFT}
%\concept{high frequency, intraday analytics, market data, portfolio, portfolio management,realtime analytics, risk, risk management, toolbox tools, trading, trading strategies}
\keyword{up_capture_ratio}
