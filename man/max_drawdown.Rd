\docType{methods}
\name{max_drawdown}
\alias{max_drawdown}
\alias{max_drawdown,portfolio-method}
\alias{max_drawdown,position-method}
\title{Max Drawdown}
\usage{
max_drawdown(asset)
}
\arguments{
  \item{asset}{Portfolio or Position object created using \link[=portfolio_create]{portfolio_create( )} or \link[=position_add]{position_add( )} function}
}
\value{Metric object}
\description{Computes maximum drawdown of portfolio returns. }
\note{\url{https://www.portfolioeffect.com/docs/glossary/measures/absolute-risk-measures/max-drawdown}
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
result=compute(max_drawdown(portfolio),max_drawdown(positionGOOG),max_drawdown(positionAAPL)) 
plot(max_drawdown(portfolio),max_drawdown(positionGOOG),max_drawdown(positionAAPL),
legend=c('Portfolio','GOOG','AAPL'),title='Max Drawdown')

dateStart = "2014-11-17 09:30:00"
dateEnd = "2014-11-17 16:00:00"
portfolio=portfolio_create(dateStart,dateEnd)
positionAAPL=position_add(portfolio,'AAPL',100)
positionC=position_add(portfolio,'C',300) 
positionGOOG=position_add(portfolio,'GOOG',150) 
result=compute(max_drawdown(positionC),max_drawdown(positionGOOG),max_drawdown(positionAAPL)) 
plot(max_drawdown(positionC),max_drawdown(positionGOOG),max_drawdown(positionAAPL),
legend=c('C','GOOG','AAPL'),title='Max Drawdown')
}}
\keyword{PortfolioEffectHFT}
%\concept{high frequency, intraday analytics, market data, portfolio, portfolio management,realtime analytics, risk, risk management, toolbox tools, trading, trading strategies}
\keyword{max_drawdown}