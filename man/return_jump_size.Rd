\docType{methods}
\name{return_jump_size}
\alias{return_jump_size}
\alias{return_jump_size,position-method}
\title{Return Jump Size}
\usage{
return_jump_size(asset)
}
\arguments{
  \item{asset}{Position object created using \link[=position_add]{position_add( )} function}
}

\value{Metric object}
\description{Computes relative magnitude of jumps in position returns.
}
\author{Kostin Andrey <andrey.kostin@portfolioeffect.com>}
\examples{
\dontrun{
dateStart = "2014-11-17 09:30:00"
dateEnd = "2014-11-30 16:00:00"
portfolio=portfolio_create(dateStart,dateEnd)
portfolio_settings(portfolio,portfolioMetricsMode="price",windowLength = '3600s',
resultsSamplingInterval='60s')
positionAAPL=position_add(portfolio,'AAPL',100)
positionC=position_add(portfolio,'C',300) 
positionGOOG=position_add(portfolio,'GOOG',150) 
result=list(return_jump_size(positionC),return_jump_size(positionGOOG),
return_jump_size(positionAAPL))
}}
\keyword{PortfolioEffectHFT}
%\concept{high frequency, intraday analytics, market data, portfolio, portfolio management,realtime analytics, risk, risk management, toolbox tools, trading, trading strategies}
\keyword{return_jump_size}