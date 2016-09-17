\docType{methods}
\name{position_remove}
\alias{position_remove}
\alias{position_remove,portfolio,character-method}
\alias{position_remove,position,missing-method}
\title{Remove position from portfolio}
\usage{
%position_remove(x, symbol)
\S4method{position_remove}{portfolio,character}(asset, symbol)
\S4method{position_remove}{position,missing}(asset)
}
\arguments{
  \item{asset}{Portfolio or Position object created using \link[=portfolio_create]{portfolio_create( )} or \link[=position_add]{position_add( )} function}
  \item{symbol}{Unique identifier of an instrument}
}
\value{Void}
\description{Removes position from an existing portfolio.
}
\author{Kostin Andrey <andrey.kostin@portfolioeffect.com>}
\seealso{\code{\link{position_add}} \code{\link{portfolio_create}}  }
\examples{
\dontrun{
data(aapl.data) 
data(goog.data) 
data(spy.data) 
portfolio=portfolio_create(priceDataIx=spy.data)
portfolio_settings(portfolio,windowLength = '3600s',resultsSamplingInterval='60s')
positionGOOG=position_add(portfolio,'GOOG',100,priceData=goog.data)   
positionAAPL=position_add(portfolio,'AAPL',300,priceData=aapl.data) 
position_remove(portfolio,'GOOG')
position_list(portfolio)

dateStart = "2014-11-17 09:30:00"
dateEnd = "2014-11-17 16:00:00"
portfolio=portfolio_create(dateStart,dateEnd)
portfolio_settings(portfolio,portfolioMetricsMode="price",windowLength = '3600s',
resultsSamplingInterval='60s')
positionAAPL=position_add(portfolio,'AAPL',100)
positionC=position_add(portfolio,'C',300) 
positionGOOG=position_add(portfolio,'GOOG',150) 
position_remove(portfolio,'C')
position_list(portfolio)
}}
\keyword{PortfolioEffectHFT}
%\concept{high frequency, intraday analytics, market data, portfolio, portfolio management,realtime analytics, risk, risk management, toolbox tools, trading, trading strategies}
\keyword{position_remove}