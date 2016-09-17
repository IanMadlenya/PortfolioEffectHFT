\docType{methods}
\name{set_quantity}
\alias{set_quantity}
\alias{set_quantity,position,numeric-method}
\alias{set_quantity,position,integer-method}
\title{Set Position Quantity}
\usage{
set_quantity(asset,quantity)
}
\arguments{
  \item{asset}{Position object created using \link[=position_add]{position_add( )} function}
  \item{quantity}{One dimensional vector of position quantities or an integer number if quantity is constant}   
}
\value{Void
}
\description{Sets new position quantity.
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
print(portfolio)

set_quantity(positionGOOG,400)
print(portfolio)
}}
\keyword{PortfolioEffectHFT}
%\concept{high frequency, intraday analytics, market data, portfolio, portfolio management,realtime analytics, risk, risk management, toolbox tools, trading, trading strategies}
\keyword{set_quantity}