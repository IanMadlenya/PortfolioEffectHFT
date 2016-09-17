\name{portfolio_getPosition}
\alias{portfolio_getPosition}
\title{Get position from portfolio}
\usage{portfolio_getPosition(portfolio, symbol)
}
\arguments{
  \item{portfolio}{Object of class \link[=portfolio-class]{portfolio} created using \link[=portfolio_create]{portfolio_create( )} function}
  \item{symbol}{Unique identifier of an instrument}
}
\value{Object of class \link[=position-class]{position} for a given symbol, if found in the portfolio
}
\description{Returns position for a given symbol if this position is found inside a given portfolio}
\author{Kostin Andrey <andrey.kostin@portfolioeffect.com>}
\seealso{\code{\link{portfolio_create}} }
\examples{
\dontrun{
dateStart = "2014-11-17 09:30:00"
dateEnd = "2014-11-17 16:00:00"
portfolio=portfolio_create(dateStart,dateEnd)
portfolio_settings(portfolio,portfolioMetricsMode="price",windowLength = '3600s',
resultsSamplingInterval='60s')
positionAAPL=position_add(portfolio,'AAPL',100)
positionC=position_add(portfolio,'C',300) 
position_add(portfolio,'GOOG',150)
positionGOOG=portfolio_getPosition(portfolio,'GOOG')
}}
\keyword{PortfolioEffectHFT}
%\concept{high frequency, intraday analytics, market data, portfolio, portfolio management,realtime analytics, risk, risk management, toolbox tools, trading, trading strategies}
\keyword{portfolio_getPosition}