\name{portfolio_getSettings}
\alias{portfolio_getSettings}
\title{Get Portfolio Settings}
\usage{portfolio_getSettings(portfolio)
}
\arguments{
  \item{portfolio}{Portfolio object created using \link[=portfolio_create]{portfolio_create( )} function}
}
\value{List with portfolio settings.}
\description{Method returns active list of settings of a given portfolio.}
\author{Kostin Andrey <andrey.kostin@portfolioeffect.com>}
\examples{
\dontrun{
dateStart = "2014-11-17 09:30:00"
dateEnd = "2014-11-17 16:00:00"
portfolio=portfolio_create(dateStart,dateEnd)
positionAAPL=position_add(portfolio,'AAPL',100)
positionC=position_add(portfolio,'C',300) 
positionGOOG=position_add(portfolio,'GOOG',150) 
portfolio_settings(portfolio,
				   windowLength='600s',
				   resultsSamplingInterval = '10s')
settings=portfolio_getSettings(portfolio)
settings
}}
\keyword{PortfolioEffectHFT}
%\concept{high frequency, intraday analytics, market data, portfolio, portfolio management,realtime analytics, risk, risk management, toolbox tools, trading, trading strategies}
\keyword{portfolio_getSettings}