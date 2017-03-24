\name{portfolio_availableSymbols}
\alias{portfolio_availableSymbols}
\title{Get All Symbol List}
\usage{portfolio_availableSymbols()
}
\description{
Returns a list of symbols .
}
\value{List of symbols, exchanges and descriptions
}
\author{Kostin Andrey <andrey.kostin@portfolioeffect.com>}
\examples{
\dontrun{
portfolio=portfolio_create(fromTime="2014-10-02 09:30:00", toTime="2014-10-02 16:00:00")
positionSPY=position_add(portfolio,'SPY',500)
positionC=position_add(portfolio,'C',600)
list=portfolio_availableSymbols()
list[1:10,]
}}
\keyword{PortfolioEffectHFT}
%\concept{high frequency, intraday analytics, market data, portfolio, portfolio management,realtime analytics, risk, risk management, toolbox tools, trading, trading strategies}
\keyword{portfolio_availableSymbols}