\docType{methods}
\name{information_ratio}
\alias{information_ratio}
\alias{information_ratio,portfolio-method}
\alias{information_ratio,position-method}
\title{Information Ratio}
\usage{
information_ratio(asset)
}
\arguments{
  \item{asset}{Portfolio or Position object created using \link[=portfolio_create]{portfolio_create( )} or \link[=position_add]{position_add( )} function}
}
\value{Metric object}
\description{Computes information ratio of a portfolio.
}
\note{\url{https://www.portfolioeffect.com/docs/glossary/measures/relative-risk-adjusted-measures/information-ratio}
}
\author{Kostin Andrey <andrey.kostin@portfolioeffect.com>}
\examples{
\dontrun{
dateStart = "2014-11-17 09:30:00"
dateEnd = "2014-11-17 16:00:00"
portfolio=portfolio_create(dateStart,dateEnd)
portfolio_settings(portfolio,portfolioMetricsMode="price",windowLength = '3600s',
resultsSamplingInterval='60s')
positionAAPL=position_add(portfolio,'AAPL',100)
positionC=position_add(portfolio,'C',300) 
positionGOOG=position_add(portfolio,'GOOG',150) 
result=compute(information_ratio(positionC),information_ratio(positionGOOG),
information_ratio(positionAAPL)) 
plot(information_ratio(positionC),information_ratio(positionGOOG),
information_ratio(positionAAPL),legend=c('C','GOOG','AAPL'),title='Information Ratio')
}}
\keyword{PortfolioEffectHFT}
%\concept{high frequency, intraday analytics, market data, portfolio, portfolio management,realtime analytics, risk, risk management, toolbox tools, trading, trading strategies}
\keyword{information_ratio}
