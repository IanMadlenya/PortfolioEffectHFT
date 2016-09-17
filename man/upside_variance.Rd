\docType{methods}
\name{upside_variance}
\alias{upside_variance}
\alias{upside_variance,portfolio-method}
\alias{upside_variance,position-method}
\title{Upside Variance}
\usage{
upside_variance(asset, thresholdReturn)
}
\arguments{
  \item{asset}{Portfolio or Position object created using \link[=portfolio_create]{portfolio_create( )} or \link[=position_add]{position_add( )} function}
  \item{thresholdReturn}{Return value to be used as a cut-off point}
}
\value{Metric object}
\description{Computes upside variance of a portfolio.
}
\note{\url{https://www.portfolioeffect.com/docs/glossary/measures/absolute-risk-measures/upside-variance}
}
\author{Kostin Andrey <andrey.kostin@portfolioeffect.com>}
\seealso{\code{\link{downside_variance}} }
\examples{
\dontrun{
dateStart = "2014-11-17 09:30:00"
dateEnd = "2014-11-17 16:00:00"
portfolio=portfolio_create(dateStart,dateEnd)
positionAAPL=position_add(portfolio,'AAPL',100)
positionC=position_add(portfolio,'C',300) 
positionGOOG=position_add(portfolio,'GOOG',150) 
result=compute(upside_variance(positionC,0.05),upside_variance(positionGOOG,0.05),
upside_variance(positionAAPL,0.05)) 
plot(upside_variance(positionC,0.05),upside_variance(positionGOOG,0.05),
upside_variance(positionAAPL,0.05),legend=c('C','GOOG','AAPL'),title='Upside Variance')
}}
\keyword{PortfolioEffectHFT}
%\concept{high frequency, intraday analytics, market data, portfolio, portfolio management,realtime analytics, risk, risk management, toolbox tools, trading, trading strategies}
\keyword{upside_variance}