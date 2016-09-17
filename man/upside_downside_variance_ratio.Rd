\docType{methods}
\name{upside_downside_variance_ratio}
\alias{upside_downside_variance_ratio}
\alias{upside_downside_variance_ratio,portfolio-method}
\alias{upside_downside_variance_ratio,position-method}
\title{Upside/Downside Variance Ratio}
\usage{
upside_downside_variance_ratio(asset, thresholdReturn)
}
\arguments{
  \item{asset}{Portfolio or Position object created using \link[=portfolio_create]{portfolio_create( )} or \link[=position_add]{position_add( )} function}
  \item{thresholdReturn}{Return value to be used as a cut-off point}
}
\value{Metric object}
\description{Computes upside to downside variance ratio of a portfolio.
}
\note{\url{https://www.portfolioeffect.com/docs/glossary/measures/absolute-risk-measures/upside-downside-variance-ratio}
}
\author{Kostin Andrey <andrey.kostin@portfolioeffect.com>}
\seealso{\code{\link{upside_variance}} \code{\link{downside_variance}}}
\examples{
\dontrun{
dateStart = "2014-11-17 09:30:00"
dateEnd = "2014-11-17 16:00:00"
portfolio=portfolio_create(dateStart,dateEnd)
positionAAPL=position_add(portfolio,'AAPL',100)
positionC=position_add(portfolio,'C',300) 
positionGOOG=position_add(portfolio,'GOOG',150) 
result=compute(upside_downside_variance_ratio(positionC,0.05),
upside_downside_variance_ratio(positionGOOG,0.05),
upside_downside_variance_ratio(positionAAPL,0.05)) 
plot(upside_downside_variance_ratio(positionC,0.05),
upside_downside_variance_ratio(positionGOOG,0.05),
upside_downside_variance_ratio(positionAAPL,0.05),
legend=c('C','GOOG','AAPL'),title='Upside/Downside Variance Ratio')
}}
\keyword{PortfolioEffectHFT}
%\concept{high frequency, intraday analytics, market data, portfolio, portfolio management,realtime analytics, risk, risk management, toolbox tools, trading, trading strategies}
\keyword{upside_downside_variance_ratio}
