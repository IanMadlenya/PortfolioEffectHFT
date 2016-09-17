\docType{methods}
\name{alpha_jensens}
\alias{alpha_jensens}
\alias{alpha_jensens,portfolio-method}
\alias{alpha_jensens,position-method}
\title{Jensen's Alpha}
\usage{
alpha_jensens(asset)
}
\arguments{
  \item{asset}{Portfolio or Position object created using \link[=portfolio_create]{portfolio_create( )} or \link[=position_add]{position_add( )} function}
}
\value{Metric object}
\description{Computes portfolio Jensen's alpha (excess return) according to the Single Index Model.
}
\note{\url{https://www.portfolioeffect.com/docs/glossary/measures/relative-risk-adjusted-measures/jensens-alpha}
}
\author{Kostin Andrey <andrey.kostin@portfolioeffect.com>}
\seealso{\code{\link{beta}} }
\examples{
\dontrun{
data(aapl.data) 
data(goog.data) 
data(spy.data) 
portfolio=portfolio_create(spy.data)
portfolio_settings(portfolio,windowLength = '3600s',resultsSamplingInterval='60s')
positionGOOG=position_add(portfolio,'GOOG',100,priceData=goog.data)   
positionAAPL=position_add(portfolio,'AAPL',300,priceData=aapl.data) 
result=compute(alpha_jensens(portfolio),alpha_jensens(positionGOOG),alpha_jensens(positionAAPL)) 
plot(alpha_jensens(portfolio),alpha_jensens(positionGOOG),alpha_jensens(positionAAPL),
legend=c('Portfolio','GOOG','AAPL'),title="Jensen's Alpha")

dateStart = "2014-11-17 09:30:00"
dateEnd = "2014-11-17 16:00:00"
portfolio=portfolio_create(dateStart,dateEnd)
portfolio_settings(portfolio,portfolioMetricsMode="price",windowLength = '3600s',
resultsSamplingInterval='60s')
positionAAPL=position_add(portfolio,'AAPL',100)
positionC=position_add(portfolio,'C',300) 
positionGOOG=position_add(portfolio,'GOOG',150) 
result=compute(alpha_jensens(positionC),alpha_jensens(positionGOOG),alpha_jensens(positionAAPL)) 
plot(alpha_jensens(positionC),alpha_jensens(positionGOOG),alpha_jensens(positionAAPL),
legend=c('C','GOOG','AAPL'),title="Jensen's Alpha")
}}
\keyword{PortfolioEffectHFT}
%\concept{high frequency, intraday analytics, market data, portfolio, portfolio management,realtime analytics, risk, risk management, toolbox tools, trading, trading strategies}
\keyword{alpha_jensens}