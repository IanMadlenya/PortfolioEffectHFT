\docType{methods}
\name{loss_variance}
\alias{loss_variance}
\alias{loss_variance,portfolio-method}
\alias{loss_variance,position-method}
\title{Loss Variance}
\usage{
loss_variance(asset)
}
\arguments{
  \item{asset}{Portfolio or Position object created using \link[=portfolio_create]{portfolio_create( )} or \link[=position_add]{position_add( )} function}
}
\value{Metric object}
\description{Computes loss variance of portfolio returns.
}
\note{\url{https://www.portfolioeffect.com/docs/glossary/measures/absolute-risk-measures/loss-variance}
}
\author{Kostin Andrey <andrey.kostin@portfolioeffect.com>}
\seealso{ \code{\link{gain_variance}}}
\examples{
\dontrun{
data(aapl.data) 
data(goog.data) 
data(spy.data) 
portfolio=portfolio_create(priceDataIx=spy.data)
portfolio_settings(portfolio,windowLength = '3600s',resultsSamplingInterval='60s')
positionGOOG=position_add(portfolio,'GOOG',100,priceData=goog.data)   
positionAAPL=position_add(portfolio,'AAPL',300,priceData=aapl.data) 
result=compute(loss_variance(portfolio),loss_variance(positionGOOG),loss_variance(positionAAPL)) 
plot(loss_variance(portfolio),loss_variance(positionGOOG),loss_variance(positionAAPL),
legend=c('Portfolio','GOOG','AAPL'),title='Loss Variance')

dateStart = "2014-11-17 09:30:00"
dateEnd = "2014-11-17 16:00:00"
portfolio=portfolio_create(dateStart,dateEnd)
portfolio_settings(portfolio,portfolioMetricsMode="price",windowLength = '3600s',
resultsSamplingInterval='60s')
positionAAPL=position_add(portfolio,'AAPL',100)
positionC=position_add(portfolio,'C',300) 
positionGOOG=position_add(portfolio,'GOOG',150) 
result=compute(loss_variance(positionC),loss_variance(positionGOOG),loss_variance(positionAAPL)) 
plot(loss_variance(positionC),loss_variance(positionGOOG),loss_variance(positionAAPL),
legend=c('C','GOOG','AAPL'),title='Loss Variance')
}}
\keyword{PortfolioEffectHFT}
%\concept{high frequency, intraday analytics, market data, portfolio, portfolio management,realtime analytics, risk, risk management, toolbox tools, trading, trading strategies}
\keyword{loss_variance}