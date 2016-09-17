\docType{methods}
\name{txn_costs}
\alias{txn_costs}
\alias{txn_costs,portfolio-method}
\alias{txn_costs,position-method}
\title{Transactional Costs}
\usage{
txn_costs(asset)
}
\arguments{
  \item{asset}{Portfolio or Position object created using \link[=portfolio_create]{portfolio_create( )} or \link[=position_add]{position_add( )} function}
}
\value{Metric object}
\description{Computes monetary value of accumulated portfolio transactional costs.
}
\author{Kostin Andrey <andrey.kostin@portfolioeffect.com>}
\examples{
\dontrun{

data(aapl.data) 
data(goog.data) 
data(spy.data) 
portfolio=portfolio_create(priceDataIx=spy.data)
portfolio_settings(portfolio,windowLength = '3600s',resultsSamplingInterval='60s',txnCostFixed=100)
positionGOOG=position_add(portfolio,'GOOG',c(100,200),time=c(1412256601000,1412266600000),
priceData=goog.data) 
positionAAPL=position_add(portfolio,'AAPL',c(300,150),time=c(1412266600000,1412276600000),
priceData=aapl.data) 
result=compute(txn_costs(portfolio),txn_costs(positionGOOG),txn_costs(positionAAPL)) 
plot(txn_costs(portfolio),txn_costs(positionGOOG),txn_costs(positionAAPL),
legend=c('Portfolio','GOOG','AAPL'),title='Transactional Costs')
}}
\keyword{PortfolioEffectHFT}
%\concept{high frequency, intraday analytics, market data, portfolio, portfolio management,realtime analytics, risk, risk management, toolbox tools, trading, trading strategies}
\keyword{txn_costs}
