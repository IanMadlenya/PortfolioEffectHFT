\name{weight_transform}
\alias{weight_transform}
\title{Weight Transform}

\usage{weight_transform(portfolio,transformType=c('sum_abs_weight','equiweight'),symbols=NULL)
}
\arguments{
  \item{portfolio}{Portfolio object created using \link[=portfolio_create]{portfolio_create( )} function}
  \item{transformType}{Transform applied to position weights: "sum_abs_weights" - sum of absolute position weights, "equiweight" - equal position weights}
  \item{symbols}{List of symbols}
}
\value{Metric object}
\description{
Returns transformed position weights for a given list of symbols. This metric is commonly used for creating position constraints during portfolio optimization. 
}
\author{Kostin Andrey <andrey.kostin@portfolioeffect.com>}
\seealso{\code{\link{portfolio_create}} }
\examples{
\dontrun{
portfolio<-portfolio_create("SPY", "2014-11-19 09:30:00", "2014-11-19 16:00:00")
portfolio_settings(portfolio,portfolioMetricsMode="price",resultsSamplingInterval='1m')
position_AAPL=position_add(portfolio,"AAPL",1000)
position_GOOG=position_add(portfolio,"GOOG",1000)
position_SPY=position_add(portfolio,"SPY",1000)

optimizer=optimization_goal(variance(portfolio),direction="min")
optimizer=optimization_constraint(optimizer,value(portfolio),'=',10^9)
optimizer=optimization_constraint(optimizer,weight_transform(portfolio,
"sum_abs_weight",c(position_AAPL,position_GOOG)),">=",0.5)
plot(optimization_run(optimizer))

optimizer=optimization_goal(weight_transform(portfolio,"equiweight"))
plot(optimization_run(optimizer))
}}
\keyword{PortfolioEffectHFT}
%\concept{plot market data, plot}
\keyword{weight_transform}