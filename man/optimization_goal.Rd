\name{optimization_goal}
\alias{optimization_goal}
\title{Porfolio Optimization - Set Optimization Goal}
\usage{optimization_goal(goal,
		direction=c("min","max"),
		approxError=1e-12,
		optimumProbability=0.99)
}
\arguments{
  \item{goal}{Object of class \link[=metric-class]{metric} to be used as an optimization goal}
  \item{direction}{choose direction of optimization algorithm:\cr
  "min" - maximization goal,\cr
  "max" - minimization goal  }
  \item{approxError}{Estimation error in decimal points for computing optimal weights. Smaller value slows down optimization algorithm, but increases precision.}
  \item{optimumProbability}{Required probability level of a global optimum. Higher value slows down optimization algorithm, but increases chance of finding globally optimal solution.}
  }
\description{Initializes portfolio optimization goals and returns newly constructed optimizer object.}
\value{
Optimizer object.
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
optimizer=optimization_goal(log_return(portfolio),"max")
optimizer=optimization_constraint(optimizer,beta(portfolio),"<=",0.5)
optimalPortfolio=optimization_run(optimizer)
print(optimalPortfolio)
}}
\keyword{PortfolioEffectHFT}
%\concept{high frequency, market data, optimization, realtime analytics, risk, risk management, trading, trading strategies}
\keyword{optimization_goal}