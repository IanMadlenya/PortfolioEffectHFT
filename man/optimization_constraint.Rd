\name{optimization_constraint}
\alias{optimization_constraint}
\title{Portfolio Optimization Constraint}
\usage{optimization_constraint(optimizer,
        constraintMertic,                              
		constraintType,
		constraintValue)
}
\arguments{
  \item{optimizer}{Object of class \link[=optimizer-class]{optimizer} created using \link[=optimization_goal]{optimization_goal( )} method}
  \item{constraintMertic}{Object of class \link[=metric-class]{metric} to be used for computing optimization constraint}
  \item{constraintType}{Optimization constraint type:\cr
  "=" - an equality constraint,\cr
  ">=" - an inclusive lower bound constraint,\cr
  "<=" - an inclusive upper bound constraint
  }
  \item{constraintValue}{Value to be used as a constraint boundary}  
}
\value{Object of class \link[=optimizer-class]{optimizer}}
\description{Adds portfolio optimization constraint restricting optimal portfolio's beta to a certain range.}
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
\keyword{optimization_constraint}