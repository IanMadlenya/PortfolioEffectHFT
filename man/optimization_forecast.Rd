\name{optimization_forecast}
\alias{optimization_forecast}
\title{Porfolio Optimization - Set Optimization Forecast}
\usage{optimization_forecast(optimizer,
 				   metricType,
				   forecast)
}
\arguments{
  \item{optimizer}{Optimizer object created using \link[=optimization_goal]{optimization_goal( )} function}
  \item{metricType}{Choose forecast metric type:\cr
  "Beta" - position beta,\cr
  "Variance" - position variance,\cr
  "ExpReturn" - position expected return,\cr
  "Cumulant3" - position 3-th cumulant,\cr
  "Cumulant4" - position 4-th cumulant
  }   
  \item{forecast}{Object of class \link[=metric-class]{metric-class( )} or \link[=forecast-class]{forecast-class( )}} 
}
\value{Optimizer object}
\description{Sets user-defined forecasted values for a given metric and returns modified optimizer object. 
By default value of the metric at time "t" is used as a forecast for "t+1". }
\author{Kostin Andrey <andrey.kostin@portfolioeffect.com>}
\examples{
\dontrun{
dateStart = "2014-11-17 09:30:00"
dateEnd = "2014-12-17 16:00:00"
portfolio=portfolio_create(dateStart,dateEnd)

# Add position AAPL and GOOG to portfolio
positionAAPL=position_add(portfolio,"AAPL",100)
positionGOOG=position_add(portfolio,"GOOG",200)
portfolio_settings(portfolio,inputSamplingInterval='30m',resultsSamplingInterval='1d')

forecastVarianceAAPL=forecast_builder(variance(positionAAPL),model="HAR",step ='1d')
forecastVarianceGOOG=forecast_builder(variance(positionGOOG),model="HAR",step ='1d')

optimizer=optimization_goal(variance(portfolio),"min")
optimizer=optimization_constraint(optimizer,log_return(portfolio),">=",0)
optimizer=optimization_forecast(optimizer, "Variance", forecastVarianceAAPL)
optimizer=optimization_forecast(optimizer, "Variance", forecastVarianceGOOG)
optimalPortfolioWithHAR=optimization_run(optimizer)

optimizer=optimization_goal(variance(portfolio),"min")
optimizer=optimization_constraint(optimizer,log_return(portfolio),">=",0)
optimalPortfolioWithoutHAR=optimization_run(optimizer)

plot(variance(optimalPortfolioWithHAR),variance(optimalPortfolioWithoutHAR),title="Variance",
legend=c("With HAR Forecast","Without HAR Forecast"))
}}
\keyword{PortfolioEffectHFT}
%\concept{high frequency, market data, optimization, realtime analytics, risk, risk management, trading, trading strategies}
\keyword{optimization_forecast}