\docType{methods}
\name{forecast_input}
\alias{forecast_input}
\title{Forecast Input}
\usage{
forecast_input(forecast,metric)
}
\arguments{
  \item{forecast}{Object of class \link[=forecast-class]{forecast}}
  \item{metric}{Object of class \link[=metric-class]{metric}}
}
\value{Object of class \link[=forecast-class]{forecast}}
\description{Adds given metric as an explanatory variable to forecast model 
}
\author{Kostin Andrey <andrey.kostin@portfolioeffect.com>}
\examples{
\dontrun{
dateStart = "2014-11-17 09:30:00"
dateEnd = "2014-11-17 16:00:00"
portfolio=portfolio_create(dateStart,dateEnd)
portfolio_settings(portfolio,portfolioMetricsMode="price",windowLength = '360s',
                   resultsSamplingInterval='60s')
positionAAPL=position_add(portfolio,'AAPL',100)
positionC=position_add(portfolio,'C',300) 
positionGOOG=position_add(portfolio,'GOOG',150) 

forecastVariance_1=forecast_builder(variance(positionAAPL),window="1d")
# plot(forecast_apply(forecastVariance),variance(positionAAPL),legend=c('Forecast','Simple'))

forecastVariance_2=forecast_builder(variance(positionAAPL),window="1d")
forecastVariance_2=forecast_input(forecastVariance_2,beta(positionAAPL))
plot(forecast_apply(forecastVariance_1),forecast_apply(forecastVariance_2),
     variance(positionAAPL),legend=c('Forecast,without input','Forecast,with input','Simple'))
}}
\keyword{PortfolioEffectHFT}
\keyword{forecast_input}