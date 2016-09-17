\docType{methods}
\name{forecast_apply}
\alias{forecast_apply}
\title{Forecast Apply}
\usage{
forecast_apply(forecast)
}
\arguments{
  \item{forecast}{Object of type forecast created using \link[=forecast_builder]{forecast_builder( )} method}                 
}
\value{Forecast object}
\description{Runs forecasting algorithm on a configured forecast object. 
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

forecastVariance_1=forecast_builder(variance(positionAAPL))
# plot(forecast_apply(forecastVariance),variance(positionAAPL),legend=c('Forecast','Simple'))

forecastVariance_2=forecast_builder(variance(positionAAPL),window="1d")
plot(forecast_apply(forecastVariance_1),forecast_apply(forecastVariance_2),
     variance(positionAAPL),legend=c('Forecast,window=20d','Forecast,window=1d','Simple'))
}}
\keyword{PortfolioEffectHFT}
\keyword{forecast_apply}