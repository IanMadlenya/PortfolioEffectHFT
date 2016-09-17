\docType{methods}
\name{forecast_builder}
\alias{forecast_builder}

\title{Forecast builder}
\usage{
forecast_builder(asset,model=c("EWMA", "HAR"), window="20d",
 step = "1d", transform = c("log", "none"), 
seasonalityInterval="none",updateInterval="1m",valueType="forecast")
}
\arguments{
  \item{asset}{Object of class \link[=portfolio-class]{portfolio} or \link[=position-class]{position} created using \link[=portfolio_create]{portfolio_create( )} or \link[=position_add]{position_add( )} methods respectively}  
  \item{model}{Forecast model to be used:    \itemize{
\item "EWMA" - exponentially-weighted moving average,
\item "HAR" - heterogeneous autoregresion}}
  \item{window}{Rolling window length for forecast model. Observations outside of the forecast window are forgotten. Available interval values are: "Xs" - seconds, "Xm" - minutes, "Xh" - hours, "Xd" - trading days (6.5 hours in a trading day), "Xw" - weeks (5 trading days in 1 week), "Xmo" - month (21 trading day in 1 month), "Xy" - years (256 trading days in 1 year).}
  \item{step}{Look-ahead forecast interval. Available interval values are: "Xs" - seconds, "Xm" - minutes, "Xh" - hours, "Xd" - trading days (6.5 hours in a trading day), "Xw" - weeks (5 trading days in 1 week), "Xmo" - month (21 trading day in 1 month), "Xy" - years (256 trading days in 1 year)}
  \item{transform}{Transform applied to dependent and independent variables: "log" - logarithmic transform, "none" - no transform}
  \item{seasonalityInterval}{Seasonality interval to be used in forecast model. Available interval values are: "Xs" - seconds, "Xm" - minutes, "Xh" - hours, "Xd" - trading days (6.5 hours in a trading day), "Xw" - weeks (5 trading days in 1 week), "Xmo" - month (21 trading day in 1 month), "Xy" - years (256 trading days in 1 year)}
  \item{updateInterval}{Update interval for forecast estimates. Available interval values are: "Xs" - seconds, "Xm" - minutes, "Xh" - hours, "Xd" - trading days (6.5 hours in a trading day), "Xw" - weeks (5 trading days in 1 week), "Xmo" - month (21 trading day in 1 month), "Xy" - years (256 trading days in 1 year)} 
  \item{valueType}{Value returned from the forecast model:\itemize{
\item "forecast" - value of forecasted variable, 
\item "error" - residual error, 
\item "coef_n" - value of n-th coefficient (e.g. "coef_2")}}
}
\value{Object of class \link[=forecast-class]{forecast}}
\description{ Create object of class \link[=forecast-class]{forecast}
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
\keyword{forecast_builder}