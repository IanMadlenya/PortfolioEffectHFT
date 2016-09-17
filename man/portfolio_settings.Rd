\name{portfolio_settings}
\alias{portfolio_settings}
\title{Portfolio Settings}
\usage{portfolio_settings(portfolio,...)
}
\arguments{
  \item{portfolio}{Portfolio object created using \link[=portfolio_create]{portfolio_create( )} function}
  \item{...}{One of the following portfolio settings:
    \itemize{
\item "portfolioMetricsMode" - Used to select method of computing portfolio metrics. Available modes are: "portfolio" - risk and performance metrics are computed based on the history of position rebalancing (see windowLength parameter) and should be used to backtest and compare trading strategies of different frequency and style, "price" - metrics are always computed without a history of previous rebalancing (classic interpretation). Defaults to "portfolio". 
\item "windowLength" - Rolling window length for metric estimations and position rebalancing history. Available interval values are: "Xs" - seconds, "Xm" - minutes, "Xh" - hours, "Xd" - trading days (6.5 hours in a trading day), "Xw" - weeks (5 trading days in 1 week), "Xmo" - month (21 trading day in 1 month), "Xy" - years (256 trading days in 1 year), "all" - all observations are used. Default value is "1d" - one trading day . 
\item "holdingPeriodsOnly - Used when portfolioMetricsMode = "portfolio". Defaults to FALSE, which means that trading strategy risk and performance metrics will be scaled to include intervals when trading strategy did not have market exposure. When TRUE, trading strategy metrics are scaled based on actual holding intervals when there was exposure to the market. 
\item "shortSalesMode" - Specifies how position weights are computed. Available modes are: "lintner" - the sum of absolute weights is equal to 1 (Lintner assumption), "markowitz" - the sum of weights must equal to 1 (Markowitz assumption). Defaults to "lintner", which implies that the sum of absolute weights is used to normalize investment weights. 
\item "synchronizationModel" - Synchronization mode for irregular spaced time series to be used when computing covariances. Defaults to TRUE, which implies that Hayashi-Yoshida algorithm would be used for synchronizing price observations. 
\item "jumpsModel" - Jump filtering mode when computing return statistics. Available modes are: "none" - price jumps are not filtered anywhere, "moments" - price jumps are filtered only when computing moments (variance, skewness, kurtosis) and derived metrics, "all" - price jumps are filtered everywhere. Defaults to "moments", which implies that only return moments and related metrics would be using jump-filtered returns in their calculations. 
\item "noiseModel" - Microstructure noise mode for intraday returns.Defaults to TRUE, which implies that microstructure effects are modeled and resulting HF noise is removed from metric calculations. 
\item "fractalPriceModel" - Fractal price model (fGBM) when time scaling return moments based on mono-fractality assumptions.Defaults to TRUE, which implies that computed Hurst exponent is used to scale return moments. When FALSE, price is assumed to follow regular GBM with Hurst exponent = 0.5. 
\item "factorModel" - Factors model for computing portfolio metrics. Available models are: "sim" - portfolio metrics are computed using the Single Index Model, "direct" - portfolio metrics are computed using portfolio value itself. Defaults to "sim", which implies that the Single Index Model is used to compute portfolio metrics. 
\item "densityModel" - Density approximation model of return distribution. Available models are: "GLD" - Generalized Lambda Distribution, "CORNER_FISHER" - Corner-Fisher approximation, "NORMAL" - Gaussian distribution. Defaults to "GLD", which would fit a broad range of distribution shapes. 
\item "driftTerm" - Drift term (expected return) presence when computing probability density approximation and related metrics (e.g. CVaR, Omega Ratio, etc.). Defaults to FALSE, which implies that distribution is centered around zero return. 
\item "resultsNAFilter" - Used to enable filtering of NA values in computed results. Defaults to TRUE, which implies that output results have all NA values removed. 
\item "resultsSamplingInterval" - Interval to be used for sampling computed results before returning them to the caller. Available interval values are: "Xs" - seconds, "Xm" - minutes, "Xh" - hours, "Xd" - trading days (6.5 hours in a trading day), "Xw" - weeks (5 trading days in 1 week), "Xmo" - month (21 trading day in 1 month), "Xy" - years (256 trading days in 1 year), "last" - last result in a series is returned, "none" - no sampling. Large sampling interval would produce smaller vector of results and would require less time spent on data transfer. Default value of "1s" indicates that data is returned for every second during trading hours. 
\item "inputSamplingInterval" - Interval to be used as a minimum step for sampling input prices. Available interval values are: "Xs" - seconds, "Xm" - minutes, "Xh" - hours, "Xd" - trading days (6.5 hours in a trading day), "Xw" - weeks (5 trading days in 1 week), "Xmo" - month (21 trading day in 1 month), "Xy" - years (256 trading days in 1 year), "none" - no sampling. Default value is "none", which indicates that no sampling is applied. 
\item "timeScale" - Interval to be used for scaling return distribution statistics and producing metrics forecasts at different horizons. Available interval values are: "Xs" - seconds, "Xm" - minutes, "Xh" - hours, "Xd" - trading days (6.5 hours in a trading day), "Xw" - weeks (5 trading days in 1 week), "Xmo" - month (21 trading day in 1 month), "Xy" - years (256 trading days in 1 year), "all" - actual interval specified in during portfolio creation. Default value is "1d" - one trading day. 
\item "txnCostPerShare" - Amount of transactional costs per share. Defaults to 0. 
\item "txnCostFixed" - Amount of fixed costs per transaction. Defaults to 0. 
}}
}
\value{Void}
\description{Advanced settings that regulate how porfolio metrics are computed, returned and stored.
Default:
    \itemize{
\item	portfolioMetricsMode="portfolio",
\item	windowLength = "1d",
\item	holdingPeriodsOnly = FALSE,
\item	shortSalesMode = "lintner",
\item	synchronizationModel = TRUE,
\item	jumpsModel = "moments",
\item	noiseModel = TRUE,
\item	fractalPriceModel=TRUE,
\item	factorModel = "sim",
\item	densityModel="GLD",
\item	driftTerm=FALSE,
\item	resultsSamplingInterval = "1s",
\item	inputSamplingInterval="none",
\item	timeScale="1d",
\item	txnCostPerShare=0,
\item	txnCostFixed=0}}
\author{Kostin Andrey <andrey.kostin@portfolioeffect.com>}
\seealso{ \code{\link{portfolio_create}}  }
\examples{
\dontrun{
data(aapl.data) 
data(goog.data) 
data(spy.data) 
portfolio=portfolio_create(priceDataIx=spy.data)
portfolio_settings(portfolio,
				   windowLength='3600s', 
				   holdingPeriodsOnly=TRUE,
				   resultsSamplingInterval = '10s')
positionGOOG=position_add(portfolio,'GOOG',100,priceData=goog.data)   
positionAAPL=position_add(portfolio,'AAPL',300,priceData=aapl.data) 
print(portfolio)

dateStart = "2014-11-17 09:30:00"
dateEnd = "2014-11-17 16:00:00"
portfolio=portfolio_create(dateStart,dateEnd)
portfolio_settings(portfolio,portfolioMetricsMode="price",windowLength = '3600s',
resultsSamplingInterval='60s')
positionAAPL=position_add(portfolio,'AAPL',100)
positionC=position_add(portfolio,'C',300) 
positionGOOG=position_add(portfolio,'GOOG',150) 
plot(portfolio)
}}
\keyword{PortfolioEffectHFT}
%\concept{high frequency, intraday analytics, market data, portfolio, portfolio management,realtime analytics, risk, risk management, toolbox tools, trading, trading strategies}
\keyword{portfolio_settings}