\name{portfolio_defaultSettings}
\alias{portfolio_defaultSettings}
\title{Portfolio Default Settings}
\usage{portfolio_defaultSettings(portfolio)
}
\arguments{
  \item{portfolio}{Portfolio object created using \link[=portfolio_create]{portfolio_create( )} function}
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
\item	resultsNAFilter= TRUE,
\item	resultsSamplingInterval = "1s",
\item	inputSamplingInterval="1s",
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
positionGOOG=position_add(portfolio,'GOOG',100,priceData=goog.data)   
positionAAPL=position_add(portfolio,'AAPL',300,priceData=aapl.data) 
portfolio_defaultSettings(portfolio,
				   inputSamplingInterval = '10s',
				   resultsSamplingInterval = '10s')
portfolio_getSettings(portfolio)
portfolio_defaultSettings(portfolio)
portfolio_getSettings(portfolio)

}}
\keyword{PortfolioEffectHFT}
%\concept{high frequency, intraday analytics, market data, portfolio, portfolio management,realtime analytics, risk, risk management, toolbox tools, trading, trading strategies}
\keyword{portfolio_defaultSettings}