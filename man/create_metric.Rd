\docType{methods}
\name{create_metric}
\alias{create_metric}
\alias{create_metric,matrix,character-method}
\alias{create_metric,xts,character-method}
\title{Create metric class}
\usage{
%create_metric(asset,order)
\S4method{create_metric}{matrix,character}(metricData,symbol)
\S4method{create_metric}{xts,character}(metricData,symbol)
}
\arguments{
  \item{metricData}{Matrix of (time, value) rows or an xts object. Time should be in POSIX format expressed in milliseconds.}
  \item{symbol}{Unique identifier of the instrument}
}
\value{Metric object}
\description{Creates metric object using data provided in matrix or xts format.
}
\author{Kostin Andrey <andrey.kostin@portfolioeffect.com>}
\examples{
\dontrun{
data(aapl.data)
data(goog.data)
AAPL=create_metric(aapl.data,'AAPL')
GOOG=create_metric(goog.data,'GOOG')
plot(AAPL,GOOG,legend=c('AAPL','GOOG'))
}}
\keyword{PortfolioEffectHFT}
%\concept{high frequency, intraday analytics, market data, portfolio, portfolio management,realtime analytics, risk, risk management, toolbox tools, trading, trading strategies}
\keyword{create_metric}
