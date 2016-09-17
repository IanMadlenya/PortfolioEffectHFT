\name{position-class}
\docType{class}
\alias{position-class}
\alias{plot,position,missing-method}
\alias{show,position-method}

\title{Class \code{"position"}}
\description{
Container class for storing position parameters.
}
\section{Slots}{
\describe{
\item{\code{java}}{Reference to the corresponding Java position object}
\item{\code{portfolio}}{Portfolio object that includes this position.}
\item{\code{symbol}}{Unique identifier of the instrument.}
}
}
\section{Methods}{
  \describe{
    \item{plot}{\code{signature(x = "position", y = "missing")}: ... }
    \item{show}{\code{signature(object = "position")}: ... }
	 }
}
\author{Kostin Andrey <andrey.kostin@portfolioeffect.com>}
\examples{
showClass("position")
}
\keyword{PortfolioEffectHFT}
%\concept{high frequency, intraday analytics, market data, portfolio, portfolio management,realtime analytics, risk, risk management, toolbox tools, trading, trading strategies}
\keyword{classes}
