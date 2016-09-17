\name{metric-class}
\docType{class}
\alias{metric-class}
\alias{plot,metric,missing-method}
\alias{plot,metric,ANY-method}
\alias{+,metric,numeric-method}
\alias{-,metric,numeric-method}
\alias{*,metric,numeric-method}
\alias{/,metric,numeric-method}
\alias{+,metric,metric-method}
\alias{-,metric,metric-method}
\alias{*,metric,metric-method}
\alias{/,metric,metric-method}
\alias{show,metric-method}

\title{Class \code{"metric"}}
\description{
Class that incorporates the notion of "lazy" portfolio or position metric.
}
\section{Slots}{
  \describe{
    \item{\code{java}:}{Object of class \code{"jobjRef"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{plot}{\code{signature(x = "metric", y = "missing")}: Displays summary plot of a metric }
    \item{plot}{\code{signature(x = "metric", y = "ANY")}: Displays summary plot of a metric }
    \item{show}{\code{signature(object = "metric")}: Displays summary plot of a metric}
    \item{+}{\code{signature("metric","numeric")}: Adds number to all metric elements}
    \item{+}{\code{signature("metric","metric")}: Adds one metric to another element-by-element}
    \item{-}{\code{signature("metric","numeric")}: Subtracts number from all metric elements}
    \item{-}{\code{signature("metric","metric")}: Subtracts one metric from another element-by-element}
    \item{*}{\code{signature("metric","numeric")}: Multiplies all metric elements by a number}
    \item{*}{\code{signature("metric","metric")}: Multiplies one metric by another element-by-element}
    \item{/}{\code{signature("metric","numeric")}: Divides all metric elements by a number}
    \item{/}{\code{signature("metric","metric")}: Divides one metric by another element-by-element}
	 }
}
\author{Kostin Andrey <andrey.kostin@portfolioeffect.com>}
\examples{
showClass("metric")
}
\keyword{PortfolioEffectHFT}
%\concept{high frequency, intraday analytics, market data, portfolio, portfolio management,realtime analytics, risk, risk management, toolbox tools, trading, trading strategies}
\keyword{classes}
