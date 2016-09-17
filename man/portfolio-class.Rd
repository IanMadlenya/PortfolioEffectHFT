\name{portfolio-class}
\docType{class}
\alias{portfolio-class}
\alias{plot,portfolio,missing-method}
\alias{position_add,portfolio,character,ANY,ANY,matrix-method}
\alias{position_add,portfolio,character,ANY,ANY,missing-method}
\alias{position_add,portfolio,character,ANY,missing,matrix-method}
\alias{position_add,portfolio,character,ANY,missing,missing-method}
\alias{portfolio_create,portfolio,missing,missing,missing-method}
\alias{show,portfolio-method}

\title{Class \code{"portfolio"}}
\description{
Container class for storing portfolio parameters
}
\section{Slots}{
  \describe{
    \item{\code{java}:}{Object of class \code{"jobjRef"} ~~ }
    \item{\code{optimization_info}:}{Object of class \code{"ANY"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{plot}{\code{signature(x = "portfolio", y = "missing")}: ... }
    \item{position_add}{\code{signature(portfolio = "portfolio",
     symbol = "character", quantity = "ANY", time = "ANY", priceData = "matrix")}: ... }
    \item{position_add}{\code{signature(portfolio = "portfolio",
     symbol = "character", quantity = "ANY", time = "ANY", priceData = "missing")}: ... }
    \item{position_add}{\code{signature(portfolio = "portfolio",
     symbol = "character", quantity = "ANY", time = "missing", priceData = "matrix")}: ... }
    \item{position_add}{\code{signature(portfolio = "portfolio",
     symbol = "character", quantity = "ANY", time = "missing", priceData = "missing")}: ... }
    \item{show}{\code{signature(object = "portfolio")}: ... }
    \item{expected_return}{\code{signature("portfolio")}: ... }
	 }
}
\author{Kostin Andrey <andrey.kostin@portfolioeffect.com>}
\examples{
showClass("portfolio")
}
\keyword{PortfolioEffectHFT}
%\concept{high frequency, intraday analytics, market data, portfolio, portfolio management,realtime analytics, risk, risk management, toolbox tools, trading, trading strategies}
\keyword{classes}
