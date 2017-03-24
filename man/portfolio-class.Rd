\name{portfolio-class}
\docType{class}
\alias{portfolio-class}
\alias{plot,portfolio,missing-method}
\alias{plot,portfolio,ANY-method}
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
    \item{plot}{\code{signature(x = "portfolio", y = "ANY")}: 
    arguments:    
    \itemize{
  \item "font_size" - Baseline font size.
  \item "line_size" -Line thickness.
  \item "bw" -Black and white color scheme flag.
  \item "axis.text.size" -Axis font size.
  \item "title.size" -Title font size.
} }
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
\dontrun{
portfolio=portfolio_create(fromTime="2014-10-02 09:30:00", toTime="2014-10-02 16:00:00")
portfolio_settings(portfolio,resultsSamplingInterval='60s')
positionSPY=position_add(portfolio,'SPY',500)
positionC=position_add(portfolio,'C',600)
plot(portfolio,font_size=7,bw=T)
}
}
\keyword{PortfolioEffectHFT}
%\concept{high frequency, intraday analytics, market data, portfolio, portfolio management,realtime analytics, risk, risk management, toolbox tools, trading, trading strategies}
\keyword{classes}
