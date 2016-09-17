\name{util_plotDensity}
\alias{util_plotDensity}
\title{Line plot of probability density}
\usage{util_plotDensity(PDF, bw=FALSE)
}
\arguments{ 
  \item{PDF}{List of density values produced by portfolio or position pdf methods.}
  \item{bw}{Black and white color scheme flag.}
}
\value{plot
}
\description{Plot probability density by produced by the \link[=dist_density]{dist_density( )} functions.}
\author{Kostin Andrey <andrey.kostin@portfolioeffect.com>}
\examples{
\dontrun{
data(aapl.data) 
data(goog.data) 
data(spy.data) 
portfolio=portfolio_create(priceDataIx=spy.data)
portfolio_settings(portfolio,windowLength = '3600s',resultsSamplingInterval='60s')
positionGOOG=position_add(portfolio,'GOOG',100,priceData=goog.data)   
positionAAPL=position_add(portfolio,'AAPL',300,priceData=aapl.data) 
util_plotDensity(dist_density(portfolio,pValueLeft=0.2,pValueRight=0.8,nPoints=100,
addNormalDensity=TRUE))

dateStart = "2014-11-17 09:30:00"
dateEnd = "2014-11-17 16:00:00"
portfolio=portfolio_create(dateStart,dateEnd)
portfolio_settings(portfolio,portfolioMetricsMode="price",windowLength = '3600s',
resultsSamplingInterval='60s')
positionAAPL=position_add(portfolio,'AAPL',100)
positionC=position_add(portfolio,'C',300) 
positionGOOG=position_add(portfolio,'GOOG',150) 
util_plotDensity(dist_density(portfolio,pValueLeft=0,pValueRight=1,nPoints=100,
addNormalDensity=TRUE))
}}
\keyword{PortfolioEffectHFT}
%\concept{plot market data, plot}
\keyword{util_plotDensity}