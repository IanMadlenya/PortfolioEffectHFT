\name{util_plot2df}
\alias{util_plot2df}
\title{Line plot of portfolio metric (for a dataframe)}
\usage{util_plot2df(formula,
data,
title=NULL,
subtitle=NULL,
font_size=10,
line_size=1.2,
bw=FALSE,
axis.text.size=1.5,
title.size=2)
}
\arguments{
  \item{formula}{Formula that describes data titles to be plotted.}
  \item{data}{Dataframe with metric data. Dataframe must have a variable 'legend'. If you want to correctly display the time in POSIX format, name the variable as 'time'.}  
  \item{title}{Plot title.}
  \item{subtitle}{Plot subtitle.}
  \item{font_size}{Baseline font size.}
  \item{line_size}{Line thickness.}
  \item{bw}{Black and white color scheme flag.}
  \item{axis.text.size}{Axis font size.}
  \item{title.size}{Title font size.}
}
\value{plot
}
\description{Draws a new line plot using a a dataframe with one or many metric values.}
\author{Kostin Andrey <andrey.kostin@portfolioeffect.com>}
\examples{
\dontrun{
portfolio=portfolio_create(fromTime="2014-10-02 09:30:00", toTime="2014-10-02 16:00:00")
portfolio_settings(portfolio,resultsSamplingInterval='60s')
positionSPY=position_add(portfolio,'SPY',500)
positionC=position_add(portfolio,'C',600)
metricSPY=compute(log_return(positionSPY))[[1]]
metricSPY=data.frame(metricSPY,legend="SPY return")
util_plot2df(value~time,metricSPY,title="Return, SPY")

metricC=compute(log_return(positionC))[[1]]
metricC=data.frame(metricC,legend="C return")
metric=rbind(metricSPY,metricC)
util_plot2df(value~time,metric,title="Return")

data(aapl.data) 
data(goog.data) 
data(spy.data) 
AAPLprice=data.frame(Price=aapl.data[,'Value'],time=aapl.data[,'Time'],legend='AAPL price')
GOOGprice=data.frame(Price=goog.data[,'Value'],time=goog.data[,'Time'],legend='GOOG price')
SPYprice=data.frame(Price=spy.data[,'Value'],time=spy.data[,'Time'],legend='SPY price')
price=rbind(AAPLprice,GOOGprice,SPYprice)
util_plot2df(Price~time,price,title="Stock prices")
}}
\keyword{PortfolioEffectHFT}
%\concept{plot market data, plot}
\keyword{util_plot2df}