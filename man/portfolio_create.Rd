\name{portfolio_create}
\alias{portfolio_create}
\title{Creates new portfolio}
\usage{portfolio_create(index, fromTime, toTime, priceDataIx)
}
\arguments{
  \item{index}{Index symbol that should be used in the Single Index Model. Defaults to "SPY".}
  \item{fromTime}{Start of market data interval in "yyyy-MM-dd hh:mm:ss" format when internal market data is used. 
   Offset from last available date/time by N days is denoted as "t-N" (e.g. "t-7" denotes offset by 7 days).}
  \item{toTime}{End of market data interval in "yyyy-MM-dd hh:mm:ss" format when internal market data is used.
   Offset from last available date/time by N days is denoted as "t-N" (e.g. "t-7" denotes offset by 7 days).}
  \item{priceDataIx}{Vector of (time, price) observations for market index asset when external market data is used.  }
}
\value{portfolio object
}
\description{Creates new empty portfolio.\cr
To add positions use \link[=position_add]{position_add( )}.\cr
To remove positions use \link[=position_remove]{position_remove( )}.
}
\author{Kostin Andrey <andrey.kostin@portfolioeffect.com>}
\seealso{\code{\link{position_add}} \code{\link{portfolio_settings}} \code{\link{position_remove}} }
\examples{
\dontrun{
data(aapl.data) 
data(goog.data) 
data(spy.data) 
portfolio=portfolio_create(priceDataIx=spy.data)
portfolio_settings(portfolio,windowLength = '3600s',resultsSamplingInterval='60s')
positionGOOG=position_add(portfolio,'GOOG',100,priceData=goog.data)  
positionAAPL=position_add(portfolio,'AAPL',300,priceData=aapl.data) 
result=compute(alpha_exante(portfolio),alpha_exante(positionGOOG),alpha_exante(positionAAPL)) 
plot(alpha_exante(portfolio),alpha_exante(positionGOOG),alpha_exante(positionAAPL),
legend=c('Portfolio','GOOG','AAPL'),title='Alpha')
print(portfolio)

portfolio=portfolio_create(priceDataIx=spy.data)
portfolio_settings(portfolio,windowLength = '3600s',resultsSamplingInterval='60s')
positionGOOG=position_add(portfolio,'GOOG',c(100,200),time=c(1412256601000,1412266600000),
priceData=goog.data) 
positionAAPL=position_add(portfolio,'AAPL',c(300,150),time=c(1412266600000,1412276600000),
priceData=aapl.data) 
plot(expected_return(portfolio),title="Expected Return")

portfolio=portfolio_create(fromTime="2014-09-01 09:00:00", toTime="2014-09-14 16:00:00")
portfolio_settings(portfolio,windowLength = '3600s',resultsSamplingInterval='60s')
positionSPY=position_add(portfolio,'SPY',500)
positionC=position_add(portfolio,'C',600)
plot(expected_return(portfolio),title="Portfolio Expected Return")

portfolio=portfolio_create(fromTime="2014-10-02 09:30:00", toTime="2014-10-02 16:00:00")
portfolio_settings(portfolio,windowLength = '3600s',resultsSamplingInterval='60s')
positionSPY=position_add(portfolio,'SPY',500)
positionC=position_add(portfolio,'C',600)
positionGOOG=position_add(portfolio,'GOOG',100,priceData=goog.data) 
position_add(portfolio,'AAPL',c(300,150),time=c(1412266600000,1412276600000),
priceData=aapl.data)
plot(expected_return(portfolio),title="Portfolio Expected Return")

portfolio=portfolio_create(fromTime="t-2", toTime="t")
portfolio_settings(portfolio,windowLength = '3600s',resultsSamplingInterval='60s')
positionSPY=position_add(portfolio,'SPY',500)
positionC=position_add(portfolio,'C',600)
plot(expected_return(portfolio),title="Portfolio Expected Return")
}}
\keyword{PortfolioEffectHFT}
%\concept{high frequency, intraday analytics, market data, portfolio, portfolio management,realtime analytics, risk, risk management, toolbox tools, trading, trading strategies}
\keyword{portfolio_create}