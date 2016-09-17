############################################################
# Part 1 - Construct buy-and-hold portfolio
############################################################

require(PortfolioEffectHFT)


dateStart = "2014-10-01 09:30:00"
dateEnd = "2014-10-02 16:00:00"

# Create empty portfolio
portfolio<-portfolio_create(dateStart,dateEnd)

# Add position AAPL and GOOG to portfolio
positionAAPL=position_add(portfolio,"AAPL",100)
positionGOOG=position_add(portfolio,"GOOG",200)

# Display general information about the portfolio at the end of the period
print(portfolio)
plot(portfolio)
# util_screenshot('R-createPortfolio1.jpg')
############################################################
# Part 2 - Compute portfolio value
############################################################

print(plot(value(portfolio),title="Portfolio value, in USD"))
# util_screenshot('R-createPortfolio2.jpg')

############################################################
# Part 3 - Compute portfolio & position expected return (daily)
############################################################

print(plot(expected_return(positionAAPL),expected_return(positionGOOG),expected_return(portfolio),
           title="Expected Return, daily",legend=c("AAPL","GOOG","Portfolio")))
# util_screenshot('R-createPortfolio3.jpg')
############################################################
# Part 4 - Compute portfolio & position variance
############################################################

# Compute portfolio and position variance (daily)
print(plot(variance(positionAAPL),variance(positionGOOG),variance(portfolio),
           title="Variance, daily",legend=c("AAPL","GOOG","Portfolio")))
# util_screenshot('R-createPortfolio4.jpg')
############################################################
# Part 5 - Compute portfolio & position Value-at-Risk (daily, 95% c.i.)
############################################################
print(plot(value_at_risk(positionAAPL,0.95),value_at_risk(positionGOOG,0.95),value_at_risk(portfolio,0.95),
           title="Value at Risk in %, daily (95% c.i.)",legend=c("AAPL","GOOG","Portfolio")))
# util_screenshot('R-createPortfolio5.jpg')
############################################################
# Part 6 - Compute portfolio & position Sharpe Ratio (daily)
############################################################
print(plot(sharpe_ratio(positionAAPL),sharpe_ratio(positionGOOG),sharpe_ratio(portfolio),
           title="Sharpe Ratio, daily",legend=c("AAPL","GOOG","Portfolio")))
# util_screenshot('R-createPortfolio6.jpg')