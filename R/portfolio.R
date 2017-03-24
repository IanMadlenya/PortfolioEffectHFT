setClass("portfolio",
		slots = c(java="jobjRef",optimization_info="ANY")
)


setMethod ("show" , "portfolio" ,
			function (object){
			util_validate()
			portfolio=portfolio_create(object)
			set<-portfolio_getSettings(portfolio)
			r<-NULL
			r<-rbind(r,c('Portfolio metrics mode',  	set$portfolioMetricsMode))
			r<-rbind(r,c('Window length ',				set$windowLength))
			r<-rbind(r,c('Time scale ',					set$timeScale))
			r<-rbind(r,c('Holding periods only ',		set$holdingPeriodsOnly))
			r<-rbind(r,c('Short sales mode',			set$shortSalesMode))
			r<-rbind(r,c('Price jumps model ',			set$jumpsModel))
			r<-rbind(r,c('Microstructure noise model ',	set$noiseModel))
			r<-rbind(r,c('Fractal price model ',	    set$fractalPriceModel))			
			r<-rbind(r,c('Portfolio factor model ',		set$factorModel))
			r<-rbind(r,c('Density model ',       		set$densityModel))
			r<-rbind(r,c('Drift term enabled ',        	set$driftTerm))
			r<-rbind(r,c('Results NA filter ',        	set$resultsNAFilter))
			r<-rbind(r,c('Results sampling interval ',	set$resultsSamplingInterval))
			r<-rbind(r,c('Input sampling interval ',	set$inputSamplingInterval))
			r<-rbind(r,c('Transaction cost per share ',	set$txnCostPerShare))
			r<-rbind(r,c('Transaction cost fixed ',     set$txnCostFixed))			
			
		colnames(r)<-array("",dim=2)
		rownames(r)<-array("",dim=NROW(r))
		cat (paste("PORTFOLIO SETTINGS",sep="    "))
			print(r, quote=FALSE)
			
			portfolio@java$setParam("resultsSamplingInterval","last")
			portfolio@java$setNaNFiltered(as.logical(F))
			positions=portfolio@java$getPositions()
			symbols=array('1',dim=length(positions))
			.jcall(portfolio@java,returnSig="V", method="createCallGroup",as.integer(1+length(symbols)))

			temp=compute(profit(portfolio),log_return(portfolio),value(portfolio))
			

			printMatrix1<-matrix(0,ncol=6,nrow=length(symbols))
			for(i in 1:length(positions)){
			  positionTemp=new('position',java=positions[[i]],symbol=positions[[i]]$getName(),portfolio=positions[[i]]$getPortfolio())
			  symbols[i]=positionTemp@symbol
			  tempPosition=compute(quantity(positionTemp),weight(positionTemp),profit(positionTemp),log_return(positionTemp),value(positionTemp),price(positionTemp))
			  printMatrix1[i,1]<-tempPosition[[1]][-1]
			  printMatrix1[i,2]<-round(tempPosition[[2]][-1]*100,digits =3)
			  printMatrix1[i,3]<-round(tempPosition[[3]][-1],digits =3)
			  printMatrix1[i,4]<-round(tempPosition[[4]][-1],digits =3)*100
			  printMatrix1[i,5]<-tempPosition[[5]][-1]
			  printMatrix1[i,6]<-tempPosition[[6]][-1]
			}

			dimnames(printMatrix1) = list( symbols, c("  Quantity","  Weight (in %)","  Profit","  Return (in %)","  Value","  Price"))
			symbols<-symbols[order(printMatrix1[,"  Weight (in %)"],decreasing=TRUE)]
			printMatrix1<-printMatrix1[symbols,]

			printMatrix2<-matrix(0,ncol=3,nrow=1)
			printMatrix2[1,1]<-round(temp[[1]][2],digits =3)
			printMatrix2[1,2]<-round(temp[[2]][2],digits =3)*100
			printMatrix2[1,3]<-temp[[3]][2]
			dimnames(printMatrix2) = list( "Portfolio", c("  Profit","  Return (in %)","  Value"))
			cat (paste("\n\n","POSITION SUMMARY","\n",sep="    "))
			print(printMatrix1)
			cat (paste("\n\n","PORTFOLIO SUMMARY","\n",sep=""))
			print(printMatrix2)
})

setMethod("plot" ,c(x="portfolio",y="ANY"),function(x,y,...){
  util_summaryPlot(x,y,...)
})
setMethod("plot" ,c(x="portfolio",y="missing"),function(x,...){
  util_summaryPlot(x,...)
})

portfolio_defaultSettings<-function(portfolio){
	portfolio_settings(portfolio,portfolioMetricsMode="portfolio",
				windowLength = "1d",
				holdingPeriodsOnly = FALSE,
				shortSalesMode = "lintner",
				synchronizationModel = TRUE,
				jumpsModel = "moments",
				noiseModel = TRUE,
				fractalPriceModel=TRUE,
				factorModel = "sim",
				densityModel="GLD",
				driftTerm=FALSE,
				resultsNAFilter= TRUE,
				resultsSamplingInterval = "1s",
				inputSamplingInterval="1s",
				timeScale="1d",
				txnCostPerShare=0,
				txnCostFixed=0)
}
portfolio_settings<-function(
		portfolio,...
		){
	isList=FALSE
	try({if(is.list(...)){isList=TRUE}},silent = TRUE)
	if(isList){change=(...)}else{change=list(...)}
	for(i in 1:length(change)){
		if(!(names(change[i]) %in% c( "portfolioMetricsMode","windowLength","holdingPeriodsOnly",     
								"shortSalesMode","jumpsModel","noiseModel", "fractalPriceModel",             
								"factorModel","resultsSamplingInterval","inputSamplingInterval",  
								"timeScale","driftTerm","txnCostPerShare",        
								"txnCostFixed","densityModel",'resultsNAFilter','synchronizationModel' ))){
				stopMessage('WRONG_SETTINGS_ARGUMENTS')
		}
	  if(is.logical(change[[i]])){
	    if(change[[i]]){
	    change[[i]]='true'
	    }else{
	   change[[i]]='false'
	  }}
	.jcall(portfolio@java,returnSig="V", method="setParam",names(change[i]),paste(change[[i]]))
	}
}

portfolio_settingsRiskMetrics<-function(portfolio){
	portfolio_settings(portfolio,portfolioMetricsMode="price",
			windowLength = "1s",
			holdingPeriodsOnly = FALSE,
			shortSalesMode = "lintner",
			jumpsModel = "none",
			noiseModel = FALSE,
			fractalPriceModel=FALSE,
			factorModel = "sim",
			densityModel="NORMAL",
			driftTerm=FALSE,
			resultsNAFilter= TRUE,
			resultsSamplingInterval = "1d",
			inputSamplingInterval="1d",
			timeScale="1d",
			txnCostPerShare=0,
			txnCostFixed=0)
	.jcall(portfolio@java,returnSig="V", method="setParam","riskMethodology", "RiskMetrics")
}

portfolio_getSettings<-function(portfolio){

	temp<-list()
	temp$portfolioMetricsMode <- .jcall(portfolio@java,returnSig="S", method="getParam","portfolioMetricsMode")
	temp$windowLength<-.jcall(portfolio@java,returnSig="S", method="getParam","windowLength")
	temp$holdingPeriodsOnly<-as.logical(.jcall(portfolio@java,returnSig="S", method="getParam","isHoldingPeriodEnabled"))
	temp$shortSalesMode<-.jcall(portfolio@java,returnSig="S", method="getParam","shortSalesMode")
	temp$jumpsModel<-.jcall(portfolio@java,returnSig="S", method="getParam","jumpsModel")
	temp$noiseModel<-as.logical(.jcall(portfolio@java,returnSig="S", method="getParam","isNoiseModelEnabled"))
	temp$fractalPriceModel<-as.logical(.jcall(portfolio@java,returnSig="S", method="getParam","isFractalPriceModelEnabled"))
	temp$factorModel<-.jcall(portfolio@java,returnSig="S", method="getParam","factorModel")
	temp$resultsNAFilter<-.jcall(portfolio@java,returnSig="Z", method="isNaNFiltered")
	temp$resultsSamplingInterval<-.jcall(portfolio@java,returnSig="S", method="getParam","samplingInterval")
	temp$inputSamplingInterval<-.jcall(portfolio@java,returnSig="S", method="getParam","priceSamplingInterval")
	temp$timeScale<-.jcall(portfolio@java,returnSig="S", method="getParam","timeScale")
	temp$driftTerm<-as.logical(.jcall(portfolio@java,returnSig="S", method="getParam","isDriftEnabled"))
	temp$txnCostPerShare<-.jcall(portfolio@java,returnSig="S", method="getParam","txnCostPerShare")
	temp$txnCostFixed<-.jcall(portfolio@java,returnSig="S", method="getParam","txnCostFixed")	
	temp$densityModel<-.jcall(portfolio@java,returnSig="S", method="getParam","densityApproxModel")
	temp
}

portfolio_create<-function(index=NULL,fromTime=NULL,toTime=NULL,priceDataIx=NULL){
	
	if(is.null(index) & is.null(fromTime)& is.null(toTime)& is.null(priceDataIx)){
		stop('No arguments provided, please check required arguments list.')
	}
	if(((class(index)=="matrix")&(is.null(priceDataIx)))||((class(priceDataIx)=="matrix")&(is.null(index)))){
		if((class(index)=="matrix")&(is.null(priceDataIx))){
			priceDataIx=index
		}
		util_validate()
		clientConnection=getOption('clientConnection')
		portfolio=new("portfolio", java=.jnew("com.portfolioeffect.quant.client.portfolio.Portfolio",clientConnection),optimization_info=NULL)
		result<-.jcall(portfolio@java,returnSig="Lcom/portfolioeffect/quant/client/result/Metric;", method="addIndex", as.double(priceDataIx[,2]),.jlong(priceDataIx[,1]))
		util_checkErrors(result)
		portfolio_defaultSettings(portfolio)
		return(portfolio)
	}
	if(((class(fromTime)=="character")&(class(toTime)=="character")&(is.null(priceDataIx)))||((class(fromTime)=="character")&(class(index)=="character")&(is.null(toTime))&(is.null(priceDataIx)))){
		if(is.null(index)){
			index="SPY"
		}
		if(((class(fromTime)=="character")&(class(index)=="character")&(is.null(toTime))&(is.null(priceDataIx)))){
			toTime=fromTime
			fromTime=index
			index="SPY"
		}
		util_validate()
		clientConnection=getOption('clientConnection')
		portfolio=new("portfolio", java=.jnew("com.portfolioeffect.quant.client.portfolio.Portfolio",clientConnection,fromTime,toTime,index),optimization_info=NULL)
# 		result<-.jcall(portfolio@java,returnSig="Lcom/portfolioeffect/quant/client/result/Metric;", method="setFromTime", fromTime)
# 		util_checkErrors(result)
# 		result<-.jcall(portfolio@java,returnSig="Lcom/portfolioeffect/quant/client/result/Metric;", method="setToTime", toTime)
# 		util_checkErrors(result)
# 		result<-.jcall(portfolio@java,returnSig="Lcom/portfolioeffect/quant/client/result/Metric;", method="addIndex", index)
# 		util_checkErrors(result)
		portfolio_defaultSettings(portfolio)
		return(portfolio)
	}
	if((class(index)=="portfolio")&(is.null(fromTime))&(is.null(toTime))&(is.null(priceDataIx))){
		util_validate()
		portfolio=new("portfolio", java=.jnew("com.portfolioeffect.quant.client.portfolio.Portfolio",index@java),optimization_info=NULL)
		return(portfolio)
	}
	if(!exists('portfolio')){
		stop('Could not create portfolio object.')
	}
}

# position_quantity<-function(portfolio,symbol){
# 	result<-.jcall(portfolio@java,returnSig="Lcom/portfolioeffect/quant/client/result/Metric;", method="getPositionQuantity",symbol)  
# 	util_checkErrors(result)
# 	result<-getResultValuesDoubleArrayWithTime(result)
# 	return(result)}

position_list<-function(portfolio){
	result<-.jcall(portfolio@java,returnSig="[S", method="getSymbols")  
	return(result)}


#position_returnJumpSize<-function(portfolio,symbol){
#	portfolioTemp=portfolio_create(portfolio)
#	portfolio_settings(portfolioTemp,jumpsModel='none')
#    time=position_price(portfolioTemp,symbol)[,1]
#	priceNoJumpsFilter=position_price(portfolioTemp,symbol)[,2]
#	portfolio_settings(portfolioTemp,jumpsModel='all')
#	priceJumpsFilter=position_price(portfolioTemp,symbol)[,2]
#	jumps=log(priceNoJumpsFilter)-log(priceJumpsFilter)
#	cbind(time,jumps)
#}

# portfolio_startBatch<-function(portfolio){
# 	.jcall(portfolio@java,returnSig="V", method="startBatch")
# }
# 
# portfolio_endBatch<-function(portfolio){
# 	result=.jcall(portfolio@java,returnSig="Lcom/portfolioeffect/quant/client/result/Metric;", method="finishBatch")
# 	util_checkErrors(result)
# }


portfolio_availableSymbols<-function(){
  util_validate()
  clientConnection=getOption('clientConnection')
  requestToServer=.jnew("com.portfolioeffect.quant.client.RequestToServer")
  result=requestToServer$getAllSymbolsList(clientConnection)
  result<-getResult(result)
	return(result)
}

portfolio_getPosition<-function(portfolio,symbol){
  positions=portfolio@java$getPositions()
  symbols=portfolio@java$getSymbols()
  num=(1:length(positions))[symbol==symbols]
  position=new("position", java=positions[[num]],symbol=symbol,portfolio=portfolio@java)
  return(position)
}