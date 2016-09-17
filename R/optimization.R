setClass("optimizer",
         slots = c(java="jobjRef",portfolio="jobjRef"))

optimization_run<-function(optimizer){
  util_validate()
  result<-optimizer@java$getOptimizedPortfolio()
  portfolio<-new("portfolio", java=getResult(result),optimization_info=NULL)
  portfolio@optimization_info=c(.jcall(result,returnSig="S", method="getInfoParam","nFunctionExecute"),.jcall(result,returnSig="S", method="getInfoParam","nGlobalStart"),.jcall(result,returnSig="S", method="getInfoParam","nLocalSolution"),.jcall(result,returnSig="S", method="getInfoParam","nOptimizations"),.jcall(result,returnSig="S", method="getInfoParam","nConstraintSatisfied"))
  return(portfolio)
}

optimization_constraint<-function(optimizer,
                                  constraintMertic,                              
                                  constraintType,
                                  constraintValue){
  constraintTypeFinal=constraintType[1]
  if(is.null(ncol(constraintValue))){
    optimizer@java$addConstraint(constraintMertic@java,constraintTypeFinal,as.double(constraintValue))
  }else{
    optimizer@java$addConstraint(constraintMertic@java,constraintTypeFinal,as.double(constraintValue[,2]),.jlong(util_dateToPOSIXTime(constraintValue[,1])))
  }
  return(optimizer)
}


optimization_goal<-function(goal,
                            direction=c("min","max"),		
                            approxError=1e-12,
                            optimumProbability=0.99)
{
  portfolio=goal@java$getPortfolio()
  if(!(direction[1] %in% c("min", "max"))) {
    stop("Direction not specified")
  }
  direction=switch(direction[1],min='minimize',max='maximize')
  if(.jcall(portfolio,returnSig="S", method="getParam","portfolioMetricsMode")=='portfolio'){
    path<-"com.portfolioeffect.quant.client.portfolio.optimizer.StrategyOptimizer"
    isStrategyOptimizer=T
  }else{
    path<-"com.portfolioeffect.quant.client.portfolio.optimizer.PortfolioOptimizer"
    isStrategyOptimizer=F
  }
  
  temp=.jnew(path,portfolio)
  temp$setOptimizationGoal(goal@java,direction)
  temp$setErrorInDecimalPoints(as.double(approxError))
  temp$setGlobalOptimumProbability(as.double(optimumProbability))
  optimizer<-new("optimizer", java=temp,portfolio=portfolio)
  if(isStrategyOptimizer){
    optimizer@java$setForecastBuilder(forecast_builder(goal)@java)
    optimizer@java$setForecastStep(portfolio$getSamplingInterval())
  }
  return(optimizer)
}

optimization_forecast<-function(optimizer,metricType,forecast){
  if(class(forecast)=='forecast'){
    switch(metricType,
           ExpReturn={optimizer@java$setExpectedReturnForecastBuilder(forecast@java)
           },
           Beta={optimizer@java$setBetaForecastBuilder(forecast@java)
           },
           Variance={optimizer@java$setVarianceForecastBuilder(forecast@java)
           },
           Cumulant3={optimizer@java$setCumulant3ForecastBuilder(forecast@java)
           },
           Cumulant4={optimizer@java$setCumulant4ForecastBuilder(forecast@java)
           },
           stop("Incorrect forecast metric type"))
  }else{
    if(class(forecast)=='metric'){
      if(forecast@java$getClass()==create_metric(matrix(1:20,ncol=2),"SPY")@java$getClass()){
        symbol=forecast@java$getDescription()
      }else{
        symbol=forecast@java$getSymbol()
      }
      data=compute(forecast)[[1]]
      forecastedValues=.jnew("com.portfolioeffect.quant.client.portfolio.optimizer.ForecastedValues",optimizer@portfolio)
      switch(metricType,
             ExpReturn={result<-forecastedValues$setSymbolForecastedExpReturn(symbol,as.double(data[,2]),.jlong(data[,1]))
             util_checkErrors(result)},
             Beta={result<-forecastedValues$setSymbolForecastedBeta(symbol,as.double(data[,2]),.jlong(data[,1]))
             util_checkErrors(result)},
             Variance={result<-forecastedValues$setSymbolForecastedVariance(symbol,as.double(data[,2]),.jlong(data[,1]))
             util_checkErrors(result)},
             Cumulant3={result<-forecastedValues$setSymbolForecastedCumulant3(symbol,as.double(data[,2]),.jlong(data[,1]))
             util_checkErrors(result)},
             Cumulant4={result<-forecastedValues$setSymbolForecastedCumulant4(symbol,as.double(data[,2]),.jlong(data[,1]))
             util_checkErrors(result)},
             stop("Incorrect forecast metric type"))
      .jcall(optimizer@java,returnSig="V",method="setForecastedValue",forecastedValues)
    }
  }
  
  return(optimizer)
}

optimization_info<-function(portfolio){
  if(!is.null(portfolio@optimization_info)){
    nFunctionExecute=portfolio@optimization_info[1]
    nGlobalStart=portfolio@optimization_info[2]
    nLocalSolution=portfolio@optimization_info[3]
    nOptimizations=portfolio@optimization_info[4]
    nConstraintSatisfied=portfolio@optimization_info[5]
    
    r<-NULL
    r<-rbind(r,paste('Total number of calls to optimization goal method ',  	as.double(nFunctionExecute)))
    r<-rbind(r,paste('Total number of search paths of the optimization algorithm ',				as.double(nGlobalStart)))
    r<-rbind(r,paste('Total number of times all optimization constraints were satisfied ',					as.double(nConstraintSatisfied)))
    r<-rbind(r,paste('Total number of local optima found by the optimization algorithm ',		as.double(nLocalSolution)))
    r<-rbind(r,paste('Total number of optimizations ',			as.double(nOptimizations)))
    r<-rbind(r,paste('Average number of calls to optimization goal method per optimization step ',			round(as.double(nFunctionExecute)/as.double(nOptimizations),digits =3)))
    r<-rbind(r,paste('Average number of search paths of the optimization algorithm per optimization step ',	round(as.double(nGlobalStart)/as.double(nOptimizations),digits =3)))
    r<-rbind(r,paste('Average number of times all optimization constraints were satisfied per optimization step ',		round(as.double(nConstraintSatisfied)/as.double(nOptimizations),digits =3)))
    r<-rbind(r,paste('Average number of local optima found by the optimization algorithm per optimization step ',       		round(as.double(nLocalSolution)/as.double(nOptimizations),digits =3)))
    r<-rbind(r,paste('Total number of local optima skipped by the optimization algorithm ',        	as.double(nLocalSolution)-as.double(nOptimizations)))
    r<-rbind(r,paste('Average number of search path per local optimum (the lower, the better) ',	round(as.double(nGlobalStart)/as.double(nLocalSolution),digits =3)))
    r<-rbind(r,paste('Average number of local optima skipped by the optimization algorithm per optimization step ',	round((as.double(nLocalSolution)-as.double(nOptimizations))/as.double(nOptimizations),digits =3)))		
    
    colnames(r)<-array("",dim=1)
    rownames(r)<-array("",dim=NROW(r))
    cat (paste("OPTIMIZATION INFO",sep="    "))
    print(r, quote=FALSE)
  }else{
    stop("given portfolio is not produced by optimization_run() method")
  }
}