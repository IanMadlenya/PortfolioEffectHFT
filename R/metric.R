setClass("metric",
		slots = c(java="jobjRef")
)

position_metric=function(argList,portfolio,...){
	util_validate()
	data=list(...)
	result=NULL
   	builder=.jnew("com.portfolioeffect.quant.client.util.LazyMetricBuilder",toJSONpe(data))
	  result=builder$build(portfolio@java)
	metric=new("metric", java=result)
	return(metric)
}

toJSONpe=function(x){
  names=names(x)
  result='{'
  t=0
  for(name in names){
    t=t+1
    if(t<=1){
      result=paste(result,'"',name,'"',':','"',x[[name]],'"',sep="")
    }else{
      result=paste(result,', "',name,'"',':','"',x[[name]],'"',sep="")
    }
  }
  result=paste(result,'}',sep="")
  return(result)
}

compute=function(...){
  util_validate()
data=c(...)
result=NULL
for(i in 1:length(data)){
result[[data[[i]]@java$getDescription()]]=getResult(data[[i]]@java,metricClass=T)
}
return(result)}

create_metric=function(metricData,symbol){
}

setMethod("create_metric", signature(metricData = "matrix",symbol="character"), function(metricData,symbol) {
  util_validate()
  builder=.jnew("com.portfolioeffect.quant.client.result.SavedMetric",as.double(metricData[,2]),.jlong(metricData[,1]))
  builder$setDescription(symbol)
  return(new('metric',java=builder))
})
setMethod("create_metric", signature(metricData = "xts",symbol="character"), function(metricData,symbol) {
  util_validate()
  builder=.jnew("com.portfolioeffect.quant.client.result.SavedMetric",as.double(metricData),.jlong(util_dateToPOSIXTime(paste(index(metricData)))))
  builder$setDescription(symbol)
  return(new('metric',java=builder))
})


setMethod("+", signature(e1 = "metric", e2 = "numeric"), function(e1, e2) {
  temp=compute(e1)[[1]]
  temp[,2]=temp[,2]+e2
  builder=.jnew("com.portfolioeffect.quant.client.result.SavedMetric",as.double(temp[,2]),.jlong(temp[,1]))
  builder$setDescription(paste("New",e1@java$getDescription()))
  return(new('metric',java=builder))
})

setMethod("-", signature(e1 = "metric", e2 = "numeric"), function(e1, e2) {
  temp=compute(e1)[[1]]
  temp[,2]=temp[,2]-e2
  builder=.jnew("com.portfolioeffect.quant.client.result.SavedMetric",as.double(temp[,2]),.jlong(temp[,1]))
  builder$setDescription(paste("New",e1@java$getDescription()))
  return(new('metric',java=builder))
})

setMethod("*", signature(e1 = "metric", e2 = "numeric"), function(e1, e2) {
  temp=compute(e1)[[1]]
  temp[,2]=temp[,2]*e2
  builder=.jnew("com.portfolioeffect.quant.client.result.SavedMetric",as.double(temp[,2]),.jlong(temp[,1]))
  builder$setDescription(paste("New",e1@java$getDescription()))
  return(new('metric',java=builder))
})

setMethod("/", signature(e1 = "metric", e2 = "numeric"), function(e1, e2) {
  temp=compute(e1)[[1]]
  temp[,2]=temp[,2]/e2
  builder=.jnew("com.portfolioeffect.quant.client.result.SavedMetric",as.double(temp[,2]),.jlong(temp[,1]))
  builder$setDescription(paste("New",e1@java$getDescription()))
  return(new('metric',java=builder))
})
  
setMethod("+", signature(e1 = "metric", e2 = "metric"), function(e1, e2) {
  temp=compute(e1,e2)
  temp1=temp[[1]]
  temp2=temp[[2]]
  temp1=temp1[temp1[,1] %in% temp2[,1],]
  temp2=temp2[temp2[,1] %in% temp1[,1],] 
  builder=.jnew("com.portfolioeffect.quant.client.result.SavedMetric",as.double(temp1[,2]+temp2[,2]),.jlong(temp1[,1]))
  builder$setDescription(paste("New",e1@java$getDescription()))
  return(new('metric',java=builder))
})

setMethod("-", signature(e1 = "metric", e2 = "metric"), function(e1, e2) {
  temp=compute(e1,e2)
  temp1=temp[[1]]
  temp2=temp[[2]]
  temp1=temp1[temp1[,1] %in% temp2[,1],]
  temp2=temp2[temp2[,1] %in% temp1[,1],] 
  builder=.jnew("com.portfolioeffect.quant.client.result.SavedMetric",as.double(temp1[,2]-temp2[,2]),.jlong(temp1[,1]))
  builder$setDescription(paste("New",e1@java$getDescription()))
  return(new('metric',java=builder))
})

setMethod("*", signature(e1 = "metric", e2 = "metric"), function(e1, e2) {
  temp=compute(e1,e2)
  temp1=temp[[1]]
  temp2=temp[[2]]
  temp1=temp1[temp1[,1] %in% temp2[,1],]
  temp2=temp2[temp2[,1] %in% temp1[,1],] 
  builder=.jnew("com.portfolioeffect.quant.client.result.SavedMetric",as.double(temp1[,2]*temp2[,2]),.jlong(temp1[,1]))
  builder$setDescription(paste("New",e1@java$getDescription()))
  return(new('metric',java=builder))
})

setMethod("/", signature(e1 = "metric", e2 = "metric"), function(e1, e2) {
  temp=compute(e1,e2)
  temp1=temp[[1]]
  temp2=temp[[2]]
  temp1=temp1[temp1[,1] %in% temp2[,1],]
  temp2=temp2[temp2[,1] %in% temp1[,1],] 
  builder=.jnew("com.portfolioeffect.quant.client.result.SavedMetric",as.double(temp1[,2]/temp2[,2]),.jlong(temp1[,1]))
  builder$setDescription(paste("New",e1@java$getDescription()))
  return(new('metric',java=builder))
})

setMethod("show" ,"metric",function (object){
  util_validate()
table=object@java$getDescription()
print(table)})

util_metricPlot=function(...){
  data=list(...)
  temp=NULL
  j=0
  for(i in 1:length(data)){
    if(class(data[[i]])=="metric"){
      j=j+1
      tempData=compute(data[[i]])[[1]]
      legend=if(is.null(data[["legend"]])){data[[i]]@java$getDescription()}else{data$legend[j]}
      temp=rbind(temp,data.frame(value=tempData[,2],time=tempData[,1],legend=legend))
      
    }}
  util_plot2df(value~time,temp,
               title=if(is.null(data[["title"]])){NULL}else{data$title},
               subtitle=if(is.null(data[["subtitle"]])){NULL}else{data$subtitle},
               font_size=if(is.null(data[["font_size"]])){10}else{data$font_size},
               line_size=if(is.null(data[["line_size"]])){1.2}else{data$line_size},
               bw=if(is.null(data[["bw"]])){FALSE}else{data$bw},
               axis.text.size=if(is.null(data[["axis.text.size"]])){1.5}else{data$axis.text.size},
               title.size=if(is.null(data[["title.size"]])){2}else{data$title.size}
  )
}
setMethod("plot" ,c(x="metric",y="ANY"),function(x,y,...){
  util_metricPlot(x,y,...)
})
setMethod("plot" ,c(x="metric",y="missing"),function(x,...){
  util_metricPlot(x,...)
})

# #' Method value
# #' @name value
# #' @rdname value
# #' @exportMethod value
# setGeneric("value", if(exists("value")){NULL}else{ function(asset)  standardGeneric("value")})
# #' @rdname value
# #' @aliases value,portfolio-method
# setMethod("value" ,"portfolio",function (asset){
#   result=position_metric(argList=as.list(environment()),portfolio=asset,metric="VALUE")
#   return(result)})
# #' @rdname value
# #' @aliases value,position-method
# setMethod("value" ,"position",function (asset){
#   result=position_metric(argList=as.list(environment()),portfolio=asset,metric="VALUE")
#   return(result)})
# 
# #' @rdname beta
# #' @export 
# setGeneric("beta", if(exists("beta")){NULL}else{ function(asset)
#   standardGeneric("beta")}
# )
# 
# #' @rdname beta
# setMethod("beta" ,"portfolio",function (a){
#   result=position_metric(argList=as.list(environment()),portfolio=a,metric="BETA")
#   return(result)})
# 
# #' @rdname beta
# setMethod("beta" ,"position",function (a){
#   result=position_metric(argList=as.list(environment()),portfolio=a,metric="BETA")
#   return(result)})

#' Method alpha_exante
#' @name alpha_exante
#' @rdname alpha_exante
#' @exportMethod alpha_exante
setGeneric("alpha_exante", if(exists("alpha_exante")){NULL}else{ function(asset)  standardGeneric("alpha_exante")})
#' @rdname alpha_exante
#' @aliases alpha_exante,portfolio-method
setMethod("alpha_exante",signature(asset="portfolio"),function (asset){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="ALPHA")
  return(result)})
#' @rdname alpha_exante
#' @aliases alpha_exante,position-method
setMethod("alpha_exante",c("position"),function (asset){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="ALPHA")
  return(result)})
#' Method beta
#' @name beta
#' @rdname beta
#' @exportMethod beta
setGeneric("beta", if(exists("beta")){NULL}else{ function(asset)  standardGeneric("beta")})
#' @rdname beta
#' @aliases beta,portfolio-method
setMethod("beta","portfolio",function (a){
  result=position_metric(argList=as.list(environment()),portfolio=a,metric="BETA")
  return(result)})
#' @rdname beta
#' @aliases beta,position-method
setMethod("beta","position",function (a){
  result=position_metric(argList=as.list(environment()),portfolio=a,metric="BETA")
  return(result)})
#' Method correlation
#' @name correlation
#' @rdname correlation
#' @exportMethod correlation
setGeneric("correlation", if(exists("correlation")){NULL}else{ function(positionA,positionB)  standardGeneric("correlation")})
#' @rdname correlation
#' @aliases correlation,portfolio-method
setMethod("correlation",c("portfolio","missing"),function (positionA){
  portfolioTemp=portfolio_create(positionA)
  positions=portfolioTemp@java$getPositions()
  symbols=array('1',dim=length(positions))
  for(i in 1:length(positions)){
    symbols[i]=positions[[i]]$getName()
  }
  set=portfolio_getSettings(portfolioTemp)
  .jcall(portfolioTemp@java,returnSig="V", method="setSamplingInterval","last")
  
  n=length(symbols)
  resultMatrix=matrix(1,nrow=n,ncol=n)
  .jcall(portfolioTemp@java,returnSig="V", method="createCallGroup",as.integer(((n*n-n)/2)))
  for(i in 2:n){
    for(j in 1:(i-1)){
      builder=.jnew("com.portfolioeffect.quant.client.util.LazyMetricBuilder",toJSONpe(list(metric="CORRELATION")))
      result=new('metric',java=builder$build(positions[[i]],positions[[j]]))
      resultMatrix[i,j]=compute(result)[[1]][2]
      resultMatrix[j,i]=resultMatrix[i,j]
    }
  }
  for(i in 1:n){
    resultMatrix[i,i]=1
  }
  dimnames(resultMatrix) = list(symbols,symbols)
  return(resultMatrix)})
#' @rdname correlation
#' @aliases correlation,position-method
setMethod("correlation",c("position","position"),function (positionA,positionB){
  builder=.jnew("com.portfolioeffect.quant.client.util.LazyMetricBuilder",toJSONpe(list(metric="CORRELATION")))
  metric=new('metric',java=builder$build(positionA@java,positionB@java))
  return(metric)})
#' Method covariance
#' @name covariance
#' @rdname covariance
#' @exportMethod covariance
setGeneric("covariance", if(exists("covariance")){NULL}else{ function(positionA,positionB)  standardGeneric("covariance")})
#' @rdname covariance
#' @aliases covariance,portfolio-method
setMethod("covariance",c("portfolio","missing"),function (positionA){
  portfolioTemp=portfolio_create(positionA)
    positions=portfolioTemp@java$getPositions()
    symbols=array('1',dim=length(positions))
    for(i in 1:length(positions)){
      symbols[i]=positions[[i]]$getName()
    }
  set=portfolio_getSettings(portfolioTemp)
  .jcall(portfolioTemp@java,returnSig="V", method="setSamplingInterval","last")
  
  n=length(symbols)
  resultMatrix=matrix(1,nrow=n,ncol=n)
  .jcall(portfolioTemp@java,returnSig="V", method="createCallGroup",as.integer(n+((n*n-n)/2)))
  for(i in 2:n){
    for(j in 1:(i-1)){
      builder=.jnew("com.portfolioeffect.quant.client.util.LazyMetricBuilder",toJSONpe(list(metric="COVARIANCE")))
      result=new('metric',java=builder$build(positions[[i]],positions[[j]]))
      resultMatrix[i,j]=compute(result)[[1]][2]
      resultMatrix[j,i]=resultMatrix[i,j]
    }
  }
  for(i in 1:n){
    builder=.jnew("com.portfolioeffect.quant.client.util.LazyMetricBuilder",toJSONpe(list(metric="VARIANCE")))
    result=new('metric',java=builder$build(positions[[i]]))
    resultMatrix[i,i]=compute(result)[[1]][2]
  }
  dimnames(resultMatrix) = list(symbols,symbols)
  return(resultMatrix)})
#' @rdname covariance
#' @aliases covariance,position-method
setMethod("covariance",c("position","position"),function (positionA,positionB){
  builder=.jnew("com.portfolioeffect.quant.client.util.LazyMetricBuilder",toJSONpe(list(metric="COVARIANCE")))
  metric=new('metric',java=builder$build(positionA@java,positionB@java))
  return(metric)})
#' Method expected_shortfall
#' @name expected_shortfall
#' @rdname expected_shortfall
#' @exportMethod expected_shortfall
setGeneric("expected_shortfall", if(exists("expected_shortfall")){NULL}else{ function(asset,confidenceInterval)  standardGeneric("expected_shortfall")})
#' @rdname expected_shortfall
#' @aliases expected_shortfall,portfolio-method
setMethod("expected_shortfall",c("portfolio","ANY"),function (asset,confidenceInterval){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="CVAR",confidenceInterval=as.double(confidenceInterval))
  return(result)})
#' @rdname expected_shortfall
#' @aliases expected_shortfall,position-method
setMethod("expected_shortfall",c("position","ANY"),function(asset,confidenceInterval){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="CVAR",confidenceInterval=as.double(confidenceInterval))
  return(result)})
#' Method kurtosis
#' @name kurtosis
#' @rdname kurtosis
#' @exportMethod kurtosis
setGeneric("kurtosis", if(exists("kurtosis")){NULL}else{ function(asset)  standardGeneric("kurtosis")})
#' @rdname kurtosis
#' @aliases kurtosis,portfolio-method
setMethod("kurtosis","portfolio",function (asset){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="KURTOSIS")
  return(result)})
#' @rdname kurtosis
#' @aliases kurtosis,position-method
setMethod("kurtosis","position",function (asset){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="KURTOSIS")
  return(result)})
#' Method mod_sharpe_ratio
#' @name mod_sharpe_ratio
#' @rdname mod_sharpe_ratio
#' @exportMethod mod_sharpe_ratio
setGeneric("mod_sharpe_ratio", if(exists("mod_sharpe_ratio")){NULL}else{ function(asset,confidenceInterval)  standardGeneric("mod_sharpe_ratio")})
#' @rdname mod_sharpe_ratio
#' @aliases mod_sharpe_ratio,portfolio-method
setMethod("mod_sharpe_ratio",c("portfolio","ANY"),function(asset,confidenceInterval){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="SHARPE_RATIO_MOD",confidenceInterval=as.double(confidenceInterval))
  return(result)})
#' @rdname mod_sharpe_ratio
#' @aliases mod_sharpe_ratio,position-method
setMethod("mod_sharpe_ratio",c("position","ANY"),function(asset,confidenceInterval){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="SHARPE_RATIO_MOD",confidenceInterval=as.double(confidenceInterval))
  return(result)})
#' Method price
#' @name price
#' @rdname price
#' @exportMethod price
setGeneric("price", if(exists("price")){NULL}else{ function(asset)  standardGeneric("price")})
#' @rdname price
#' @aliases price,position-method
setMethod("price","position",function (asset){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="PRICE")
  return(result)})
#' Method profit
#' @name profit
#' @rdname profit
#' @exportMethod profit
setGeneric("profit", if(exists("profit")){NULL}else{ function(asset)  standardGeneric("profit")})
#' @rdname profit
#' @aliases profit,portfolio-method
setMethod("profit","portfolio",function (asset){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="PROFIT")
  return(result)})
#' @rdname profit
#' @aliases profit,position-method
setMethod("profit","position",function (asset){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="PROFIT")
  return(result)})
#' Method quantity
#' @name quantity
#' @rdname quantity
#' @exportMethod quantity
setGeneric("quantity", if(exists("quantity")){NULL}else{ function(asset)  standardGeneric("quantity")})
#' @rdname quantity
#' @aliases quantity,position-method
setMethod("quantity","position",function (asset){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="QUANTITY")
  return(result)})
#' Method log_return
#' @name log_return
#' @rdname log_return
#' @exportMethod log_return
setGeneric("log_return", if(exists("log_return")){NULL}else{ function(asset)  standardGeneric("log_return")})
#' @rdname log_return
#' @aliases log_return,portfolio-method
setMethod("log_return","portfolio",function (asset){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="RETURN")
  return(result)})
#' @rdname log_return
#' @aliases log_return,position-method
setMethod("log_return","position",function (asset){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="RETURN")
  return(result)})
#' Method return_autocovariance
#' @name return_autocovariance
#' @rdname return_autocovariance
#' @exportMethod return_autocovariance
setGeneric("return_autocovariance", if(exists("return_autocovariance")){NULL}else{ function(asset,lag)  standardGeneric("return_autocovariance")})
#' @rdname return_autocovariance
#' @aliases return_autocovariance,position-method
setMethod("return_autocovariance",c("position","ANY"),function(asset,lag=10){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="RETURN_AUTOCOVARIANCE",lag=lag)
  return(result)})
#' Method expected_return
#' @name expected_return
#' @rdname expected_return
#' @exportMethod expected_return
setGeneric("expected_return", if(exists("expected_return")){NULL}else{ function(asset)  standardGeneric("expected_return")})
#' @rdname expected_return
#' @aliases expected_return,portfolio-method
setMethod("expected_return","portfolio",function (asset){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="EXPECTED_RETURN")
  return(result)})
#' @rdname expected_return
#' @aliases expected_return,position-method
setMethod("expected_return","position",function (asset){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="EXPECTED_RETURN")
  return(result)})
#' Method sharpe_ratio
#' @name sharpe_ratio
#' @rdname sharpe_ratio
#' @exportMethod sharpe_ratio
setGeneric("sharpe_ratio", if(exists("sharpe_ratio")){NULL}else{ function(asset)  standardGeneric("sharpe_ratio")})
#' @rdname sharpe_ratio
#' @aliases sharpe_ratio,portfolio-method
setMethod("sharpe_ratio","portfolio",function (asset){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="SHARPE_RATIO")
  return(result)})
#' @rdname sharpe_ratio
#' @aliases sharpe_ratio,position-method
setMethod("sharpe_ratio","position",function (asset){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="SHARPE_RATIO")
  return(result)})
#' Method starr_ratio
#' @name starr_ratio
#' @rdname starr_ratio
#' @exportMethod starr_ratio
setGeneric("starr_ratio", if(exists("starr_ratio")){NULL}else{ function(asset,confidenceInterval)  standardGeneric("starr_ratio")})
#' @rdname starr_ratio
#' @aliases starr_ratio,portfolio-method
setMethod("starr_ratio",c("portfolio","ANY"),function(asset,confidenceInterval){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="STARR_RATIO",confidenceInterval=as.double(confidenceInterval))
  return(result)})
#' @rdname starr_ratio
#' @aliases starr_ratio,position-method
setMethod("starr_ratio",c("position","ANY"),function(asset,confidenceInterval){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="STARR_RATIO",confidenceInterval=as.double(confidenceInterval))
  return(result)})
#' Method skewness
#' @name skewness
#' @rdname skewness
#' @exportMethod skewness
setGeneric("skewness", if(exists("skewness")){NULL}else{ function(asset)  standardGeneric("skewness")})
#' @rdname skewness
#' @aliases skewness,portfolio-method
setMethod("skewness","portfolio",function (asset){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="SKEWNESS")
  return(result)})
#' @rdname skewness
#' @aliases skewness,position-method
setMethod("skewness","position",function (asset){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="SKEWNESS")
  return(result)})
#' Method treynor_ratio
#' @name treynor_ratio
#' @rdname treynor_ratio
#' @exportMethod treynor_ratio
setGeneric("treynor_ratio", if(exists("treynor_ratio")){NULL}else{ function(asset)  standardGeneric("treynor_ratio")})
#' @rdname treynor_ratio
#' @aliases treynor_ratio,portfolio-method
setMethod("treynor_ratio","portfolio",function (asset){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="TREYNOR_RATIO")
  return(result)})
#' @rdname treynor_ratio
#' @aliases treynor_ratio,position-method
setMethod("treynor_ratio","position",function (asset){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="TREYNOR_RATIO")
  return(result)})
#' Method value
#' @name value
#' @rdname value
#' @exportMethod value
setGeneric("value", if(exists("value")){NULL}else{ function(asset)  standardGeneric("value")})
#' @rdname value
#' @aliases value,portfolio-method
setMethod("value","portfolio",function (asset){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="VALUE")
  return(result)})
#' @rdname value
#' @aliases value,position-method
setMethod("value","position",function (asset){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="VALUE")
  return(result)})
#' Method value_at_risk
#' @name value_at_risk
#' @rdname value_at_risk
#' @exportMethod value_at_risk
setGeneric("value_at_risk", if(exists("value_at_risk")){NULL}else{ function(asset,confidenceInterval)  standardGeneric("value_at_risk")})
#' @rdname value_at_risk
#' @aliases value_at_risk,portfolio-method
setMethod("value_at_risk",c("portfolio","ANY"),function(asset,confidenceInterval){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="VAR",confidenceInterval=as.double(confidenceInterval))
  return(result)})
#' @rdname value_at_risk
#' @aliases value_at_risk,position-method
setMethod("value_at_risk",c("position","ANY"),function(asset,confidenceInterval){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="VAR",confidenceInterval=as.double(confidenceInterval))
  return(result)})
#' Method variance
#' @name variance
#' @rdname variance
#' @exportMethod variance
setGeneric("variance", if(exists("variance")){NULL}else{ function(asset)  standardGeneric("variance")})
#' @rdname variance
#' @aliases variance,portfolio-method
setMethod("variance","portfolio",function (asset){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="VARIANCE")
  return(result)})
#' @rdname variance
#' @aliases variance,position-method
setMethod("variance","position",function (asset){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="VARIANCE")
  return(result)})
#' Method weight
#' @name weight
#' @rdname weight
#' @exportMethod weight
setGeneric("weight", if(exists("weight")){NULL}else{ function(asset)  standardGeneric("weight")})
#' @rdname weight
#' @aliases weight,position-method
setMethod("weight","position",function (asset){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="WEIGHT")
  return(result)})
#' Method alpha_jensens
#' @name alpha_jensens
#' @rdname alpha_jensens
#' @exportMethod alpha_jensens
setGeneric("alpha_jensens", if(exists("alpha_jensens")){NULL}else{ function(asset)  standardGeneric("alpha_jensens")})
#' @rdname alpha_jensens
#' @aliases alpha_jensens,portfolio-method
setMethod("alpha_jensens","portfolio",function (asset){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="ALPHA_JENSEN")
  return(result)})
#' @rdname alpha_jensens
#' @aliases alpha_jensens,position-method
setMethod("alpha_jensens","position",function (asset){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="ALPHA_JENSEN")
  return(result)})
#' Method information_ratio
#' @name information_ratio
#' @rdname information_ratio
#' @exportMethod information_ratio
setGeneric("information_ratio", if(exists("information_ratio")){NULL}else{ function(asset)  standardGeneric("information_ratio")})
#' @rdname information_ratio
#' @aliases information_ratio,portfolio-method
setMethod("information_ratio","portfolio",function (asset){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="INFORMATION_RATIO")
  return(result)})
#' @rdname information_ratio
#' @aliases information_ratio,position-method
setMethod("information_ratio","position",function (asset){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="INFORMATION_RATIO")
  return(result)})
#' Method max_drawdown
#' @name max_drawdown
#' @rdname max_drawdown
#' @exportMethod max_drawdown
setGeneric("max_drawdown", if(exists("max_drawdown")){NULL}else{ function(asset)  standardGeneric("max_drawdown")})
#' @rdname max_drawdown
#' @aliases max_drawdown,portfolio-method
setMethod("max_drawdown","portfolio",function (asset){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="MAX_DRAWDOWN")
  return(result)})
#' @rdname max_drawdown
#' @aliases max_drawdown,position-method
setMethod("max_drawdown","position",function (asset){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="MAX_DRAWDOWN")
  return(result)})
#' Method calmar_ratio
#' @name calmar_ratio
#' @rdname calmar_ratio
#' @exportMethod calmar_ratio
setGeneric("calmar_ratio", if(exists("calmar_ratio")){NULL}else{ function(asset)  standardGeneric("calmar_ratio")})
#' @rdname calmar_ratio
#' @aliases calmar_ratio,portfolio-method
setMethod("calmar_ratio","portfolio",function (asset){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="CALMAR_RATIO")
  return(result)})
#' @rdname calmar_ratio
#' @aliases calmar_ratio,position-method
setMethod("calmar_ratio","position",function (asset){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="CALMAR_RATIO")
  return(result)})
#' Method omega_ratio
#' @name omega_ratio
#' @rdname omega_ratio
#' @exportMethod omega_ratio
setGeneric("omega_ratio", if(exists("omega_ratio")){NULL}else{ function(asset,thresholdReturn)  standardGeneric("omega_ratio")})
#' @rdname omega_ratio
#' @aliases omega_ratio,portfolio-method
setMethod("omega_ratio",c("portfolio","ANY"),function(asset,thresholdReturn){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="OMEGA_RATIO",thresholdReturn=as.double(thresholdReturn))
  return(result)})
#' @rdname omega_ratio
#' @aliases omega_ratio,position-method
setMethod("omega_ratio",c("position","ANY"),function(asset,thresholdReturn){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="OMEGA_RATIO",thresholdReturn=as.double(thresholdReturn))
  return(result)})
#' Method rachev_ratio
#' @name rachev_ratio
#' @rdname rachev_ratio
#' @exportMethod rachev_ratio
setGeneric("rachev_ratio", if(exists("rachev_ratio")){NULL}else{ function(asset,confidenceIntervalA=0.95,confidenceIntervalB=0.95)  standardGeneric("rachev_ratio")})
#' @rdname rachev_ratio
#' @aliases rachev_ratio,portfolio-method
setMethod("rachev_ratio",c("portfolio","ANY","ANY"),function(asset,confidenceIntervalA=0.95,confidenceIntervalB=0.95){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="RACHEV_RATIO",confidenceIntervalAlpha=as.double(confidenceIntervalA),confidenceIntervalBeta=as.double(confidenceIntervalB))
  return(result)})
#' @rdname rachev_ratio
#' @aliases rachev_ratio,position-method
setMethod("rachev_ratio",c("position","ANY","ANY"),function(asset,confidenceIntervalA=0.95,confidenceIntervalB=0.95){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="RACHEV_RATIO",confidenceIntervalAlpha=as.double(confidenceIntervalA),confidenceIntervalBeta=as.double(confidenceIntervalB))
  return(result)})
#' Method gain_variance
#' @name gain_variance
#' @rdname gain_variance
#' @exportMethod gain_variance
setGeneric("gain_variance", if(exists("gain_variance")){NULL}else{ function(asset)  standardGeneric("gain_variance")})
#' @rdname gain_variance
#' @aliases gain_variance,portfolio-method
setMethod("gain_variance","portfolio",function (asset){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="GAIN_VARIANCE")
  return(result)})
#' @rdname gain_variance
#' @aliases gain_variance,position-method
setMethod("gain_variance","position",function (asset){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="GAIN_VARIANCE")
  return(result)})
#' Method loss_variance
#' @name loss_variance
#' @rdname loss_variance
#' @exportMethod loss_variance
setGeneric("loss_variance", if(exists("loss_variance")){NULL}else{ function(asset)  standardGeneric("loss_variance")})
#' @rdname loss_variance
#' @aliases loss_variance,portfolio-method
setMethod("loss_variance","portfolio",function (asset){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="LOSS_VARIANCE")
  return(result)})
#' @rdname loss_variance
#' @aliases loss_variance,position-method
setMethod("loss_variance","position",function (asset){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="LOSS_VARIANCE")
  return(result)})
#' Method downside_variance
#' @name downside_variance
#' @rdname downside_variance
#' @exportMethod downside_variance
setGeneric("downside_variance", if(exists("downside_variance")){NULL}else{ function(asset,thresholdReturn)  standardGeneric("downside_variance")})
#' @rdname downside_variance
#' @aliases downside_variance,portfolio-method
setMethod("downside_variance",c("portfolio","ANY"),function(asset,thresholdReturn){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="DOWNSIDE_VARIANCE",thresholdReturn=as.double(thresholdReturn))
  return(result)})
#' @rdname downside_variance
#' @aliases downside_variance,position-method
setMethod("downside_variance",c("position","ANY"),function(asset,thresholdReturn){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="DOWNSIDE_VARIANCE",thresholdReturn=as.double(thresholdReturn))
  return(result)})
#' Method upside_variance
#' @name upside_variance
#' @rdname upside_variance
#' @exportMethod upside_variance
setGeneric("upside_variance", if(exists("upside_variance")){NULL}else{ function(asset,thresholdReturn)  standardGeneric("upside_variance")})
#' @rdname upside_variance
#' @aliases upside_variance,portfolio-method
setMethod("upside_variance",c("portfolio","ANY"),function(asset,thresholdReturn){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="UPSIDE_VARIANCE",thresholdReturn=as.double(thresholdReturn))
  return(result)})
#' @rdname upside_variance
#' @aliases upside_variance,position-method
setMethod("upside_variance",c("position","ANY"),function(asset,thresholdReturn){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="UPSIDE_VARIANCE",thresholdReturn=as.double(thresholdReturn))
  return(result)})
#' Method expected_downside_return
#' @name expected_downside_return
#' @rdname expected_downside_return
#' @exportMethod expected_downside_return
setGeneric("expected_downside_return", if(exists("expected_downside_return")){NULL}else{ function(asset,thresholdReturn)  standardGeneric("expected_downside_return")})
#' @rdname expected_downside_return
#' @aliases expected_downside_return,portfolio-method
setMethod("expected_downside_return",c("portfolio","ANY"),function(asset,thresholdReturn){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="EXPECTED_DOWNSIDE_THRESHOLD_RETURN",thresholdReturn=as.double(thresholdReturn))
  return(result)})
#' @rdname expected_downside_return
#' @aliases expected_downside_return,position-method
setMethod("expected_downside_return",c("position","ANY"),function(asset,thresholdReturn){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="EXPECTED_DOWNSIDE_THRESHOLD_RETURN",thresholdReturn=as.double(thresholdReturn))
  return(result)})
#' Method expected_upside_return
#' @name expected_upside_return
#' @rdname expected_upside_return
#' @exportMethod expected_upside_return
setGeneric("expected_upside_return", if(exists("expected_upside_return")){NULL}else{ function(asset,thresholdReturn)  standardGeneric("expected_upside_return")})
#' @rdname expected_upside_return
#' @aliases expected_upside_return,portfolio-method
setMethod("expected_upside_return",c("portfolio","ANY"),function(asset,thresholdReturn){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="EXPECTED_UPSIDE_THRESHOLD_RETURN",thresholdReturn=as.double(thresholdReturn))
  return(result)})
#' @rdname expected_upside_return
#' @aliases expected_upside_return,position-method
setMethod("expected_upside_return",c("position","ANY"),function(asset,thresholdReturn){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="EXPECTED_UPSIDE_THRESHOLD_RETURN",thresholdReturn=as.double(thresholdReturn))
  return(result)})
#' Method sortino_ratio
#' @name sortino_ratio
#' @rdname sortino_ratio
#' @exportMethod sortino_ratio
setGeneric("sortino_ratio", if(exists("sortino_ratio")){NULL}else{ function(asset,thresholdReturn)  standardGeneric("sortino_ratio")})
#' @rdname sortino_ratio
#' @aliases sortino_ratio,portfolio-method
setMethod("sortino_ratio",c("portfolio","ANY"),function(asset,thresholdReturn){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="SORTINO_RATIO",thresholdReturn=as.double(thresholdReturn))
  return(result)})
#' @rdname sortino_ratio
#' @aliases sortino_ratio,position-method
setMethod("sortino_ratio",c("position","ANY"),function(asset,thresholdReturn){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="SORTINO_RATIO",thresholdReturn=as.double(thresholdReturn))
  return(result)})
#' Method upside_downside_variance_ratio
#' @name upside_downside_variance_ratio
#' @rdname upside_downside_variance_ratio
#' @exportMethod upside_downside_variance_ratio
setGeneric("upside_downside_variance_ratio", if(exists("upside_downside_variance_ratio")){NULL}else{ function(asset,thresholdReturn)  standardGeneric("upside_downside_variance_ratio")})
#' @rdname upside_downside_variance_ratio
#' @aliases upside_downside_variance_ratio,portfolio-method
setMethod("upside_downside_variance_ratio",c("portfolio","ANY"),function(asset,thresholdReturn){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="UPSIDE_DOWNSIDE_VARIANCE_RATIO",thresholdReturn=as.double(thresholdReturn))
  return(result)})
#' @rdname upside_downside_variance_ratio
#' @aliases upside_downside_variance_ratio,position-method
setMethod("upside_downside_variance_ratio",c("position","ANY"),function(asset,thresholdReturn){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="UPSIDE_DOWNSIDE_VARIANCE_RATIO",thresholdReturn=as.double(thresholdReturn))
  return(result)})
#' Method gain_loss_variance_ratio
#' @name gain_loss_variance_ratio
#' @rdname gain_loss_variance_ratio
#' @exportMethod gain_loss_variance_ratio
setGeneric("gain_loss_variance_ratio", if(exists("gain_loss_variance_ratio")){NULL}else{ function(asset)  standardGeneric("gain_loss_variance_ratio")})
#' @rdname gain_loss_variance_ratio
#' @aliases gain_loss_variance_ratio,portfolio-method
setMethod("gain_loss_variance_ratio","portfolio",function (asset){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="GAIN_LOSS_VARIANCE_RATIO")
  return(result)})
#' @rdname gain_loss_variance_ratio
#' @aliases gain_loss_variance_ratio,position-method
setMethod("gain_loss_variance_ratio","position",function (asset){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="GAIN_LOSS_VARIANCE_RATIO")
  return(result)})
#' Method down_capture_ratio
#' @name down_capture_ratio
#' @rdname down_capture_ratio
#' @exportMethod down_capture_ratio
setGeneric("down_capture_ratio", if(exists("down_capture_ratio")){NULL}else{ function(asset)  standardGeneric("down_capture_ratio")})
#' @rdname down_capture_ratio
#' @aliases down_capture_ratio,portfolio-method
setMethod("down_capture_ratio","portfolio",function (asset){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="DOWN_CAPTURE_RATIO")
  return(result)})
#' @rdname down_capture_ratio
#' @aliases down_capture_ratio,position-method
setMethod("down_capture_ratio","position",function (asset){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="DOWN_CAPTURE_RATIO")
  return(result)})
#' Method up_capture_ratio
#' @name up_capture_ratio
#' @rdname up_capture_ratio
#' @exportMethod up_capture_ratio
setGeneric("up_capture_ratio", if(exists("up_capture_ratio")){NULL}else{ function(asset)  standardGeneric("up_capture_ratio")})
#' @rdname up_capture_ratio
#' @aliases up_capture_ratio,portfolio-method
setMethod("up_capture_ratio","portfolio",function (asset){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="UP_CAPTURE_RATIO")
  return(result)})
#' @rdname up_capture_ratio
#' @aliases up_capture_ratio,position-method
setMethod("up_capture_ratio","position",function (asset){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="UP_CAPTURE_RATIO")
  return(result)})
#' Method down_number_ratio
#' @name down_number_ratio
#' @rdname down_number_ratio
#' @exportMethod down_number_ratio
setGeneric("down_number_ratio", if(exists("down_number_ratio")){NULL}else{ function(asset)  standardGeneric("down_number_ratio")})
#' @rdname down_number_ratio
#' @aliases down_number_ratio,portfolio-method
setMethod("down_number_ratio","portfolio",function (asset){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="DOWN_NUMBER_RATIO")
  return(result)})
#' @rdname down_number_ratio
#' @aliases down_number_ratio,position-method
setMethod("down_number_ratio","position",function (asset){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="DOWN_NUMBER_RATIO")
  return(result)})
#' Method up_number_ratio
#' @name up_number_ratio
#' @rdname up_number_ratio
#' @exportMethod up_number_ratio
setGeneric("up_number_ratio", if(exists("up_number_ratio")){NULL}else{ function(asset)  standardGeneric("up_number_ratio")})
#' @rdname up_number_ratio
#' @aliases up_number_ratio,portfolio-method
setMethod("up_number_ratio","portfolio",function (asset){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="UP_NUMBER_RATIO")
  return(result)})
#' @rdname up_number_ratio
#' @aliases up_number_ratio,position-method
setMethod("up_number_ratio","position",function (asset){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="UP_NUMBER_RATIO")
  return(result)})
#' Method down_percentage_ratio
#' @name down_percentage_ratio
#' @rdname down_percentage_ratio
#' @exportMethod down_percentage_ratio
setGeneric("down_percentage_ratio", if(exists("down_percentage_ratio")){NULL}else{ function(asset)  standardGeneric("down_percentage_ratio")})
#' @rdname down_percentage_ratio
#' @aliases down_percentage_ratio,portfolio-method
setMethod("down_percentage_ratio","portfolio",function (asset){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="DOWN_PERCENTAGE_RATIO")
  return(result)})
#' @rdname down_percentage_ratio
#' @aliases down_percentage_ratio,position-method
setMethod("down_percentage_ratio","position",function (asset){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="DOWN_PERCENTAGE_RATIO")
  return(result)})
#' Method up_percentage_ratio
#' @name up_percentage_ratio
#' @rdname up_percentage_ratio
#' @exportMethod up_percentage_ratio
setGeneric("up_percentage_ratio", if(exists("up_percentage_ratio")){NULL}else{ function(asset)  standardGeneric("up_percentage_ratio")})
#' @rdname up_percentage_ratio
#' @aliases up_percentage_ratio,portfolio-method
setMethod("up_percentage_ratio","portfolio",function (asset){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="UP_PERCENTAGE_RATIO")
  return(result)})
#' @rdname up_percentage_ratio
#' @aliases up_percentage_ratio,position-method
setMethod("up_percentage_ratio","position",function (asset){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="UP_PERCENTAGE_RATIO")
  return(result)})
#' Method hurst_exponent
#' @name hurst_exponent
#' @rdname hurst_exponent
#' @exportMethod hurst_exponent
setGeneric("hurst_exponent", if(exists("hurst_exponent")){NULL}else{ function(asset)  standardGeneric("hurst_exponent")})
#' @rdname hurst_exponent
#' @aliases hurst_exponent,portfolio-method
setMethod("hurst_exponent","portfolio",function (asset){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="HURST_EXPONENT")
  return(result)})
#' @rdname hurst_exponent
#' @aliases hurst_exponent,position-method
setMethod("hurst_exponent","position",function (asset){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="HURST_EXPONENT")
  return(result)})
#' Method fractal_dimension
#' @name fractal_dimension
#' @rdname fractal_dimension
#' @exportMethod fractal_dimension
setGeneric("fractal_dimension", if(exists("fractal_dimension")){NULL}else{ function(asset)  standardGeneric("fractal_dimension")})
#' @rdname fractal_dimension
#' @aliases fractal_dimension,portfolio-method
setMethod("fractal_dimension","portfolio",function (asset){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="FRACTAL_DIMENSION")
  return(result)})
#' @rdname fractal_dimension
#' @aliases fractal_dimension,position-method
setMethod("fractal_dimension","position",function (asset){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="FRACTAL_DIMENSION")
  return(result)})
#' Method moment
#' @name moment
#' @rdname moment
#' @exportMethod moment
setGeneric("moment", if(exists("moment")){NULL}else{ function(asset, order)  standardGeneric("moment")})
#' @rdname moment
#' @aliases moment,portfolio-method
setMethod("moment",c("portfolio","ANY"),function (asset, order){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric=paste('MOMENT',order,sep=""))
  return(result)})
#' @rdname moment
#' @aliases moment,position-method
setMethod("moment",c("position","ANY"),function (asset, order){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric=paste('MOMENT',order,sep=""))
  return(result)})
#' Method cumulant
#' @name cumulant
#' @rdname cumulant
#' @exportMethod cumulant
setGeneric("cumulant", if(exists("cumulant")){NULL}else{ function(asset, order)  standardGeneric("cumulant")})
#' @rdname cumulant
#' @aliases cumulant,portfolio-method
setMethod("cumulant",c("portfolio","ANY"),function (asset, order){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric=paste('CUMULANT',order,sep=""))
  return(result)})
#' @rdname cumulant
#' @aliases cumulant,position-method
setMethod("cumulant",c("position","ANY"),function (asset, order){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric=paste('CUMULANT',order,sep=""))
  return(result)})
#' Method dist_density
#' @name dist_density
#' @rdname dist_density
#' @exportMethod dist_density
setGeneric("dist_density", if(exists("dist_density")){NULL}else{ function(asset,pValueLeft,pValueRight,nPoints,addNormalDensity)  standardGeneric("dist_density")})
#' @rdname dist_density
#' @aliases dist_density,portfolio-method
setMethod("dist_density",c("portfolio"),function (asset,pValueLeft,pValueRight,nPoints,addNormalDensity){
  portfolioTemp=portfolio_create(asset)
  set=portfolio_getSettings(portfolioTemp)
  .jcall(portfolioTemp@java,returnSig="V", method="setSamplingInterval","last")
  
  z=portfolioTemp@java$getPDF(as.double(pValueLeft),as.double(pValueRight),as.integer(nPoints))
  
  result=list(pdf=.jcall(z,returnSig="[[D",method="getDoubleMatrix", "pdf" ,simplify=TRUE),
               value=.jcall(z,returnSig="[[D",method="getDoubleMatrix", "x", simplify=TRUE),
               time=.jcall(z,returnSig="[J",method="getLongArray", "time"))
  
  if(addNormalDensity){
    GaussianMoments=FALSE
    if(set$densityModel!="NORMAL"){
      GaussianMoments=TRUE
      portfolio_settings(portfolioTemp,densityModel="NORMAL")
    }
    z=portfolioTemp@java$getPDF(as.double(pValueLeft),as.double(pValueRight),as.integer(nPoints))
    
    result$pdfNormal=.jcall(z,returnSig="[[D",method="getDoubleMatrix", "pdf", simplify=TRUE)
    result$valueNormal=.jcall(z,returnSig="[[D",method="getDoubleMatrix", "x", simplify=TRUE)
  }
  return(result)})
#' @rdname dist_density
#' @aliases dist_density,position-method
setMethod("dist_density",c("position"),function (asset,pValueLeft,pValueRight,nPoints,addNormalDensity){
  portfolioTemp=portfolio_create(new("portfolio", java=asset@java$getPortfolio(),optimization_info=NULL))
#   positions=portfolioTemp@java$getPositions()
#   for(i in 1:length(positions)){
#     if(positions[[i]]$getName()==asset@java$getName()){
#       positionTemp=positions[[i]]
#     }
#   }
  set=portfolio_getSettings(portfolioTemp)
  .jcall(portfolioTemp@java,returnSig="V", method="setSamplingInterval","last")
  
  z=portfolioTemp@java$getPDF(as.double(pValueLeft),as.double(pValueRight),as.integer(nPoints),asset@java$getName())
  
  result=list(pdf=.jcall(z,returnSig="[[D",method="getDoubleMatrix", "pdf" ,simplify=TRUE),
               value=.jcall(z,returnSig="[[D",method="getDoubleMatrix", "x", simplify=TRUE),
               time=.jcall(z,returnSig="[J",method="getLongArray", "time"))
  
  if(addNormalDensity){
    GaussianMoments=FALSE
    if(set$densityModel!="NORMAL"){
      GaussianMoments=TRUE
      portfolio_settings(portfolioTemp,densityModel="NORMAL")
    }
    z=portfolioTemp@java$getPDF(as.double(pValueLeft),as.double(pValueRight),as.integer(nPoints),asset@java$getName())
    
    result$pdfNormal=.jcall(z,returnSig="[[D",method="getDoubleMatrix", "pdf", simplify=TRUE)
    result$valueNormal=.jcall(z,returnSig="[[D",method="getDoubleMatrix", "x", simplify=TRUE)
  }
  return(result)})
#' Method return_jump_size
#' @name return_jump_size
#' @rdname return_jump_size
#' @exportMethod return_jump_size
setGeneric("return_jump_size", if(exists("return_jump_size")){NULL}else{ function(asset)  standardGeneric("return_jump_size")})
#' @rdname return_jump_size
#' @aliases return_jump_size,position-method
setMethod("return_jump_size","position",function (asset){
  portfolioTemp=portfolio_create(new("portfolio", java=asset@java$getPortfolio(),optimization_info=NULL))
  positions=portfolioTemp@java$getPositions()
  symbols=portfolioTemp@java$getSymbols()
  num=(1:length(positions))[asset@java$getName()==symbols]
  positionTemp= new("position", java=positions[[num]])
  portfolio_settings(portfolioTemp,jumpsModel='none')
  time=compute(price(positionTemp))[[1]][,1]
  priceNoJumpsFilter=compute(price(positionTemp))[[1]][,2]
  portfolio_settings(portfolioTemp,jumpsModel='all')
  priceJumpsFilter=compute(price(positionTemp))[[1]][,2]
  jumps=log(priceNoJumpsFilter)-log(priceJumpsFilter)
  jump_size=cbind(time,jumps)
  result=create_metric(jump_size,asset@java$getName())
  return(result)})
#' Method txn_costs
#' @name txn_costs
#' @rdname txn_costs
#' @exportMethod txn_costs
setGeneric("txn_costs", if(exists("txn_costs")){NULL}else{ function(asset)  standardGeneric("txn_costs")})
#' @rdname txn_costs
#' @aliases txn_costs,portfolio-method
setMethod("txn_costs","portfolio",function (asset){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="TRANSACTION_COSTS_SIZE")
  return(result)})
#' @rdname txn_costs
#' @aliases txn_costs,position-method
setMethod("txn_costs","position",function (asset){
  result=position_metric(argList=as.list(environment()),portfolio=asset,metric="TRANSACTION_COSTS_SIZE")
  return(result)})

setGeneric("position_remove", if(exists("position_remove")){NULL}else{ function(asset,symbol)  standardGeneric("position_remove")})
setMethod("position_remove",c(asset="portfolio",symbol="character"),function (asset,symbol){
  .jcall(asset@java,returnSig="V", method="removePositionQuantity",symbol)
  .jcall(asset@java,returnSig="V", method="removePositionPrice",symbol)
  })
setMethod("position_remove",c(asset="position",symbol="missing"),function (asset){
  .jcall(asset@java,returnSig="V", method="removePositionQuantity")
  .jcall(asset@java,returnSig="V", method="removePositionPrice")
  })

setGeneric("set_quantity", if(exists("set_quantity")){NULL}else{ function(asset,quantity)  standardGeneric("set_quantity")})
# setMethod("set_quantity",c(asset="portfolio",symbol="character",quantity="ANY"),function (asset,symbol,quantity){
#   result=asset@java$setPositionQuantity(symbol,as.integer(quantity))  
#   util_checkErrors(result)
# })
setMethod("set_quantity",c(asset="position",quantity="numeric"),function (asset,quantity){
  asset@java$setPositionQuantity(as.integer(quantity))
})
setMethod("set_quantity",c(asset="position",quantity="integer"),function (asset,quantity){
  asset@java$setPositionQuantity(as.integer(quantity))
})

weight_transform=function(portfolio,transformType=c('sum_abs_weight','equiweight'),symbols=NULL){
  symbols=c(symbols)
  switch(transformType[1],sum_abs_weight={
    builder=.jnew("com.portfolioeffect.quant.client.util.LazyMetricBuilder",toJSONpe(list(metric="POSITIONS_SUM_ABS_WEIGHT")))
    if(is.null(symbols)){
      symbols=portfolio@java$getPositions()
      for(i in 1:length(symbols)){
        builder$addToList("positions", symbols[[i]])
      }
    }else{
    for(i in 1:length(symbols)){
      builder$addToList("positions", symbols[[i]]@java)
    }}
    temp=builder$build(portfolio@java)
    result=new("metric", java=temp)
    },
    equiweight={
      result=position_metric(argList=as.list(environment()),portfolio=portfolio,metric="EQUIWEIGHT")
    }
  )
return(result)
}
