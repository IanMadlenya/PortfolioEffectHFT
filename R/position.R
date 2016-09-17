#' An S4 class to represent a bank account.
#'
#' @slot java A length-one numeric vector
#' @slot symbol A length-one numeric vector
setClass("position",
         slots = c(java="jobjRef",symbol="character",portfolio="jobjRef")
)

position_add<-function(portfolio,symbol,quantity,time,priceData){
}

setMethod("position_add" ,c(portfolio="portfolio",symbol="character",quantity="ANY",time="missing",priceData="missing"),function(
  portfolio,symbol,quantity){
  result<-.jnew("com.portfolioeffect.quant.client.portfolio.Position",portfolio@java,symbol, as.integer(quantity))
  # result<-.jcall(portfolio@java,returnSig="Lcom/portfolioeffect/quant/client/result/Metric;", method="addPosition",symbol, as.integer(quantity))
  # util_checkErrors(result)
  position=new("position", java=result,symbol=symbol,portfolio=portfolio@java)
  return(position)
})

setMethod("position_add" ,c(portfolio="portfolio",symbol="character",quantity="ANY",time="missing",priceData="matrix"),function(
  portfolio,symbol,quantity,priceData){
  result<-.jnew("com.portfolioeffect.quant.client.portfolio.Position",portfolio@java,symbol, as.double(priceData[,2]), as.integer(quantity),.jlong(priceData[,1]))
#   result<-.jcall(portfolio@java,returnSig="Lcom/portfolioeffect/quant/client/result/Metric;", method="addPosition",symbol, as.double(priceData[,2]), as.integer(quantity),.jlong(priceData[,1]))
#   util_checkErrors(result)
  position=new("position", java=result,symbol=symbol,portfolio=portfolio@java)
  return(position)
})

setMethod("position_add" ,c(portfolio="portfolio",symbol="character",quantity="ANY",time="ANY",priceData="missing"),function(
  portfolio,symbol,quantity,time){
  if(!is.numeric(time)){
    time<-util_dateToPOSIXTime(time)
  }
  result<-.jnew("com.portfolioeffect.quant.client.portfolio.Position",portfolio@java,symbol, as.integer(quantity),.jlong(time))
  
#   result<-.jcall(portfolio@java,returnSig="Lcom/portfolioeffect/quant/client/result/Metric;", method="addPosition",symbol, as.integer(quantity),.jlong(time))
#   util_checkErrors(result)
  position=new("position", java=result,symbol=symbol,portfolio=portfolio@java)
  return(position)
})

setMethod("position_add" ,c(portfolio="portfolio",symbol="character",quantity="ANY",time="ANY",priceData="matrix"),function(
  portfolio,symbol,quantity,time,priceData){
  if(!is.numeric(time)){
    time<-util_dateToPOSIXTime(time)
  }
  result<-.jnew("com.portfolioeffect.quant.client.portfolio.Position",portfolio@java,symbol, as.double(priceData[,2]),.jlong(priceData[,1]), as.integer(quantity),.jlong(time))
  
#   result<-.jcall(portfolio@java,returnSig="Lcom/portfolioeffect/quant/client/result/......;", method="addPosition",symbol, as.double(priceData[,2]),.jlong(priceData[,1]), as.integer(quantity),.jlong(time))
  # util_checkErrors(result)
  position=new("position", java=result,symbol=symbol,portfolio=portfolio@java)
  return(position)
})

setMethod ("show" , "position" ,
           function (object){
             util_validate()
             portfolio=portfolio_create(new("portfolio", java=object@portfolio,optimization_info=NULL))
             
             symbols<-position_list(portfolio)
             
             .jcall(portfolio@java,returnSig="V", method="setParam","resultsSamplingInterval","last")
             
             position=portfolio_getPosition(portfolio,object@symbol)
             
             .jcall(portfolio@java,returnSig="V", method="createCallGroup",as.integer(6))
             
             printMatrix1<-matrix(0,ncol=6,nrow=1)
             
               printMatrix1[,1]<-compute(position_metric(argList=NULL,portfolio=position,metric='QUANTITY'))[[1]][-1]
               printMatrix1[,2]<-round(compute(position_metric(argList=NULL,portfolio=position,metric='WEIGHT'))[[1]][-1]*100,digits =3)
               printMatrix1[,3]<-round(compute(position_metric(argList=NULL,portfolio=position,metric='PROFIT'))[[1]][-1],digits =3)
               printMatrix1[,4]<-round(compute(position_metric(argList=NULL,portfolio=position,metric='RETURN'))[[1]][-1],digits =3)*100
               printMatrix1[,5]<-compute(position_metric(argList=NULL,portfolio=position,metric='VALUE'))[[1]][-1]
               printMatrix1[,6]<-compute(position_metric(argList=NULL,portfolio=position,metric='PRICE'))[[1]][-1]

             dimnames(printMatrix1) = list( object@symbol, c("  Quantity","  Weight (in %)","  Profit","  Return (in %)","  Value","  Price"))

             cat (paste("\n\n","POSITION SUMMARY","\n",sep="    "))
             print(printMatrix1)
           })


util_summaryPositionPlot<-function (x,y){	
  util_summaryPosition(x)
}
#util_summaryPlotBW<-function (x,y){	
#	util_summary(x,bw=y)
#}
setMethod("plot" ,c(x="position",y="missing"),util_summaryPositionPlot)
