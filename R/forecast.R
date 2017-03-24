setClass("forecast",
         slots = c(java="jobjRef")
)

forecast_builder=function(asset,model=c("EWMA","HAR"), window="20d", step = "1d", transform = c("log","none"), seasonalityInterval="none",updateInterval="1m",valueType="forecast") {  
  forecastBuilderM<-.jnew("com.portfolioeffect.quant.client.util.LinearForecastBuilder")
  forecastBuilderM$setTransform(transform[1]);
  forecastBuilderM$setRollingWindow(window);
  forecastBuilderM$setRegressionUpdateInterval(updateInterval);
  forecastBuilderM$setForecastStep(step);
  forecastBuilderM$setValueType(valueType);
  forecastBuilderM$setTimeShiftEnable(valueType=="forecast");
  forecastBuilderM$setDependentVariable(asset@java);
  forecastBuilderM$setForecastModel(switch(model[1],
                                          HAR="[{\"windowLength\":\"1d\"},{\"windowLength\":\"5d\"},{\"windowLength\":\"21d\"}]",
                                          EWMA="[]"));
  if(seasonalityInterval!="none"){
    forecastBuilderM$setSeasonInterval(seasonalityInterval);
  }
  forecastBuilderM = new("forecast",java=forecastBuilderM);
  return(forecastBuilderM)
}

forecast_input= function(forecast, metric) {
  forecast@java$addIndependentVariable(metric@java) 
  return(forecast)
}

forecast_apply = function(forecast) {
  util_validate()
  forecast_lazy_metric = new("metric",java=forecast@java$build())

  return(forecast_lazy_metric)
}

