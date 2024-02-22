#######################################################################
###################### METRICAS DE AVALIACAO ##########################

RMSE_f = function(yt,ft){
  RMSE = round(sqrt(mean((yt-ft)^2)),digits = 4)
  return(RMSE)
}

MAE_f = function(yt,ft){
  MAE = round(mean(abs(yt-ft)),digits = 4)
  return(MAE)
}

MAPE_f = function(yt,ft){
  MAPE = round(mean((abs(yt-ft)/max(yt))),digits = 4)
  return(MAPE)
}

R_squared_f = function(yt,ft){
  rss <- sum((yt - ft)^2)  ## residual sum of squares
  tss <- sum((yt - mean(yt))^2)  ## total sum of squares
  rsq <- round((1 - (rss/tss)),digits = 4)
  return(rsq)
}

R_squared_f2 = function(yt,ft){
  a = cor(yt,ft)
  rsq <- round(a^2,digits = 4)
  return(rsq)
}



