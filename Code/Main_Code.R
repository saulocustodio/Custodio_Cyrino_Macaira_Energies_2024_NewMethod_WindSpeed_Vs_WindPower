#######################################################################################
# Main Code
#######################################################################################
Num_metrics = 5

save_local = "C:/..." #location where you will save the files

#Choose the Wind Farm:
W_Farm = 1
#1 (Wind Farm 1)
#2 (Wind Farm 2)
#3 (Wind Farm 3)
#4 (Wind Farm 4)
#5 (Wind Farm 5)

# Period:
if((W_Farm == 1 )|(W_Farm == 2) ){
  Begin = "2017-11-01"
  End    = "2022-02-28"
}else{
  Begin = "2013-01-01"
  End    = "2022-02-28"
}

#location on the computer where the other codes are located
source("C:/ ...  .../Libraries.R")
source("C:/ ...  .../Methodology_TheoreticalCurve.R")
source("C:/ ...  .../Metrics.R")
source("C:/ ...  .../Graphics.R")
source("C:/ ...  .../Methodologies_Proposed.R")

Registration_W_Farm = data.frame(ind = 1:5,
  Name_WFarm=c("Wind Farm 1","Wind Farm 2","Wind Farm 3","Wind Farm 4","Wind Farm 5"),
  Capacity= c(31500,63000,10200,48000,4500),
  n_tur= c(15,30,13,60,3),
  Capacity_tur= c(2100,2100,800,800,1500),
  Tab_Generation = 1:5,
  radius = c(57,57,24,24,38.5),
  air_density = c(1.16,1.16,1.16,1.16,1.16),
  efficiency_factor = c(0.3547,0.3547,0.2724,0.2778,0.2528),
  cut_in = c(3.5,3.5,3,3,3), 
  cut_off = c(25,25,25,25,22),
  cut_rated = c(10,10,14,14,13)
)

if(W_Farm == 1){
  Table_Metrics_Totals = data.frame(
    Metrics = c(rep("RMSE",10),rep("MAE",10),rep("MAPE",10),rep("R²",10)), 
    WindFarm = rep(c(1,1,2,2,3,3,4,4,5,5),4),
    SpeedTreatment = rep(c("EXT","INMET","EXT","INMET","EXT","INMET","EXT","INMET","EXT","INMET"),4),
    Cubic = NA,
    Singular = NA,
    Monthly = NA,
    Hourly = NA,
    Monthly_Hourly = NA
  )
  
  Table_Metrics_Month = data.frame(
    Metrics = c(rep("RMSE",50),rep("MAE",50),rep("MAPE",50),rep("R²",50)), 
    WindFarm = rep(c(rep(1,10), rep(2,10), rep(3,10), rep(4,10), rep(5,10)),4),
    Methods = rep(c("Cubic","Cubic", "Singular","Singular", "Monthly","Monthly", "Hourly","Hourly", "Monthly_Hourly","Monthly_Hourly"),20),
    SpeedTreatment = rep(c("EXT","INMET"),100),
    Jan=NA,
    Feb=NA,
    Mar=NA,
    Apr=NA,
    May=NA,
    Jun=NA,
    Jul=NA,
    Aug=NA,
    Set=NA,
    Oct=NA,
    Nov=NA,
    Dec=NA
  )
  
  Table_Metrics_Hour = data.frame(
    Metrics = c(rep("RMSE",50),rep("MAE",50),rep("MAPE",50),rep("R²",50)), 
    WindFarm = rep(c(rep(1,10), rep(2,10), rep(3,10), rep(4,10), rep(5,10)),4),
    Methods = rep(c("Cubic","Cubic", "Singular","Singular", "Monthly","Monthly", "Hourly","Hourly", "Monthly_Hourly","Monthly_Hourly"),20),
    SpeedTreatment = rep(c("EXT","INMET"),100),
    Hour01=NA, Hour02=NA, Hour03=NA, Hour04=NA, Hour05=NA, Hour06=NA,
    Hour07=NA, Hour08=NA, Hour09=NA, Hour10=NA, Hour11=NA, Hour12=NA,
    Hour13=NA, Hour14=NA, Hour15=NA, Hour16=NA, Hour17=NA, Hour18=NA,
    Hour19=NA, Hour20=NA, Hour21=NA, Hour22=NA, Hour23=NA, Hour24=NA
  )
  
  Num_clusters = data.frame(
    WindFarm = c(rep(1,2), rep(2,2), rep(3,2), rep(4,2), rep(5,2)),
    SpeedTreatment = rep(c("EXT","INMET"),5),
    Singular = NA,
    Monthly_Min = NA,
    Monthly_Max = NA,
    Hourly_Min = NA,
    Hourly_Max = NA,
    Monthly_Hourly_Min = NA,
    Monthly_Hourly_Max = NA
  )
}

errors_EXT_Power_Curve = matrix(NA, ncol = 24, nrow = 2*Num_metrics, dimnames = list(c("RMSE","MAE","MAPE","R2","R2(cor2)",
                                                                                       "RMSE_H","MAE_H","MAPE_H","R2_H","R2(cor2)_H"), c(1:24)))
errors_INMET_Power_Curve = errors_EXT_Power_Curve

errors_EXT_KDE_SinglePeriod = matrix(NA, ncol = 24, nrow = 4*Num_metrics, dimnames = list(c("RMSE","MAE","MAPE","R2","R2(cor2)",
                                                                                     "RMSE_H","MAE_H","MAPE_H","R2_H","R2(cor2)_H",
                                                                                     "C1_RMSE","C1_MAE","C1_MAPE","C1_R2","C1_R2(cor2)",
                                                                                     "C1_RMSE_H","C1_MAE_H","C1_MAPE_H","C1_R2_H","C1_R2(cor2)_H"), c(1:24)))
errors_INMET_KDE_SinglePeriod = errors_EXT_KDE_SinglePeriod

errors_EXT_KDE_Monthly = errors_EXT_KDE_SinglePeriod
errors_INMET_KDE_Monthly = errors_EXT_KDE_SinglePeriod

errors_EXT_KDE_Hourly = errors_EXT_KDE_SinglePeriod
errors_INMET_KDE_Hourly = errors_EXT_KDE_SinglePeriod

errors_EXT_KDE_MonthHourly = errors_EXT_KDE_SinglePeriod
errors_INMET_KDE_MonthHourly = errors_EXT_KDE_SinglePeriod

errors_EXT_Totals = matrix(NA, ncol = 9, nrow = Num_metrics, 
                           dimnames = list(c("RMSE","MAE","MAPE","R2","R2(cor2)"), c("Power Curve","Proposed - All",
                                                                                   "Proposed - All [Cen1]",
                                                                                   "Proposed - By month",
                                                                                   "Proposed - By month [Cen1]",
                                                                                   "Proposed - By hour",
                                                                                   "Proposed - By hour [Cen1]",
                                                                                   "Proposed - By month and hour",
                                                                                   "Proposed - By month and hour [Cen1]")))
errors_INMET_Totals = errors_EXT_Totals

#Location of the computer where speed and power data from wind farms are stored
setwd("C:/... ...")
power_base = read_excel("... .xlsx", col_names = TRUE, sheet = Registration_W_Farm$Tab_Generation[W_Farm])
power_base$mes = month(power_base$Data)

Begin_pow = as.POSIXct(Begin, tz="UTC")
End_pow    = as.POSIXct(End, tz="UTC")
power = power_base[min(which(power_base$Data == Begin_pow)):max(which(power_base$Data == End_pow)),]

Base_Veloc = function(arquivo, i, f){
  setwd("C:/... .../")
  a = read_excel (arquivo, col_names = TRUE, sheet = 1)
  Dados_vel_base = data.frame(tempo=rep(NA,nrow(a)), velocidade=NA)
  Dados_vel_base$tempo = a[[1]]
  Dados_vel_base$velocidade = a[[2]]
  Dados_vel_base$hora = hour(Dados_vel_base$tempo)+1
  Dados_vel_base$mes = month(Dados_vel_base$tempo)
  
  Begin_Vel = as.POSIXct(  paste0(i,"00:00:00") ,tz="UTC")
  End_vel    = as.POSIXct(  paste0(f,"23:00:00") ,tz="UTC")
  dados = Dados_vel_base[which(Dados_vel_base$tempo == Begin_Vel):which(Dados_vel_base$tempo == End_vel),]
  
  return(dados)
}


###############################################################################
#Wind Farm 1
###############################################################################
if(W_Farm == 1){
  Dados_vel = Base_Veloc("...  .xlsx", Begin, End)
  Dados_vel3 = Base_Veloc("...  .xlsx", Begin, End)
}
###############################################################################
#Wind Farm 2
###############################################################################
if(W_Farm == 2){
  Dados_vel = Base_Veloc("...  .xlsx", Begin, End)
  Dados_vel3 = Base_Veloc("...  .xlsx", Begin, End)
}
###############################################################################
#Wind Farm 3
###############################################################################
if(W_Farm == 3){
  Dados_vel = Base_Veloc("...  .xlsx", Begin, End)
  Dados_vel3 = Base_Veloc("...  .xlsx", Begin, End)
}
###############################################################################
#Wind Farm 4
###############################################################################
if(W_Farm == 4){
  Dados_vel = Base_Veloc("...  .xlsx", Begin, End)
  Dados_vel3 = Base_Veloc("...  .xlsx", Begin, End)
}
###############################################################################
#Wind Farm 5
###############################################################################
if(W_Farm == 5){
  Dados_vel = Base_Veloc("...  .xlsx", Begin, End)
  Dados_vel3 = Base_Veloc("...  .xlsx", Begin, End)
}
###############################################################################
#Creating folder for local storage

setwd(save_local)
dir = paste0(save_local,Registration_W_Farm$Name_WFarm[W_Farm]," Vpu/")
dir.create(file.path(dir), showWarnings = TRUE)
dir_EXT = paste0(dir,"EXT")
dir.create(file.path(dir_EXT), showWarnings = TRUE)
dir_INMET = paste0(dir,"INMET")
dir.create(file.path(dir_INMET), showWarnings = TRUE)

d_c_EXT = paste0(dir_EXT,"/PowerCurve")
dir.create(file.path(d_c_EXT), showWarnings = TRUE)
d_c_INMET = paste0(dir_INMET,"/PowerCurve")
dir.create(file.path(d_c_INMET), showWarnings = TRUE)

d_u_EXT = paste0(dir_EXT,"/SinglePeriod")
dir.create(file.path(d_u_EXT), showWarnings = TRUE)
d_u_INMET = paste0(dir_INMET,"/SinglePeriod")
dir.create(file.path(d_u_INMET), showWarnings = TRUE)

d_m_EXT = paste0(dir_EXT,"/Monthly")
dir.create(file.path(d_m_EXT), showWarnings = TRUE)
d_m_INMET = paste0(dir_INMET,"/Monthly")
dir.create(file.path(d_m_INMET), showWarnings = TRUE)

d_h_EXT = paste0(dir_EXT,"/Hourly")
dir.create(file.path(d_h_EXT), showWarnings = TRUE)
d_h_INMET = paste0(dir_INMET,"/Hourly")
dir.create(file.path(d_h_INMET), showWarnings = TRUE)

d_mh_EXT = paste0(dir_EXT,"/MonthHourly")
dir.create(file.path(d_mh_EXT), showWarnings = TRUE)
d_mh_INMET = paste0(dir_INMET,"/MonthHourly")
dir.create(file.path(d_mh_INMET), showWarnings = TRUE)

###############################################################################
#Theoretical Power Curve

setwd(d_c_EXT)
metrics = Estimacao_Resultados("Power Curve","EXT", power, Dados_vel, Registration_W_Farm, W_Farm, Num_metrics) 
errors_EXT_Totals[,1] = metrics[1:Num_metrics,1]
errors_EXT_Power_Curve[,] = metrics[(Num_metrics+1):(3*Num_metrics),]

setwd(d_c_INMET)
metrics = Estimacao_Resultados("Power Curve","INMET", power, Dados_vel3, Registration_W_Farm, W_Farm, Num_metrics) 
errors_INMET_Totals[,1] = metrics[1:Num_metrics,1]
errors_INMET_Power_Curve[,] = metrics[(Num_metrics+1):(3*Num_metrics),]

###################################################################################
#SinglePeriod Methodology

setwd(d_u_EXT)
metrics = Estimacao_Resultados("Single period","EXT", power, Dados_vel, Registration_W_Farm, W_Farm, Num_metrics) 
errors_EXT_Totals[,2] = metrics[1:Num_metrics,1]
errors_EXT_KDE_SinglePeriod[1:(2*Num_metrics),] = metrics[(Num_metrics+1):(3*Num_metrics),]
errors_EXT_Totals[,3] = metrics[((3*Num_metrics)+1):(4*Num_metrics),1]
errors_EXT_KDE_SinglePeriod[((2*Num_metrics)+1):(4*Num_metrics),] = metrics[((4*Num_metrics)+1):(6*Num_metrics),]
Num_clusters[(2*(W_Farm-1)+1),3] = metrics[((6*Num_metrics)+1),1]

setwd(d_u_INMET)
metrics = Estimacao_Resultados("Single period","INMET", power, Dados_vel3, Registration_W_Farm, W_Farm, Num_metrics) 
errors_INMET_Totals[,2] = metrics[1:Num_metrics,1]
errors_INMET_KDE_SinglePeriod[1:(2*Num_metrics),] = metrics[(Num_metrics+1):(3*Num_metrics),]
errors_INMET_Totals[,3] = metrics[((3*Num_metrics)+1):(4*Num_metrics),1]
errors_INMET_KDE_SinglePeriod[((2*Num_metrics)+1):(4*Num_metrics),] = metrics[((4*Num_metrics)+1):(6*Num_metrics),]
Num_clusters[(2*(W_Farm-1)+2),3] = metrics[((6*Num_metrics)+1),1]

###################################################################################
#Monthly Methodology

setwd(d_m_EXT)
metrics = Estimacao_Resultados("Monthly","EXT", power, Dados_vel, Registration_W_Farm, W_Farm, Num_metrics) 
errors_EXT_Totals[,4] = metrics[1:Num_metrics,1]
errors_EXT_KDE_Monthly[1:(2*Num_metrics),] = metrics[(Num_metrics+1):(3*Num_metrics),]
errors_EXT_Totals[,5] = metrics[((3*Num_metrics)+1):(4*Num_metrics),1]
errors_EXT_KDE_Monthly[((2*Num_metrics)+1):(4*Num_metrics),] = metrics[((4*Num_metrics)+1):(6*Num_metrics),]
Num_clusters[(2*(W_Farm-1)+1),4] = metrics[((6*Num_metrics)+1),1]
Num_clusters[(2*(W_Farm-1)+1),5] = metrics[((6*Num_metrics)+1),2]

setwd(d_m_INMET)
metrics = Estimacao_Resultados("Monthly","INMET", power, Dados_vel3, Registration_W_Farm, W_Farm, Num_metrics) 
errors_INMET_Totals[,4] = metrics[1:Num_metrics,1]
errors_INMET_KDE_Monthly[1:(2*Num_metrics),] = metrics[(Num_metrics+1):(3*Num_metrics),]
errors_INMET_Totals[,5] = metrics[((3*Num_metrics)+1):(4*Num_metrics),1]
errors_INMET_KDE_Monthly[((2*Num_metrics)+1):(4*Num_metrics),] = metrics[((4*Num_metrics)+1):(6*Num_metrics),]
Num_clusters[(2*(W_Farm-1)+2),4] = metrics[((6*Num_metrics)+1),1]
Num_clusters[(2*(W_Farm-1)+2),5] = metrics[((6*Num_metrics)+1),2]

###################################################################################
#Hourly Methodology

setwd(d_h_EXT)
metrics = Estimacao_Resultados("Hourly","EXT", power, Dados_vel, Registration_W_Farm, W_Farm, Num_metrics) 
errors_EXT_Totals[,6] = metrics[1:Num_metrics,1]
errors_EXT_KDE_Hourly[1:(2*Num_metrics),] = metrics[(Num_metrics+1):(3*Num_metrics),]
errors_EXT_Totals[,7] = metrics[((3*Num_metrics)+1):(4*Num_metrics),1]
errors_EXT_KDE_Hourly[((2*Num_metrics)+1):(4*Num_metrics),] = metrics[((4*Num_metrics)+1):(6*Num_metrics),]
Num_clusters[(2*(W_Farm-1)+1),6] = metrics[((6*Num_metrics)+1),1]
Num_clusters[(2*(W_Farm-1)+1),7] = metrics[((6*Num_metrics)+1),2]

setwd(d_h_INMET)
metrics = Estimacao_Resultados("Hourly","INMET", power, Dados_vel3, Registration_W_Farm, W_Farm, Num_metrics) 
errors_INMET_Totals[,6] = metrics[1:Num_metrics,1]
errors_INMET_KDE_Hourly[1:(2*Num_metrics),] = metrics[(Num_metrics+1):(3*Num_metrics),]
errors_INMET_Totals[,7] = metrics[((3*Num_metrics)+1):(4*Num_metrics),1]
errors_INMET_KDE_Hourly[((2*Num_metrics)+1):(4*Num_metrics),] = metrics[((4*Num_metrics)+1):(6*Num_metrics),]
Num_clusters[(2*(W_Farm-1)+2),6] = metrics[((6*Num_metrics)+1),1]
Num_clusters[(2*(W_Farm-1)+2),7] = metrics[((6*Num_metrics)+1),2]

###################################################################################
#Monthly-Hourly Methodology

setwd(d_mh_EXT)
metrics = Estimacao_Resultados("Month-Hourly","EXT", power, Dados_vel, Registration_W_Farm, W_Farm, Num_metrics) 
errors_EXT_Totals[,8] = metrics[1:Num_metrics,1]
errors_EXT_KDE_MonthHourly[1:(2*Num_metrics),] = metrics[(Num_metrics+1):(3*Num_metrics),]
errors_EXT_Totals[,9] = metrics[((3*Num_metrics)+1):(4*Num_metrics),1]
errors_EXT_KDE_MonthHourly[((2*Num_metrics)+1):(4*Num_metrics),] = metrics[((4*Num_metrics)+1):(6*Num_metrics),]
Num_clusters[(2*(W_Farm-1)+1),8] = metrics[((6*Num_metrics)+1),1]
Num_clusters[(2*(W_Farm-1)+1),9] = metrics[((6*Num_metrics)+1),2]

setwd(d_mh_INMET)
metrics = Estimacao_Resultados("Month-Hourly","INMET", power, Dados_vel3, Registration_W_Farm, W_Farm, Num_metrics) 
errors_INMET_Totals[,8] = metrics[1:Num_metrics,1]
errors_INMET_KDE_MonthHourly[1:(2*Num_metrics),] = metrics[(Num_metrics+1):(3*Num_metrics),]
errors_INMET_Totals[,9] = metrics[((3*Num_metrics)+1):(4*Num_metrics),1]
errors_INMET_KDE_MonthHourly[((2*Num_metrics)+1):(4*Num_metrics),] = metrics[((4*Num_metrics)+1):(6*Num_metrics),]
Num_clusters[(2*(W_Farm-1)+2),8] = metrics[((6*Num_metrics)+1),1]
Num_clusters[(2*(W_Farm-1)+2),9] = metrics[((6*Num_metrics)+1),2]

###################################################################################
setwd(dir)
errors_EXT_Totals
errors_EXT_Power_Curve
errors_EXT_KDE_SinglePeriod
errors_EXT_KDE_Monthly
errors_EXT_KDE_Hourly
errors_EXT_KDE_MonthHourly

write.csv2(errors_EXT_Totals,paste0(Registration_W_Farm$Name_WFarm[W_Farm]," - errors_EXT_Totals.csv"))
write.csv2(errors_INMET_Totals,paste0(Registration_W_Farm$Name_WFarm[W_Farm]," - errors_INMET_Totals.csv"))

write.csv2(errors_EXT_Power_Curve,paste0(Registration_W_Farm$Name_WFarm[W_Farm]," - errors_EXT_Power_Curve.csv"))
write.csv2(errors_INMET_Power_Curve,paste0(Registration_W_Farm$Name_WFarm[W_Farm]," - errors_INMET_Power_Curve.csv"))

write.csv2(errors_EXT_KDE_SinglePeriod,paste0(Registration_W_Farm$Name_WFarm[W_Farm]," - errors_EXT_KDE_SinglePeriod.csv"))
write.csv2(errors_INMET_KDE_SinglePeriod,paste0(Registration_W_Farm$Name_WFarm[W_Farm]," - errors_INMET_KDE_SinglePeriod.csv"))

write.csv2(errors_EXT_KDE_Monthly,paste0(Registration_W_Farm$Name_WFarm[W_Farm]," - errors_EXT_KDE_Monthly.csv"))
write.csv2(errors_INMET_KDE_Monthly,paste0(Registration_W_Farm$Name_WFarm[W_Farm]," - errors_INMET_KDE_Monthly.csv"))

write.csv2(errors_EXT_KDE_Hourly,paste0(Registration_W_Farm$Name_WFarm[W_Farm]," - errors_EXT_KDE_Hourly.csv"))
write.csv2(errors_INMET_KDE_Hourly,paste0(Registration_W_Farm$Name_WFarm[W_Farm]," - errors_INMET_KDE_Hourly.csv"))

write.csv2(errors_EXT_KDE_MonthHourly,paste0(Registration_W_Farm$Name_WFarm[W_Farm]," - errors_EXT_KDE_MonthHourly.csv"))
write.csv2(errors_INMET_KDE_MonthHourly,paste0(Registration_W_Farm$Name_WFarm[W_Farm]," - errors_INMET_KDE_MonthHourly.csv"))

#Tabelas Artigo

#Table 5 – Number of clusters obtained by each approach.
write.csv2(Num_clusters,"Table_Quantity_clusters.csv")

##RMSE##MAE##MAPE#R2
#Table 6 – Results reached by the methods according to the evaluation metrics.
for (i in 1:4) {
  #EXT
  Table_Metrics_Totals[(2*(W_Farm-1)+(10*(i-1))+1),4] = errors_EXT_Totals[i,1]
  Table_Metrics_Totals[(2*(W_Farm-1)+(10*(i-1))+1),5] = errors_EXT_Totals[i,2]
  Table_Metrics_Totals[(2*(W_Farm-1)+(10*(i-1))+1),6] = errors_EXT_Totals[i,4]
  Table_Metrics_Totals[(2*(W_Farm-1)+(10*(i-1))+1),7] = errors_EXT_Totals[i,6]
  Table_Metrics_Totals[(2*(W_Farm-1)+(10*(i-1))+1),8] = errors_EXT_Totals[i,8]
  #INMET
  Table_Metrics_Totals[(2*(W_Farm-1)+(10*(i-1))+2),4] = errors_INMET_Totals[i,1]
  Table_Metrics_Totals[(2*(W_Farm-1)+(10*(i-1))+2),5] = errors_INMET_Totals[i,2]
  Table_Metrics_Totals[(2*(W_Farm-1)+(10*(i-1))+2),6] = errors_INMET_Totals[i,4]
  Table_Metrics_Totals[(2*(W_Farm-1)+(10*(i-1))+2),7] = errors_INMET_Totals[i,6]
  Table_Metrics_Totals[(2*(W_Farm-1)+(10*(i-1))+2),8] = errors_INMET_Totals[i,8]
}
write.csv2(Table_Metrics_Totals,"Table_Metrics_Totals.csv")

##RMSE##MAE##MAPE#R2
#Table 7 - RMSE attained by the methods per month.
for (i in 1:4) {
  #EXT
  Table_Metrics_Month[(10*(W_Farm-1)+(50*(i-1))+1),5:16] = errors_EXT_Power_Curve[i,1:12]
  Table_Metrics_Month[(10*(W_Farm-1)+(50*(i-1))+3),5:16] = errors_EXT_KDE_SinglePeriod[i,1:12]
  Table_Metrics_Month[(10*(W_Farm-1)+(50*(i-1))+5),5:16] = errors_EXT_KDE_Monthly[i,1:12]
  Table_Metrics_Month[(10*(W_Farm-1)+(50*(i-1))+7),5:16] = errors_EXT_KDE_Hourly[i,1:12]
  Table_Metrics_Month[(10*(W_Farm-1)+(50*(i-1))+9),5:16] = errors_EXT_KDE_MonthHourly[i,1:12]
  #INMET
  Table_Metrics_Month[(10*(W_Farm-1)+(50*(i-1))+2),5:16] = errors_INMET_Power_Curve[i,1:12]
  Table_Metrics_Month[(10*(W_Farm-1)+(50*(i-1))+4),5:16] = errors_INMET_KDE_SinglePeriod[i,1:12]
  Table_Metrics_Month[(10*(W_Farm-1)+(50*(i-1))+6),5:16] = errors_INMET_KDE_Monthly[i,1:12]
  Table_Metrics_Month[(10*(W_Farm-1)+(50*(i-1))+8),5:16] = errors_INMET_KDE_Hourly[i,1:12]
  Table_Metrics_Month[(10*(W_Farm-1)+(50*(i-1))+10),5:16] = errors_INMET_KDE_MonthHourly[i,1:12]
}
write.csv2(Table_Metrics_Month,"Table_Metrics_Month.csv")

##RMSE##MAE##MAPE#R2
#Table 8 - RMSE attained by the methods per hour.
for (i in 1:4) {
  #EXT
  Table_Metrics_Hour[(10*(W_Farm-1)+(50*(i-1))+1),5:28] = errors_EXT_Power_Curve[(5+i),1:24]
  Table_Metrics_Hour[(10*(W_Farm-1)+(50*(i-1))+3),5:28] = errors_EXT_KDE_SinglePeriod[(5+i),1:24]
  Table_Metrics_Hour[(10*(W_Farm-1)+(50*(i-1))+5),5:28] = errors_EXT_KDE_Monthly[(5+i),1:24]
  Table_Metrics_Hour[(10*(W_Farm-1)+(50*(i-1))+7),5:28] = errors_EXT_KDE_Hourly[(5+i),1:24]
  Table_Metrics_Hour[(10*(W_Farm-1)+(50*(i-1))+9),5:28] = errors_EXT_KDE_MonthHourly[(5+i),1:24]
  #INMET
  Table_Metrics_Hour[(10*(W_Farm-1)+(50*(i-1))+2),5:28] = errors_INMET_Power_Curve[(5+i),1:24]
  Table_Metrics_Hour[(10*(W_Farm-1)+(50*(i-1))+4),5:28] = errors_INMET_KDE_SinglePeriod[(5+i),1:24]
  Table_Metrics_Hour[(10*(W_Farm-1)+(50*(i-1))+6),5:28] = errors_INMET_KDE_Monthly[(5+i),1:24]
  Table_Metrics_Hour[(10*(W_Farm-1)+(50*(i-1))+8),5:28] = errors_INMET_KDE_Hourly[(5+i),1:24]
  Table_Metrics_Hour[(10*(W_Farm-1)+(50*(i-1))+10),5:28] = errors_INMET_KDE_MonthHourly[(5+i),1:24]
}
write.csv2(Table_Metrics_Hour,"Table_Metrics_Hour.csv")
