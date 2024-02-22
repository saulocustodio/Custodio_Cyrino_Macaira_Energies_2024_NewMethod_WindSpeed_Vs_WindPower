power_curve = function(radius, velocity, air_density,
                       efficiency_factor, cut_in, 
                       cut_off, cut_rated, 
                       rated_power, n_turbinas){
  
  fixo = (pi/2)*(radius^2)*air_density*efficiency_factor/1000 #divide por 1000 para sair em KW
  power_output = n_turbinas*fixo*(velocity^3)
  
  velocidade_0 = which(velocity < cut_in | velocity > cut_off)
  power_output[velocidade_0] = 0
  
  velocidade_rated = which(velocity > cut_rated & velocity < cut_off)
  power_output[velocidade_rated] = rated_power
  
  #Passando para pu:
  power_output = power_output/rated_power
  
  return(power_output)
}

Metricas_Graficos = function(dados_EM02, estimado, N_metr, nome, metodo, abordagem, caso, monthOrder,ind_parque){
  
  Tabela_Metricas = matrix(NA, ncol = 24, nrow = (3*N_metr))
  Tabela_Metricas[1,1] = RMSE_f(dados_EM02$power, estimado)
  Tabela_Metricas[2,1] = MAE_f(dados_EM02$power, estimado)
  Tabela_Metricas[3,1] = MAPE_f(dados_EM02$power, estimado)
  Tabela_Metricas[4,1] = R_squared_f(dados_EM02$power, estimado)
  Tabela_Metricas[5,1] = R_squared_f2(dados_EM02$power, estimado)
  
  contador = 1
  for(mes in monthOrder){
    linhas = which(format(dados_EM02$data, "%b") == mes)
    Tabela_Metricas[(N_metr+1),contador] = RMSE_f(dados_EM02$power[linhas], estimado[linhas])
    Tabela_Metricas[(N_metr+2),contador] = MAE_f(dados_EM02$power[linhas], estimado[linhas])
    Tabela_Metricas[(N_metr+3),contador] = MAPE_f(dados_EM02$power[linhas], estimado[linhas])
    Tabela_Metricas[(N_metr+4),contador] = R_squared_f(dados_EM02$power[linhas], estimado[linhas])
    Tabela_Metricas[(N_metr+5),contador] = R_squared_f2(dados_EM02$power[linhas], estimado[linhas])
    contador = contador + 1
  }
  
  contador = 1
  for(hora in 1:24){
    linhas = which(dados_EM02$Hour == hora)
    Tabela_Metricas[((2*N_metr)+1),contador] = RMSE_f(dados_EM02$power[linhas], estimado[linhas])
    Tabela_Metricas[((2*N_metr)+2),contador] = MAE_f(dados_EM02$power[linhas], estimado[linhas])
    Tabela_Metricas[((2*N_metr)+3),contador] = MAPE_f(dados_EM02$power[linhas], estimado[linhas])
    Tabela_Metricas[((2*N_metr)+4),contador] = R_squared_f(dados_EM02$power[linhas], estimado[linhas])
    Tabela_Metricas[((2*N_metr)+5),contador] = R_squared_f2(dados_EM02$power[linhas], estimado[linhas])
    contador = contador + 1
  }
  
  nome_arq = paste0(nome," - Historical data versus ", metodo," (",abordagem," - ",caso,").png")
  graf_dispersao(dados_EM02, dados_EM02$speed, dados_EM02$power, estimado, paste0("Scatter Plot - Historical vs Estimated (",metodo," - ",abordagem,")"),
                 "Wind speed (m/s)", "Wind power (p.u.)", nome_arq, ind_parque)
  
  nome_arq = paste0(nome," - [Frequency polygon] Historical data versus ", metodo," (",abordagem," - ",caso,").png")
  graf_pol_freq(dados_EM02, dados_EM02$power, estimado, paste0("Frequency Polygons - historical vs estimated (",metodo," - ",abordagem,")"),
                "Wind power (p.u.)", "Density", nome_arq, ind_parque)
  
  return(Tabela_Metricas)
}


Estimacao_Resultados = function(metodo, abordagem, Potencia, Velocidades, Dados_Parq, Parque, n){
  
  dados_EM = data.frame(data=Velocidades$tempo, speed=Velocidades$velocidade,
                        power=Potencia$`Ativa G (kWh)`, Month=Velocidades$mes, 
                        Hour=Velocidades$hora, Year=year(Velocidades$tempo), 
                        capacidade = Dados_Parq$Capacity[Parque], estimado = NA)
  
  dados_EM = dados_EM[which(is.na(dados_EM$power) == F),] #mantem linhas com geracao diferente de NA
  dados_EM = dados_EM[which(dados_EM$power > 0),] #mantem linhas com geracao maior que zero
  dados_EM = dados_EM[which(dados_EM$power <= Dados_Parq$Capacity[Parque]),] #mantem linhas com geracao menor que capacidade do parque
  dados_EM$power = dados_EM$power/Dados_Parq$Capacity[Parque]
  dados_EM = dados_EM[which(dados_EM$speed >= 0),] #mantem linhas com velocidade maior que zero
  dados_EM = dados_EM[which(dados_EM$speed <= Dados_Parq$cut_off[Parque]),] #mantem linhas com velocidade menor que vinte e cinco
  monthOrder = c('jan', 'fev', 'mar', 'abr', 'mai', 'jun','jul', 'ago', 'set', 'out', 'nov', 'dez')
  dados_EM$Month = factor(format(dados_EM$data,"%b"),levels = monthOrder)
  unique(dados_EM$Month)
  #head(dados_EM)
  #tail(dados_EM)
  
  if(metodo == "Power Curve"){
    dados_EM$estimado = power_curve(Dados_Parq$radius[Parque], dados_EM$speed, Dados_Parq$air_density[Parque],
                                    Dados_Parq$efficiency_factor[Parque], Dados_Parq$cut_in[Parque], 
                                    Dados_Parq$cut_off[Parque], Dados_Parq$cut_rated[Parque], 
                                    Dados_Parq$Capacity[Parque], Dados_Parq$n_tur[Parque])
    
    Metricas = Metricas_Graficos(dados_EM, dados_EM$estimado, n, Dados_Parq$Name_WFarm[Parque], metodo, abordagem, " ", monthOrder, Parque)
  }
  
  if(metodo == "Single period"){
    dir_aux = getwd()
    aux = clusterizacao(dados_EM, Dados_Parq$Name_WFarm[Parque], metodo, abordagem, Parque, "")
    dados_EM$cluster = aux$CL
    setwd(dir_aux)
    Resultado = Metodo_KDE(dados_EM, max(aux$CL), aux$Min_S, aux$Max_S)
      
    dados_EM$estimado = rowMeans(Resultado[[1]])
    Metricas1 = Metricas_Graficos(dados_EM, dados_EM$estimado, n, Dados_Parq$Name_WFarm[Parque], metodo, abordagem, " ", monthOrder, Parque)
    
    dados_EM$estimadoCen1 = Resultado[[1]][,1] #pega o primeiro cenario
    Metricas2 = Metricas_Graficos(dados_EM, dados_EM$estimadoCen1, n, Dados_Parq$Name_WFarm[Parque], metodo, abordagem, "Scene 1", monthOrder, Parque)
    
    Num_cl = matrix(NA, ncol = 24, nrow = 1)
    Num_cl[1,1] = max(aux$CL)
    
    Metricas = rbind(Metricas1, Metricas2, Num_cl)
  }
  
  if(metodo == "Monthly"){
    dados_EM$cluster = NA
    resultados_min = resultados_max = matrix(NA, ncol = 12, nrow = 30)
    dir_aux = getwd()
    aux_Num_cluster = matrix(NA, ncol = 12, nrow = 1)
    
    for(mes in 1:12){
      d_aux=paste0(getwd(),"/",monthOrder[mes])
      dir.create(file.path(d_aux), showWarnings = TRUE)
      setwd(d_aux)
      
      linhas = which(dados_EM$Month == monthOrder[mes])
      temp = data.frame(data = dados_EM[linhas,'data'],
                        speed = dados_EM[linhas,'speed'],
                        power = dados_EM[linhas,'power'],
                        Month = dados_EM[linhas,'Month'])
      nom = paste0(Dados_Parq$Name_WFarm[Parque],"_",monthOrder[mes])
      aux = clusterizacao(temp, nom, metodo, abordagem, Parque, paste0(monthOrder[mes]," - ") )
      dados_EM$cluster[linhas] = aux$CL
      n_cluster = max(aux$CL)
      aux_Num_cluster[1,mes] = n_cluster
      resultados_min[1:n_cluster,mes] = aux$Min_S
      resultados_max[1:n_cluster,mes] = aux$Max_S
      
      setwd(dir_aux)
    }
    
    Resultado = Metodo_KDE_Monthly(dados_EM, resultados_min, resultados_max, monthOrder)
    
    dados_EM$estimado = rowMeans(Resultado[[1]])
    Metricas1 = Metricas_Graficos(dados_EM, dados_EM$estimado, n, Dados_Parq$Name_WFarm[Parque], metodo, abordagem, " ", monthOrder, Parque)
    
    dados_EM$estimadoCen1 = Resultado[[1]][,1] #pega o primeiro cenario
    Metricas2 = Metricas_Graficos(dados_EM, dados_EM$estimadoCen1, n, Dados_Parq$Name_WFarm[Parque], metodo, abordagem, "Scene 1", monthOrder, Parque)
    
    Num_cl = matrix(NA, ncol = 24, nrow = 1)
    Num_cl[1,1] = min(aux_Num_cluster)
    Num_cl[1,2] = max(aux_Num_cluster)
    
    Metricas = rbind(Metricas1, Metricas2, Num_cl)
  }

  if(metodo == "Hourly"){
    dados_EM$cluster = NA
    resultados_min = resultados_max = matrix(NA, ncol = 24, nrow = 30)
    dir_aux = getwd()
    aux_Num_cluster = matrix(NA, ncol = 24, nrow = 1)
    
    for(hora in 1:24){
      d_aux=paste0(getwd(),"/Hour ",hora)
      dir.create(file.path(d_aux), showWarnings = TRUE)
      setwd(d_aux)
      
      linhas = which(dados_EM$Hour == hora)
      temp = data.frame(data = dados_EM[linhas,'data'],
                        speed = dados_EM[linhas,'speed'],
                        power = dados_EM[linhas,'power'],
                        Month = dados_EM[linhas,'Month'],
                        Hour = dados_EM[linhas,'Hour'])
      nom = paste0(Dados_Parq$Name_WFarm[Parque],"_Hour",hora)
      aux = clusterizacao(temp, nom, metodo, abordagem, Parque, paste0("Hour ",hora, " - "))
      dados_EM$cluster[linhas] = aux$CL
      n_cluster = max(aux$CL)
      aux_Num_cluster[1,hora] = n_cluster
      resultados_min[1:n_cluster,hora] = aux$Min_S
      resultados_max[1:n_cluster,hora] = aux$Max_S
      
      setwd(dir_aux)
    }
    
    Resultado = Metodo_KDE_hourly(dados_EM, resultados_min, resultados_max)
    
    dados_EM$estimado = rowMeans(Resultado[[1]])
    Metricas1 = Metricas_Graficos(dados_EM, dados_EM$estimado, n, Dados_Parq$Name_WFarm[Parque], metodo, abordagem, " ", monthOrder, Parque)
    
    dados_EM$estimadoCen1 = Resultado[[1]][,1] #pega o primeiro cenario
    Metricas2 = Metricas_Graficos(dados_EM, dados_EM$estimadoCen1, n, Dados_Parq$Name_WFarm[Parque], metodo, abordagem, "Scene 1", monthOrder, Parque)
    
    Num_cl = matrix(NA, ncol = 24, nrow = 1)
    Num_cl[1,1] = min(aux_Num_cluster)
    Num_cl[1,2] = max(aux_Num_cluster)
    
    Metricas = rbind(Metricas1, Metricas2, Num_cl)
  }
  
  if(metodo == "Month-Hourly"){
    dados_EM$cluster = NA
    resultados_min = resultados_max = list()
    r_min = r_max = matrix(NA, ncol = 24, nrow = 30)
    dir_aux = getwd()
    aux_Num_cluster = matrix(NA, ncol = 24, nrow = 12)
    
    for(mes in 1:12){
      for (hora in 1:24) {
        d_aux=paste0(getwd(),"/",monthOrder[mes],"_Hour",hora)
        dir.create(file.path(d_aux), showWarnings = TRUE)
        setwd(d_aux)
        
        linhas = which(dados_EM$Month == monthOrder[mes] & dados_EM$Hour == hora)
        temp = data.frame(data = dados_EM[linhas,'data'],
                          speed = dados_EM[linhas,'speed'],
                          power = dados_EM[linhas,'power'],
                          Month = dados_EM[linhas,'Month'],
                          Hour = dados_EM[linhas,'Hour'])
        nom = paste0(Dados_Parq$Name_WFarm[Parque],"_",monthOrder[mes],"_Hour",hora)
        aux = clusterizacao(temp, nom, metodo, abordagem, Parque, paste0(monthOrder[mes],"-Hour ",hora, " - "))
        dados_EM$cluster[linhas] = aux$CL
        n_cluster = max(aux$CL)
        aux_Num_cluster[mes,hora] = n_cluster
        r_min[1:n_cluster,hora] = aux$Min_S
        r_max[1:n_cluster,hora] = aux$Max_S
        
        setwd(dir_aux)
      }
      resultados_min = c(resultados_min,list(r_min))
      resultados_max = c(resultados_max,list(r_max))
      r_min = r_max = matrix(NA, ncol = 24, nrow = 30)
    }
    
    Resultado = Metodo_KDE_MonthHourly(dados_EM, resultados_min, resultados_max, monthOrder)
    
    dados_EM$estimado = rowMeans(Resultado[[1]])
    Metricas1 = Metricas_Graficos(dados_EM, dados_EM$estimado, n, Dados_Parq$Name_WFarm[Parque], metodo, abordagem, " ", monthOrder, Parque)
    
    dados_EM$estimadoCen1 = Resultado[[1]][,1] #pega o primeiro cenario
    Metricas2 = Metricas_Graficos(dados_EM, dados_EM$estimadoCen1, n, Dados_Parq$Name_WFarm[Parque], metodo, abordagem, "Scene 1", monthOrder, Parque)
    
    Num_cl = matrix(NA, ncol = 24, nrow = 1)
    Num_cl[1,1] = min(aux_Num_cluster)
    Num_cl[1,2] = max(aux_Num_cluster)
    
    Metricas = rbind(Metricas1, Metricas2, Num_cl)
  }
  
  
  
  return(Metricas)
}

