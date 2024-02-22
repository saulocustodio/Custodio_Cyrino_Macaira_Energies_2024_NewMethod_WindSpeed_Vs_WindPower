wss = function(k,aux){
  #tamanho = 1
  #while (tamanho > 0) {
    a = kmeans(aux, k, nstart = 10 )
    #tamanho = length(which(a$size < 4))
  #}
  #talves nao eh melhor remover nstart?
  #kmeans(temp$speed, k )$tot.withinss
  return(a)
}

clusterizacao = function(temp, nome, metodo, abordagem, p, marco){
  
  Vet_cluster = rep(NA, nrow(temp))
  
  wss_values  = rep(NA, 30)
  wss_size  = rep(NA, 30)
  for (k.values in 1:30) {
    clt = wss(k.values,temp$speed)
    wss_values[k.values] = clt$tot.withinss
    wss_size[k.values] = min(clt$size)
  }
  
  k.values = 1:30
  df = data.frame(k.values = k.values, wss_values = wss_values)
  df$perc = df$wss_values/df$wss_values[1]
  df$diference = NA
  for(i in k.values){
    if(i == length(k.values)){df$diference[i] = 0}else{
      df$diference[i] = round(df$perc[i] - df$perc[i+1],3)
    }
  }
  n_cluster = which(df$diference < 0.001)[1]
  
  while(wss_size[n_cluster] < 4) {
    n_cluster=n_cluster-1
  }
  
  nome_arquivo = paste0(nome," - Number of clusters (",n_cluster, " - ", metodo," - ", abordagem,").png")
  graf_Cotovelo(df, df$k.values, df$wss_values, n_cluster, paste0("Application of the Elbow Method (",metodo," - ", marco, abordagem,")"), "Number of clusters K",
                "With-in-Sum-of-Squares (WSS)", nome_arquivo)
  
  cl = kmeans(temp$speed, n_cluster)
  while (min(cl$size) < 4) {
    cl = kmeans(temp$speed, n_cluster)
  }
  Vet_cluster = cl$cluster
   
  labelled_points = tibble(
    cluster = cl$cluster,
    x1 = temp$speed,
    x2 = temp$power)
  
  nome_arquivo = paste0(nome," - Data by Clusters (", metodo," - ", abordagem,").png")
  graf_dispersao_clusters(labelled_points, labelled_points$x1, labelled_points$x2, labelled_points$cluster, paste0("Scatter Plot - Historical by cluster (",metodo," - ", marco, abordagem,")"), 
                          "Wind speed (m/s)", "Wind power (p.u.)", nome_arquivo, p)
  
  dados_EM_divisao = list()
  min_speed = max_speed = NULL
  for(i in 1:n_cluster){
    dados_EM_divisao = c(dados_EM_divisao,
                         list(temp$power[which(cl$cluster == i)]))
    min_speed[i] = min(temp$speed[which(cl$cluster == i)])
    max_speed[i] = max(temp$speed[which(cl$cluster == i)])
  }
  
  dir_aux=paste0(getwd(),"/Density Graphics")
  dir.create(file.path(dir_aux), showWarnings = TRUE)
  setwd(dir_aux)
  for(i in 1:n_cluster){
    t = data.frame(x = 1:length(dados_EM_divisao[[i]]), y_df = dados_EM_divisao[[i]])
    
    tit = paste0("Density Curve (",metodo," - ", marco ,abordagem,"): " , round(min_speed[i],2), "<=", " Wind speed (m/s) ", "<", round(max_speed[i],2))
    nome_arquivo = paste0(nome," - Cluster Density Curve (", metodo," - ", abordagem,") [",round(min_speed[i],2)," - ",round(max_speed[i],2),"] N°", i,".png")
    graf_densidade(t, t$y_df, tit, "Wind power (p.u.)", "Density", nome_arquivo, p)
    
  }
  
  Saida = list(CL=Vet_cluster, Min_S=min_speed, Max_S=max_speed)

  return(Saida)
}

Metodo_KDE = function(temp, n_cluster, resultados_min, resultados_max){
  # Kernel Density Estimator
  kde_resultados = list()
  
  for(i in 1:n_cluster){
    linhas = which(temp$cluster == i)
    kde = density(temp$power[linhas])
    kde_resultados = c(kde_resultados,list(kde))
  }
  
  total_cenarios = 100
  resultado_final = list()
  
  speed_original = temp$speed
  power_original = temp$power
  cenarios = matrix(NA, ncol = total_cenarios, nrow = length(power_original))
  
  for(obs in 1:length(speed_original)){
    speed = speed_original[obs]
    cluster = which(speed >= resultados_min & speed <= resultados_max)
    if(length(cluster) == 0){
      cluster1 = NULL
      n_speed = n_speed1 = speed
      while(length(cluster) == 0 & length(cluster1) == 0){
        n_speed = (n_speed+0.1)
        n_speed1 = (n_speed1-0.1)
        cluster = which(n_speed >= resultados_min & n_speed <= resultados_max) # estranho mes2 n?o aparece antes
        cluster1 = which(n_speed1 >= resultados_min & n_speed1 <= resultados_max)  # estranho mes2 n?o aparece antes
      }
      if(length(cluster) == 0){cluster = cluster1}
    }
    kde = kde_resultados[[cluster[1]]]
    cenarios[obs,] = sample(x = kde$x, size = ncol(cenarios), prob = kde$y, replace = TRUE)
    print(paste(obs,"/",length(speed_original)))
  }
  resultado_final = c(resultado_final, list(cenarios))
  
  return(resultado_final)
}

Metodo_KDE_Monthly = function(temp, resultados_min, resultados_max, monthOrder){
  # Kernel Density Estimator
  kde_resultados = list('Jan' = list(), 'Feb' = list(), 'Mar' = list(),
                        'Apr' = list(), 'May' = list(), 'Jun' = list(),
                        'Jul' = list(), 'Aug' = list(), 'Set' = list(),
                        'Oct' = list(), 'Nov' = list(), 'Dec' = list())
  
  for(mes in 1:12){
    mes2 = monthOrder[mes]
    n_cluster = which(is.na(resultados_max[,mes]))[1]-1
    for(i in 1:n_cluster){
      linhas = which(temp$Month == mes2 & temp$cluster == i)
      kde = density(temp$power[linhas])
      kde_resultados[[mes]] = c(kde_resultados[[mes]],list(kde))
    }
    print(mes)
  }
  
  total_cenarios = 100
  resultado_final = list()
  
  speed_original = temp$speed
  power_original = temp$power
  cenarios = matrix(NA, ncol = total_cenarios, nrow = length(power_original))
  
  for(obs in 1:length(speed_original)){
    speed = speed_original[obs]
    mes = format(temp$data[obs], "%b")
    mes2 = as.numeric(format(temp$data[obs], "%m"))
    cluster = which(speed >= resultados_min[,mes2] & speed <= resultados_max[,mes2])
    if(length(cluster) == 0){
      cluster1 = NULL
      n_speed = n_speed1 = speed
      while(length(cluster) == 0 & length(cluster1) == 0){
        n_speed = (n_speed+0.1)
        n_speed1 = (n_speed1-0.1)
        cluster = which(n_speed >= resultados_min[,mes2] & n_speed <= resultados_max[,mes2])
        cluster1 = which(n_speed1 >= resultados_min[,mes2] & n_speed1 <= resultados_max[,mes2])
      }
      if(length(cluster) == 0){cluster = cluster1}
    }
    kde = kde_resultados[[mes2]][[cluster[1]]]
    cenarios[obs,] = sample(x = kde$x, size = ncol(cenarios), prob = kde$y, replace = TRUE)
    print(paste(obs,"/",length(speed_original)))
  }
  
  resultado_final = c(resultado_final, list(cenarios))
  
  return(resultado_final)
}

Metodo_KDE_hourly = function(temp, resultados_min, resultados_max){
  # Kernel Density Estimator
  kde_resultados = list('Hour1' = list(), 'Hour2' = list(), 'Hour3' = list(), 'Hour4' = list(), 'Hour5' = list(), 'Hour6' = list(),
                        'Hour7' = list(), 'Hour8' = list(), 'Hour9' = list(), 'Hour10' = list(), 'Hour11' = list(), 'Hour12' = list(),
                        'Hour13' = list(), 'Hour14' = list(), 'Hour15' = list(), 'Hour16' = list(),'Hour17' = list(),'Hour18' = list(),
                        'Hour19' = list(), 'Hour20' = list(), 'Hour21' = list(), 'Hour22' = list(),'Hour23' = list(),'Hour24' = list())
  
  for(hora in 1:24){
    n_cluster = which(is.na(resultados_max[,hora]))[1]-1
    for(i in 1:n_cluster){
      linhas = which(temp$Hour == hora & temp$cluster == i)
      kde = density(temp$power[linhas])
      kde_resultados[[hora]] = c(kde_resultados[[hora]],list(kde))
    }
    print(hora)
  }
  
  total_cenarios = 100
  resultado_final = list()
  
  speed_original = temp$speed
  power_original = temp$power
  cenarios = matrix(NA, ncol = total_cenarios, nrow = length(power_original))
  
  for(obs in 1:length(speed_original)){
    speed = speed_original[obs]
    hora = temp$Hour[obs]
    #mes = format(temp$data[obs], "%b")
    #mes2 = as.numeric(format(temp$data[obs], "%m"))
    cluster = which(speed >= resultados_min[,hora] & speed <= resultados_max[,hora])
    if(length(cluster) == 0){
      cluster1 = NULL
      n_speed = n_speed1 = speed
      while(length(cluster) == 0 & length(cluster1) == 0){
        n_speed = (n_speed+0.1)
        n_speed1 = (n_speed1-0.1)
        cluster = which(n_speed >= resultados_min[,hora] & n_speed <= resultados_max[,hora])
        cluster1 = which(n_speed1 >= resultados_min[,hora] & n_speed1 <= resultados_max[,hora])
      }
      if(length(cluster) == 0){cluster = cluster1}
    }
    kde = kde_resultados[[hora]][[cluster[1]]]
    cenarios[obs,] = sample(x = kde$x, size = ncol(cenarios), prob = kde$y, replace = TRUE)
    print(paste(obs,"/",length(speed_original)))
  }
  
  resultado_final = c(resultado_final, list(cenarios))
  
  return(resultado_final)
}

Metodo_KDE_MonthHourly = function(temp, resultados_min, resultados_max, monthOrder){
  
  kde_r_hora = list('Hour1' = list(), 'Hour2' = list(), 'Hour3' = list(), 'Hour4' = list(), 'Hour5' = list(), 'Hour6' = list(),
                    'Hour7' = list(), 'Hour8' = list(), 'Hour9' = list(), 'Hour10' = list(), 'Hour11' = list(), 'Hour12' = list(),
                    'Hour13' = list(), 'Hour14' = list(), 'Hour15' = list(), 'Hour16' = list(),'Hour17' = list(),'Hour18' = list(),
                    'Hour19' = list(), 'Hour20' = list(), 'Hour21' = list(), 'Hour22' = list(),'Hour23' = list(),'Hour24' = list())
  
  # Kernel Density Estimator
  kde_resultados = list('Jan' = kde_r_hora, 'Feb' = kde_r_hora, 'Mar' = kde_r_hora,
                        'Apr' = kde_r_hora, 'May' = kde_r_hora, 'Jun' = kde_r_hora,
                        'Jul' = kde_r_hora, 'Aug' = kde_r_hora, 'Set' = kde_r_hora,
                        'Oct' = kde_r_hora, 'Nov' = kde_r_hora, 'Dec' = kde_r_hora)
  ###
  ## Problema
  for(mes in 1:12){
    mes2 = monthOrder[mes]
    for (hora in 1:24) {
      n_cluster = which(is.na(resultados_max[[mes]][,hora]))[1]-1
      for(i in 1:n_cluster){
        linhas = which(temp$Month == mes2 & temp$Hour == hora & temp$cluster == i)
        kde = density(temp$power[linhas])
        kde_resultados[[mes]][[hora]] = c(kde_resultados[[mes]][[hora]],list(kde))
      }
    }
  }
  
  total_cenarios = 100
  resultado_final = list()
  
  speed_original = temp$speed
  power_original = temp$power
  cenarios = matrix(NA, ncol = total_cenarios, nrow = length(power_original))
  
  for(obs in 1:length(speed_original)){
    speed = speed_original[obs]
    mes = format(temp$data[obs], "%b")
    mes2 = as.numeric(format(temp$data[obs], "%m"))
    hora = temp$Hour[obs]
    cluster = which(speed >= resultados_min[[mes2]][,hora] & speed <= resultados_max[[mes2]][,hora])
    if(length(cluster) == 0){
      cluster1 = NULL
      n_speed = n_speed1 = speed
      while(length(cluster) == 0 & length(cluster1) == 0){
        n_speed = (n_speed+0.1)
        n_speed1 = (n_speed1-0.1)
        cluster = which(n_speed >= resultados_min[[mes2]][,hora] & n_speed <= resultados_max[[mes2]][,hora])
        cluster1 = which(n_speed1 >= resultados_min[[mes2]][,hora] & n_speed1 <= resultados_max[[mes2]][,hora])
      }
      if(length(cluster) == 0){cluster = cluster1}
    }
    kde = kde_resultados[[mes2]][[hora]][[cluster[1]]]
    cenarios[obs,] = sample(x = kde$x, size = ncol(cenarios), prob = kde$y, replace = TRUE)
    print(paste(obs,"/",length(speed_original)))
  }
  
  resultado_final = c(resultado_final, list(cenarios))
  
  return(resultado_final)
}
