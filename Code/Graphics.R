graf_dispersao = function(base, var_x, var_y, var_y2, titulo, eixo_x,
                          eixo_y, nome_arquivo, p){
  
  if(p==1){cor_curva ="green"}
  if(p==2){cor_curva ="#03BB85"}
  if(p==3){cor_curva ="#00AAE4"}
  if(p==4){cor_curva ="#5C83B4"}
  if(p==5){cor_curva ="blue"}
  
  ggplot(base, aes(var_x, var_y, color = var_y)) +
    geom_point(colour=cor_curva, alpha = 0.2) +
    geom_point(aes(var_x,var_y2, color = var_y2), colour="black") +
    ggtitle(titulo) +
    xlab(eixo_x) +
    ylab(eixo_y) +
    scale_x_continuous(limits = c(0, 16))+
    theme_minimal()+
    theme(legend.position = "none",
          axis.text = element_text(size = 6),
          axis.title = element_text(size = 8),
          title = element_text(size = 8))
  
  ggsave(nome_arquivo, width = 15, height = 9, units = "cm")
}

graf_pol_freq = function(base, var_x, var_x2, titulo, eixo_x,
                          eixo_y, nome_arquivo, p){
  
  if(p==1){cor_curva ="green"}
  if(p==2){cor_curva ="#03BB85"}
  if(p==3){cor_curva ="#00AAE4"}
  if(p==4){cor_curva ="#5C83B4"}
  if(p==5){cor_curva ="blue"}
  
  ggplot(base, aes(var_x, after_stat(density), colour = var_x)) +
    #geom_freqpoly(binwidth = 50) +
    geom_freqpoly(colour = cor_curva) +
    #geom_freqpoly(aes(var_x2, colour = "ESTIM"), binwidth = 50) +
    geom_freqpoly(aes(var_x2, colour = var_x2), colour="black") +
    ggtitle(titulo) +
    xlab(eixo_x) +
    ylab(eixo_y) +
    theme_minimal()+
    theme(legend.position = "none",
          axis.text = element_text(size = 6),
          axis.title = element_text(size = 8),
          title = element_text(size = 8))
  
  ggsave(nome_arquivo, width = 15, height = 9, units = "cm")
}

graf_Cotovelo = function(base, var_x, var_x2, num_cl, titulo, eixo_x,
                         eixo_y, nome_arquivo){
  
  ggplot(data = base, aes(var_x, var_x2)) +
    geom_line() +
    geom_point(size = c(rep(1,num_cl-1),3,rep(1,var_x[length(var_x)]-num_cl)), col = c(rep(1,num_cl-1),2,rep(1,var_x[length(var_x)]-num_cl))) +
    scale_x_discrete(limits = var_x) +
    xlab(eixo_x) +
    ylab(eixo_y) +
    ggtitle(titulo) +
    theme_minimal()+
    theme(axis.text = element_text(size = 6),
          axis.title = element_text(size = 8),
          title = element_text(size = 8))
  
  ggsave(nome_arquivo, width = 15, height = 9, units = "cm")
}


graf_dispersao_clusters = function(base, var_x, var_y, var_cluster, titulo, eixo_x,
                          eixo_y, nome_arquivo, p){
  
  ggplot(base, aes(var_x, var_y, color = var_cluster)) +
    geom_point() +
    ggtitle(titulo) +
    xlab(eixo_x) +
    ylab(eixo_y) +
    scale_x_continuous(limits = c(0, 16))+
    theme_minimal()+
    theme(legend.position = "none",
          axis.text = element_text(size = 6),
          axis.title = element_text(size = 8),
          title = element_text(size = 8))
  
  
  ggsave(nome_arquivo, width = 15, height = 9, units = "cm")
}


graf_densidade = function(base, var_x, titulo, eixo_x, eixo_y, nome_arquivo, p){
  
  if(p==1){cor_curva ="green"}
  if(p==2){cor_curva ="#03BB85"}
  if(p==3){cor_curva ="#00AAE4"}
  if(p==4){cor_curva ="#5C83B4"}
  if(p==5){cor_curva ="blue"}
  
  ggplot(data = base, aes_string(x = "var_x")) + 
    geom_density(fill = cor_curva, colour = cor_curva, alpha = 0.5) +
    ggtitle(titulo) +
    xlab(eixo_x) +
    ylab(eixo_y) +
    scale_x_continuous(limits = c(0, 1))+
    theme_minimal()+
    theme(legend.position = "none",
          axis.text = element_text(size = 6),
          axis.title = element_text(size = 8),
          title = element_text(size = 8))
  
  ggsave(nome_arquivo, width = 15, height = 9, units = "cm")
}
