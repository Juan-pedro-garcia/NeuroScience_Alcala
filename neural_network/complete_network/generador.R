library(plotly)
source("./funciones.R")
system.time({
  
  
  datos_RFB <- read.csv2("./RFB_valors.csv2")
  aceptables_RFB <- which(datos_RFB[,"freq_inter"]>2 & datos_RFB[,"freq_inter"]<9 &datos_RFB[,"freq_intra.ms."]>100)
  
  datos_RS <- read.csv2("./RS_valors.csv2")
  aceptables_RS <- which(datos_RS[,"freq.ms."]>5 &datos_RS[,"freq.ms."]<6)
  
  datos_RSB <- read.csv2("./RSB_valors.csv2")
  aceptables_RSB <- which(datos_RSB[,"freq_inter"]>0.4 &datos_RSB[,"freq_inter"]<0.5 & datos_RSB[,"freq_intra.ms."]<20)
  
  ##caracterización neuronas##
  #number of neurons of each class
  ISe <- 0
  ISi <- 0
  ISB <- 0
  IFB <- 0
  A <- 1
  RS <- 60
  RSB <- 30
  RFB <- 10
  
  #three constant are needed to each class
  #regular neurons depend on a Simple armonic Movement function, so the need
  #angular velocity and Amplitud. 
  #the constant "a" of the regular neurons depends of the period.

  
  h <- sample(aceptables_RFB,RFB,replace = TRUE)
  f <- sample(aceptables_RS,RS,replace = TRUE)
  g <- sample(aceptables_RSB,RSB,replace = TRUE)
  
  a <- c(rep(0.02,ISe),rep(0.02,ISi),rep(0.14,ISB),
         rep(0.1,IFB),rep(0.02,A),
         datos_RS[f,"a"],datos_RSB[g,"a"],datos_RFB[h,"a"])
  
  b <- c(rep(0.2,ISe),rep(0.2,ISi),runif(ISB,0.263,0.264),
         runif(IFB,0.249,0.251),rep(0.2,A),
         datos_RS[f,"b"],datos_RSB[g,"b"],datos_RFB[h,"b"])
  
  c <- c(rep(-65,ISe),rep(-65,ISi),rep(-65,ISB),
         rep(-65,IFB),rep(-65,A),
         rep(-65,RS),datos_RSB[g,"c"],datos_RFB[h,"c"])
  
  d <- c(rep(8,ISe),rep(8,ISi),runif(ISB,-8,-8),runif(IFB,-8,-7.95),rep(8,A))
  periodo <-c(d,datos_RS[f,"period"],datos_RSB[g,"period"],datos_RFB[h,"period"])
  
  ######## diseño circuito ######
  cantidad_neu <- c(ISe,ISi,ISB,IFB,A,RS,RSB,RFB)
  
  size <- sum(cantidad_neu) #total number of neurons
  
  nombres_neu <- c("ISe","ISi","ISB","IFB","A","RS","RSB","RFB")
  circuito <- matrix(data=0,nrow=size,ncol=size)
  tipos <- rep(nombres_neu,cantidad_neu)
  burst <- which(tipos%in%c("IFB","ISB","RSB","RFB"))
  datos_conexiones <- data.frame(nombres_neu,cumsum(cantidad_neu))
  
  colnames(datos_conexiones) <- c("nombre","numero_neurons_tipo")
  colnames(circuito ) <- tipos
  rownames(circuito ) <- tipos
  
  ############################
  # generador aleatorio de   #
  #         conexiones       #
  ############################
  
  
  #the function "conexiones" create conexion between two types of neurons
  
  circuito[which(tipos=="RSB"),which(tipos=="RS")] <- conexiones(RSB,RS,13,4)
  circuito[which(tipos=="RS"),which(tipos=="RFB")] <- conexiones(RS,RFB,-13,4)
  circuito[which(tipos=="RS"),which(tipos=="RSB")] <- conexiones(RS,RSB,-13,4)
  circuito[which(tipos=="RFB"),which(tipos=="RSB")] <- conexiones(RFB,RSB,13,4)
  # circuito[which(tipos=="RSB"),which(tipos=="A")] <- conexiones(RSB,A,6,4)
  # circuito[which(tipos=="A"),which(tipos=="A")] <- conexiones(A,A,13,4)
  # circuito[which(tipos=="A"),which(tipos=="RS")] <- conexiones(A,RS,18,4)
  # circuito[which(tipos=="A"),which(tipos=="RFB")] <- conexiones(A,RFB,13,4)
  # circuito[which(tipos=="RS"),which(tipos=="A")] <- conexiones(RS,A,13,4)
  # circuito[which(tipos=="RS"),which(tipos=="ISe")] <- conexiones(RS,ISe,13,4)
  # circuito[which(tipos=="RFB"),which(tipos=="ISB")] <- conexiones(RFB,ISB,13,4)
  # circuito[which(tipos=="ISB"),which(tipos=="IFB")] <- conexiones(ISB,IFB,13,4)
  # circuito[which(tipos=="ISe"),which(tipos=="IFB")] <- conexiones(ISe,IFB,13,4)

  
  #############################
  #        SIMULACION         #
  #############################
  
  
  # library("plot.matrix")
  
  ######### parámetros controlables del circuito############
  
  max_delay <- 5
  min_delay <- 1
  tiempo <- 100
  
  ############  parámetros y variables internos###############
  
  delays <- sample(min_delay:max_delay,size,replace = TRUE)
  contador <- matrix(data=0,nrow=size,ncol=2)
  
  volt <- c
  reg <- rep(-13,size)
  inputs <- rep(0, size)
  
  lim <- datos_conexiones[5,2]
  
  punto_medio <- reg-(b*cos(a*0)/a-b*cos(a*pi/a)/a)/2
  tclave <- acos((-16-punto_medio)*a/b)/a
  t=rep(0,size)
  
  # grupos_nume <- round(size/grupos_tama+0.4)
  
  grupo_tag <- as.numeric(as.factor(colnames(circuito)))
  grupos_nume <- max(grupo_tag)
  nombres_grupos <- levels(as.factor(colnames(circuito)))
  # grupo_tag[which(tipos=="A")] <- grupos_nume
  list_aferentes <- which(colnames(circuito)=="A")
  
  ###################sim de disp####################
  
  sim <- list()
  sim <- vector("list",length=size)
  sim_con <- c(0)
  sim_con_2 <- c()
  grupo <- vector("list",length=grupos_nume)
  
  for (i in 1:length(grupo)) {
    grupo[[i]] <- 0
  }
  
  for (i in 1:size) {
    sim[[i]] <- 0  
    
  }
  for (j in 1:tiempo) {
    print(j)
    grupocorto <- vector("list",length=grupos_nume)
    sim_short_con <- c()
    sim_short_con_2 <- c()
    
    sim_short <- vector("list",length=size)
    for (i in 1:1000) {
      t=t+1
      
      volt[1:(lim)]=volt[1:(lim)]+0.5*((0.04*volt[1:(lim)]+5)*volt[1:(lim)]+140-reg[1:(lim)]+inputs[1:(lim)])
      volt[1:(lim)]=volt[1:(lim)]+0.5*((0.04*volt[1:(lim)]+5)*volt[1:(lim)]+140-reg[1:(lim)]+inputs[1:(lim)])
      reg[1:(lim)]=reg[1:(lim)]+a[1:(lim)]*(b[1:(lim)]*volt[1:(lim)]-reg[1:(lim)]);
      
      volt[(lim+1):size]=volt[(lim+1):size]+0.5*((0.04*volt[(lim+1):size]+5)*volt[(lim+1):size]+140-reg[(lim+1):size]+inputs[(lim+1):size])
      volt[(lim+1):size]=volt[(lim+1):size]+0.5*((0.04*volt[(lim+1):size]+5)*volt[(lim+1):size]+140-reg[(lim+1):size]+inputs[(lim+1):size])
      reg[(lim+1):size] <- reg[(lim+1):size]-sin((t[(lim+1):size])*(a[(lim+1):size]))*(b[(lim+1):size])
      
      inputs <- rep(0, size)
      
      if(any(volt>30)){
        
        disp <- which(volt>30)
        DR <-c((lim+1):size)[which(c((lim+1):size)%in%disp)] 
        DI <- c(1:(lim))[which(c(1:(lim))%in%disp)]
        DA <- which(disp%in%list_aferentes)
        
        contador[disp,1] <-contador[disp,1]+ delays[disp]
        contador[disp,2] <- contador[disp,2]+1
        
        volt[disp]<- c[disp]
        
        reg[DI]=reg[DI]+d[DI]
        reg[DA] <- -13
        
        for (k in 1:length(disp)) {
          
          sim_short[[disp[k]]] <- c(sim_short[[disp[k]]], (j+i/1000))
          grupocorto[[grupo_tag[disp[k]]]] <- c(grupocorto[[grupo_tag[disp[k]]]],(j+i/1000))
          if (!disp[k]%in%burst) {
            sim_short_con_2 <- c(sim_short_con_2,j+i/1000)
          }
          if (!disp[k]%in%list_aferentes) {sim_short_con <- c(sim_short_con,j+i/1000)}
        }
        
        
        disp <- c()
      }
      contador2 <- ceiling(contador[,1]/delays)
      contador[,1] <- contador[,1]-contador[,2]
      contador[,2] <- ceiling(contador[,1]/delays)
      y <- which(contador[,2]<contador2)
      
      if(!identical(y, integer(0))){
        if (length(y)==1) {inputs <- circuito[y,]
        }else{inputs <- colSums(circuito[y,]) }
        receptor <- which(inputs!=0)[which(inputs!=0)%in%c((lim+1):size)]   
        t[receptor] <- round(t[receptor]+((tclave[receptor]-t[receptor]%%(periodo[receptor]/2))*(inputs[receptor]/20)))
        reg[receptor] <- punto_medio[receptor]+b[receptor]*cos(a[receptor]*t[receptor])/a[receptor]
      }
      
      
    }
    sim_con <- c(sim_con,sim_short_con)
    sim_con_2 <- c(sim_con_2,sim_short_con_2)
    for (i in 1:size) {
      sim[[i]] <- c(sim[[i]],sim_short[[i]])
    }
    for (i in 1:(grupos_nume)) {
      grupo[[i]] <- c(grupo[[i]],grupocorto[[i]])
      
    }
  }
  grupo[[length(grupo)+1]] <- sim_con
  maximo <- max(sapply(sim,length))
  final <- data.frame(matrix(data=NA, nrow=maximo, ncol=size))
  nah <- c()
  for (i in 1:size) {
    final[,i] <- c(sim[[i]],rep(NA,maximo-length(sim[[i]])))
    nah <- c(nah,paste0("U",sprintf("%02d", as.numeric(i))))
  }
  
  
  
  colnames(final) <- (nah)
  colnames(final) <- tipos
  
  #############################
  #         METRICAS          #
  #############################
  
  
  lista_metricas <- list()
  metricas <- data.frame(c(rep(0,5)))
  tipo_irre_burst <- c("IFB","ISB")
  tipo_re_burst <- c("RFB","RSB")
  for(i in 1:ncol(final)){
    print(i)
    temp <- final[which(!is.na(final[,i])),i]
    if (length(temp)<=3) {next}
    temp <- temp[-1]
    
    if(colnames(final)[i]=="ISe"||colnames(final)[i]=="ISi"){nom <- "IS"}else{nom <- colnames(final)[i]}
    
    
    media <- mean_freq(temp)
    CV <- NA
    media_intraburst <-  NA
    media_interburst <- NA
    CV_intrab <- NA
    if(nom=="RS"){
      CV <- cal_CV(temp)
    }else{
      diff <-  temp[-1]-temp[-length(temp)]
      x <- cutree(hclust(dist(diff),method = "single"),2)
      
      if(nom%in%tipo_irre_burst){
        media_intraburst <- mean_intraburst_freq(diff,x)
        media_interburst <- mean_interburst_freq(diff,x)
      }else{
        if (nom%in%tipo_re_burst) {
          CV <- cal_CV_burst(diff,x)
          media_intraburst <- mean_intraburst_freq(diff,x)
          media_interburst <- mean_interburst_freq(diff,x)
          CV_intrab <- CV_intraburst(diff,x)
        }
      }
    }
    metricas_temp <- c(media, CV, media_intraburst,media_interburst,CV_intrab)
    lista_metricas[[nom]] <- cbind(lista_metricas[[nom]],metricas_temp)
  }
  
  for(i in 1:length(lista_metricas)){
    metricas_temp <- apply(lista_metricas[[i]],MARGIN = 1,FUN = mean)
    metricas <- cbind(metricas,metricas_temp)
  }
  
  metricas <- metricas[,-1]
  colnames(metricas) <- names(lista_metricas)
  
  rownames(metricas) <- c("media","CV","media_intraburst","media_interburst","CV_intrab")
  
  
  
  
})

#######################
#       EVENTOS       #
#######################
#se considera que se produce un evento cuando la frecuencia de disparos en los 200ms 
#anteriores a un disparo supera un umbral.

vector_metricas <- as.matrix(metricas)[which(!is.na(metricas))]

grupo

#calculo de frecuencias usando 200ms de ventana
freq_200 <- calculo_frecuencia(0.200,
                               grupo[length(grupo)],
                               nombres_grupos,
                               do_plots = F,
                               do_plot_general = TRUE) #si TRUE genera la gráfica de eventos
umbral<-mean(freq_200[,1])*2 #selecciona el umbral

#calcula los maximos en el umbral.
maximos <- calculo_maximos_eventos(freq_200,umbral)


#el tiempo exacto del evento se calcula con las frecuencis de los disparos en sus 20 ms previos
#que se hayan producido en los 250ms previos al instante del evento calculado anteriormente.
freq_20 <- calculo_frecuencia(0.02,grupo[length(grupo)])



i=1
j=1
k=0
freq_temp <- data.frame(0,0)
max_event <- c()
while (i<=length(maximos)) {
  if (freq_20[j,2]<(maximos[i]-0.25)) {
    j=j+1
  }else{
    if (freq_20[j,2]>maximos[i]) {
      i=i+1
      k=0
      max_event <- c(max_event,freq_temp[which.max(freq_temp[,1]),2])
      freq_temp <- data.frame(0,0)
    }else{
      k=k+1
      freq_temp[k,] <- freq_20[j,]
      j=j+1
    }
  }
  
}

max_event #tiempos de los eventos.
