library(plotly)
source("./funciones.R")
posiciones <- c(2,3,4,6,7,8)
n_op <- c(2,3,2,3,2,2)
for(num_resultados in c(129:144 )) {
  

  resultados <- data.frame("0",0,0,0,0)
  colnames(resultados) <- c("superestructura","frecuencia_eventos",
                            "numero de eventos","num_disparos_total","seed")
  ruta <- paste0(num_resultados,"_de_144")
  dir.create(paste0("./resultados/",ruta))
  
  
  fuerza <- matrix(0,3,3)
  sinapsis <- matrix(0,3,3)
  con_totales <- matrix(sample(c(5:10),3,replace = TRUE),3)
  
  vector_fuerza <- c()
  
  for(i in 1:length(posiciones)){
    if(n_op[i] ==2){opciones <- c(8,13)}else{opciones <- c(-8,-13,-18)}
    
    volt <- opciones[((num_resultados-1)%/%(144/prod(n_op[1:i])))%%n_op[i]+1]
    vector_fuerza <- c(vector_fuerza,volt)
    
  }
  fuerza[posiciones] <- vector_fuerza
  
  ciclos <- 6
  
  for (numero in 1:63) {
    
    seed=sample(1:9999,1)
    set.seed(seed)
    
    
    
    IS <- 65
    ISB <- 9
    IFB <- 26
    A <- 0
    RS <- 0
    RSB <- 0
    RFB <- 0
    
    h =runif(RFB,0.007,0.008)
    periodo=runif(RS,200,300)
    f=2*pi/periodo
    periodo2=runif(RSB,3000,4000)
    g=2*pi/periodo2 
    
    a <- c(rep(0.02,IS),rep(0.14,ISB),
           rep(0.1,IFB),rep(0.02,A),
           f,g,h)
    
    b <- c(rep(0.2,IS),
           runif(ISB,0.263,0.264),
           runif(IFB,0.249,0.251),
           rep(0.2,A),
           f*3.45/(cos(f*periodo/2*pi/180)+1),
           g*3.42/(cos(g*periodo2/2*pi/180)+1),
           h/0.5963)
    
    d <- c(rep(8,IS),runif(ISB,-8,-8),runif(IFB,-8,-7.95),rep(8,A))
    ######## tamaño final######
    cantidad_neu <- matrix(c(IS,ISB,IFB,A,RS,RSB,RFB))
    row.names(cantidad_neu) <- c("IS","ISB","IFB","A","RS","RSB","RFB")
    size <- sum(cantidad_neu)
    
    tipos <- rep(c("IS","ISB","IFB","A","RS","RSB","RFB"),cantidad_neu)
    burst <- which(tipos%in%c("IFB","ISB","RSB","RFB"))
    datos_conexiones <- data.frame(
      c("IS","ISB","IFB","A","RS","RSB","RFB"),
      cumsum(cantidad_neu))
    colnames(datos_conexiones) <- c("nombre",
                                    "numero_neurons_tipo")
    
    circuito <- matrix(data=0,nrow=size,ncol=size)
    colnames(circuito ) <- tipos
    rownames(circuito ) <- tipos
    clases <- c("IS","ISB","IFB")
    marcador <- recursiva(numero,ciclos)
    sinapsis[posiciones] <- marcador
    
    con_parcial <- round(con_totales/apply(sinapsis, MARGIN=1,sum))
    con_parcial[is.infinite(con_parcial)] <- 0
    
    for(i in 1:nrow(fuerza)){
      for(j in 1:ncol(fuerza)){
        circuito[tipos==clases[i],tipos==clases[j]] <- conexiones(cantidad_neu[clases[i],],cantidad_neu[clases[j],],
                                                                  fuerza[i,j],sinapsis[i,j]*con_parcial[i]) 
      }
    }
    
    
    
    
    
    # circuito[which(tipos=="IFB"),which(tipos=="ISB")] <- conexiones(IFB,ISB,-8,marcador[1]*3)
    # circuito[which(tipos=="IFB"),which(tipos=="IS")] <- conexiones(IFB,IS,-8,marcador[2]*3)
    # circuito[which(tipos=="ISB"),which(tipos=="IFB")] <- conexiones(ISB,IFB,8,marcador[3]*3)
    # circuito[which(tipos=="ISB"),which(tipos=="IS")] <- conexiones(ISB,IS,8,marcador[4]*3)
    # circuito[which(tipos=="IS"),which(tipos=="ISB")] <- conexiones(IS,ISB,8,marcador[5]*3)
    # circuito[which(tipos=="IS"),which(tipos=="IFB")] <- conexiones(IS,IFB,8,marcador[6]*3)
    IS_RANDOM <- sample(which(tipos=="IS"),IS/3)
    circuito[IS_RANDOM,] <- circuito[IS_RANDOM,]*(-1)
    # circuito[sample(size,size/3),] <- circuito[sample(size,size/3),]*(-1)
    ########################
    #      Simulación      #
    ########################
    
    
    ######### parámetros controlables del circuito############
    
    max_delay <- 2
    min_delay <- 2
    tiempo <- 100
    grupos_tama <- size
    
    ############  parámetros y variables internos###############
    delays <- sample(min_delay:max_delay,size,replace = TRUE)
    
    contador <- matrix(data=0,nrow=size,ncol=2)
    grupos_nume <- round(size/grupos_tama+0.4)
    volt <- rep(-65,size)
    reg <- 0.2*volt
    inputs <- rep(0, size)
    
    lim <- datos_conexiones[4,2]
    
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
      grupocorto <- vector("list",length=grupos_nume)
      sim_short_con <- c()
      sim_short_con_2 <- c()
      
      sim_short <- vector("list",length=size)
      for (i in 1:1000) {
        random <- sample(c(1:lim),1)
        inputs[random] <-inputs[random]+13 
        
        volt[1:(lim)]=volt[1:(lim)]+0.5*((0.04*volt[1:(lim)]+5)*volt[1:(lim)]+140-reg[1:(lim)]+inputs[1:(lim)])
        volt[1:(lim)]=volt[1:(lim)]+0.5*((0.04*volt[1:(lim)]+5)*volt[1:(lim)]+140-reg[1:(lim)]+inputs[1:(lim)])
        reg[1:(lim)]=reg[1:(lim)]+a[1:(lim)]*(b[1:(lim)]*volt[1:(lim)]-reg[1:(lim)]);
        

        inputs <- rep(0, size)
        
        if(any(volt>30)){
          
          disp <- which(volt>30)
          DI <- c(1:(lim))[which(c(1:(lim))%in%disp)]
          DA <- which(disp%in%list_aferentes)
          
          contador[disp,1] <-contador[disp,1]+ delays[disp]
          contador[disp,2] <- contador[disp,2]+1
          
          volt[disp]<- -65
          
          reg[DI]=reg[DI]+d[DI]
          reg[DA] <- -13
          
          for (k in 1:length(disp)) {
            
            sim_short[[disp[k]]] <- c(sim_short[[disp[k]]], (j+i/1000))
            grupocorto[[grupo_tag[disp[k]]]] <- c(grupocorto[[grupo_tag[disp[k]]]],(j+i/1000))
            if (!disp[k]%in%burst) {
              sim_short_con_2 <- c(sim_short_con_2,j+i/1000)
            }
            sim_short_con <- c(sim_short_con,j+i/1000)
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
    
    ########################
    #       eventos        #
    ########################
    
    
    freq_200 <- calculo_frecuencia(0.200,
                                   grupo[length(grupo)],
                                   do_plot_general = TRUE)
    png(paste0("./resultados/",ruta,"/",Sys.Date(),"_grupo-",num_resultados,"_circuito-",numero,".png"),
        width = 1200, height = 800)
    
    plot(freq_200[,2],freq_200[,1],type ="l")
    dev.off()
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
    
    
    superestructura <- c()
    lista_conexiones <- c("IFB-ISB","IFB-IS","ISB-IFB","ISB-IS","IS-ISB","IS-IFB")
    estructura <- lista_conexiones[which(marcador==1)]
    for (i in 1:length(estructura)) {
      superestructura <- paste(superestructura,"-(",estructura[i],")")
    }
    frecuencia_eventos <- length(max_event)/(tiempo/60)
    num_disparos_total <- length(grupo[[length(grupo)]])
    numero_eventos <- length(max_event)
    resultados <- rbind(resultados,c(superestructura,
                                     frecuencia_eventos,
                                     numero_eventos,
                                     num_disparos_total,seed))
  }
  lineas <- c(paste0("semilla: ",seed),
              "Numero de neuronas:",
              paste0("IS <- ", IS),
              paste0("ISB <- ", ISB),
              paste0("IFB <- ", IFB),
              paste0("A <- ",A),
              paste0("RS <- ",RS),
              paste0("RSB <- ",RSB),
              paste0("RFB <- ",RFB),
              paste0("delay: ",max_delay),
              paste0("tiempo: ",tiempo)
  )
  writeLines(lineas, paste0("./resultados/",num_resultados,"_de_144/datos.txt"))
  
  write.table(fuerza,paste0("./resultados/",num_resultados,"_de_144/fuerza.txt"))
  write.table(sinapsis,paste0("./resultados/",num_resultados,"_de_144/sinapsis.txt"))
  
  write.csv2(resultados,file = paste0("./resultados/",ruta,"/",Sys.Date(),"datos-",num_resultados,".csv"))
}