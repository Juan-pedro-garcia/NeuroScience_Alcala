
####funciones

conexiones <- function( pres,post, fuerza,nc){ 
  #pres <- int; numero de neuronas presinapticas(emisoras del impulso)
  #post <- int; numero de neuronas postsinapticas(receptoras del impulso)
  #fuerza <- int; voltaje trasmitido a la neurona postsinaptica
  #nc <- int; número de neuronas presinapticas conectadas a cada neurona postisnaptica
  matriz <- matrix(0,nrow=pres,ncol=post)
  nc_pre <- nc*pres                                         #cuantas conexiones reciben las postsinapticas en total
  con_total <- round(runif(1,nc_pre-1,nc_pre+1))           #varía el numero de conexiones para aumenar la variabilidad entre circuitos.
  con_post <- rep(con_total%/%post,post)                    #cuantas conexiones reciben las presinapticas en total
  con_post[sample(post,con_total%%post)] <- con_post[sample(post,con_total%%post)]+1
  i=0
  while (any(con_post>0)) {                                  #va estableciendo conexiones de una en una de forma que sean homogéneas
    if(i==pres){i=0}
    i=i+1
    
    if(length(which(con_post>0))>1){s <- sample(which(con_post>0),1)
    }else{s <- which(con_post>0)}
    matriz[i,s] <- fuerza
    con_post[s] <- con_post[s]-1
  }
  return <- matriz
}


#calcula la frecuencia de disparos de un grupo determinado de neuronas en un espacio de tiempo (bin) 
#se cuentan los disparos en el "bin" previo a cada disparo de cada una de las neuronas del grupo y se divide entre
#el "bin" o entre el tiempo hasta el primer disparo dentro del "bin" si este se produce e un tiempo menor de bin/2
calculo_frecuencia<- function(bin=0.200,              #bin <- float; tiempo previo al disparo que se usa para calcular la frecuencia
                              grupo,                  #grupo <- lista; lista con el tiempo de disparo de cada neurona el grupo.
                              nombres_grupos=NULL,    #nombres_grupo <- vector; lps nombres de las neuronas que entran en el grupo
                              do_plots=FALSE,         #do_plots <- boolean; si TRUE genera las gráficas de frecuencia de los grupos de neuronas.
                              do_plot_general=FALSE){ #do_plots_general <- boolean; si TRUE genera las gráficas de frecuencia de todas las neuronas.
  
  plots <- list()
  for (h in 1:(length(grupo))) {
    if (length(grupo[[h]])==1) {next}                  #saltar esa neurona si no ha disprado ninguna vez.
    conj_dispa <- c(grupo[[h]])
    frame_freq_todo <- data.frame()
    
    n <- (ceiling(length(conj_dispa)/1000))            #divide el proceso en bloques de 1000 disparo para reducir el uso de memoria
    
    for (i in 0:(n-1)) {
      print(i)
      frame_freq_peq <- data.frame()
      l=0
      if (length(conj_dispa)<1000) {m <- 2:length(conj_dispa)}else{
        if (i!=(n-1)&i!=0){m <- (i*1000+1):((i+1)*1000)}else{
          if (i==0) {m <- 2:1000}else{
            m <-(i*1000+1):(length(conj_dispa))}}} #seleccion el bloque de 1000 disparos.
      for(k in m ){#recorre todos los disparos del intervalo actual calculando la frecuencia en el bin anterior a cada disparo
        l=l+1
        if (conj_dispa[k]==conj_dispa[k-1]) {next}    #si dos disparos se producen a al vez la frecuencia será la misma, asi que salta.
        j=k
        t=0
        while((conj_dispa[k]-bin)<=conj_dispa[j]){
          j=j-1                                        #j da la numeración del primer disparo en el bin anterior al disparo K.
          t=t+1   
        }
        if((conj_dispa[k]-conj_dispa[k-t+1])>(bin/2)){ #formula de calculo de frecuencia.
          freq <- (t-1)/((conj_dispa[k]-conj_dispa[k-t+1]))
        }else{freq <- t/(bin)}
        
        frame_freq_peq[l,1] <- freq
        frame_freq_peq[l,2] <- conj_dispa[k]*1000
      }
      frame_freq_todo <- rbind(frame_freq_todo,frame_freq_peq)
    }
    frame_freq_todo[,2] <- frame_freq_todo[,2]/1000
    frame_freq_todo <- frame_freq_todo[which(!is.na(frame_freq_todo[,1])),]
    if(isTRUE(do_plots) & h<length(grupo)){
      plots[[h]] <- plot_ly(frame_freq_todo, x=~V2, y=~V1, type ="scatter",
                            mode = 'lines',name=nombres_grupos[h])
    }
  }
  if(isTRUE(do_plots)){print(subplot(plots,nrows = length(plots)))}
  if(isTRUE(do_plot_general)){print(plot_ly(frame_freq_todo,x=~V2,y=~V1,type ="scatter",
                                            mode="lines", name="todo"))}
  return(frame_freq_todo)#devuelve un dataframe de dos columnas: 
  #la primera con la frecuencia de disparos 
  #la segunda con el tiempo de cada disparo.
}

#busca el valor máxino dentro de cada evento sincrónico desde la frecuencia de disparos.
calculo_maximos_eventos <- function(frame_freq_todo,limite){  #limite <- float; umbral a partir del cual se considera que se produce un evento.
  binario <- 0
  superior <- c()
  tiempo <- c()
  
  i=1
  max_event <- c(0)
  while (i<(nrow(frame_freq_todo))) {
    superior <- c()
    tiempo <- c()
    
    while(frame_freq_todo[i,1]>limite & i<(nrow(frame_freq_todo))){#cuando supera el umbral se guarda la frecuencia y el tiempo, 
      superior <- c(superior,frame_freq_todo[i,1])                  #cuando es inferior sale del bucle
      tiempo <- c(tiempo,frame_freq_todo[i,2])
      binario <- 1
      i=i+1
    }
    if(binario==1){                                 #cuando sale del bucle calcula el máximo del bloque de frecuencias que superaba el umbral
      max_event <-c(max_event,tiempo[which.max(superior)])
    }
    binario <- 0                                    #evita que calcule el maximo sin entrar en el bucle.
    i=i+1
  }
  
  max_event2 <- c()
  i=1
  j=0
  while (j<(length(max_event))) { #elimina los eventos si ha habido un evento en los 0'5s anteriores.
    j=j+1
    if ((max_event[i]+0.5)<max_event[j]){
      max_event2 <- c(max_event2,max_event[i])
      i=j
    }
    
  }
  if ((max_event[i]+0.5)<max_event[j] | i==j) {max_event2<- c(max_event2,max_event[i])}
  return(max_event2[-1])
}

#calcula cuando se producen los disparos de las neuronas respecto al momento en el que se produce un evento sincrónico
disparo_medio<- function(max_event,list_neu){       #max_event <- vector; vector con los instantes en los que se han producido los eventos
  lista_ratio <- list()                             #list_neu <- lista; lista con los disparos de cada neurona.
  lista_temporal <- list()
  matriz_Neu <- cbind(max_event,matrix(0,length(max_event),length(list_neu)))
  for (k in 1:length(list_neu)) {
    
    lista_ratio <- c()
    disp_Neu <- c()
    Neu <- final[,list_neu[k]]
    Neu <- Neu[which(!is.na(Neu))]
    
    t=0
    i=1
    j=1
    while (i<length(Neu) & j<=length(max_event)) {    #busca los disparos de las neuronas en un rango de mas-menos 0.15
      #seg respecto al evento
      t=t+1
      if (Neu[i]<(max_event[j]-0.15)) {i=i+1
      }else{ 
        if (Neu[i]>(max_event[j]-0.15) & Neu[i]<max_event[j]+0.15) {
          disp_Neu <- c(disp_Neu,Neu[i])
          estandar2 <- c(estandar2,(Neu[i]-max_event[j]))
          matriz_Neu[j,k+1] <- 1
          i=i+1
        }else{j=j+1}
      }
    }
    lista_temporal[[k]] <- disp_Neu
    lista_ratio[[k]] <- estandar2
    
  }
  return(lista_ratio)                                   #lista con los disparos de cada neurona que se encuentran en un intervalo de mas-menos 0.15
}                                                       # se guardan la diferencia entre el instante del evento y el disparo dentro del rango 


#cuenta cuantos disparos de cada neurona se han producido entre mas-menos 0.15 segundos del evento
#agrupados según el intervalo elegido
disparos_acumulados <- function(lista_ratio,intervalo=0.005){
  
  m <- cbind(round(seq(-0.15,0.15,intervalo),digits = 3),0)
  n <- data.frame(rep(0,nrow(m)))
  row.names(n) <- m[,1]
  for (i in 1:length(lista_ratio)) {#redondea los disparos y cuenta cuantos hay de cada intervalo.
    if (is.null(lista_ratio[[i]])) {
      n[,i] <- rep(0,nrow(m))
      next}
    disp_ventana <- lista_ratio[[i]]
    x <- round(as.numeric(row.names(as.matrix(table(round((disp_ventana),digit=3)))))/intervalo)*intervalo
    y <- as.matrix(table(round(unlist(disp_ventana),digit=3)))
    s<- which(rownames(n)%in%as.data.frame(table(rep(x,y))/size)[,1])
    n[,i] <- rep(0,nrow(m))
    n[s,i] <- as.data.frame(table(rep(x,y))/size)[,2]
  }
  medias <- apply(n,1,mean)
  
  return(medias)
}

#calcula la media de disparos de cada grupo neuronal respecto al evento sincrónico
ratio_neu_evento <- function(vector_num_neu,              #vector_num_neu <- vector; vector con la numeración de neuronas que se 
                             eventos,intervalo){              #van a usar para calcular la media
  lista_ratio <- disparo_medio(eventos,vector_num_neu)    #eventos <- vector; vector con el tiempo en que ha sucedido cada evento
  disp_ratio<- disparos_acumulados(lista_ratio,intervalo) #intervalo <- float; intervalo en el que se van a juntar los disparos respecto al tiempo del evento
  disp_ratio <- as.data.frame(disp_ratio)
  return(disp_ratio)
}



#evaluaciones

#calculo de frecuencia media
mean_freq <- function(temp){  #temp <- vector; tiempo de los disparos de una neurona
  media <- length(temp)/temp[length(temp)]
  return(media)
}

#calculo de Coeficiente de Variación
cal_CV <- function(temp){
  diff <- temp[-1]-temp[-length(temp)]
  CV <- sd(diff)/mean(diff)
  return(CV)
}

#calculo de la frecuencia media entre burst
mean_interburst_freq <- function(diff,x){         #diff <- vector, diferencia de tiempo entre cada disparo y su disparo siguiente
  Mean_interburst_freq <- mean(1/diff[x!=x[1]])   #x <- vector; clasificación de los disparos. 1 son los disparos que no están en un burst
  return(Mean_interburst_freq)                   # 2 son los disparos dentro de un burst.
}

#calculo de la frecuencia media dentro de los burst
mean_intraburst_freq <- function(diff,x){
  Mean_intraburst_freq <- mean(1/diff[x==x[1]])
  return(Mean_intraburst_freq)
}


#calculo de Coeficiente de Variación de los disparos dentro de un burst
CV_intraburst <- function(diff,x){
  CV_intraburst <- sd(1/diff[x==2])/mean(1/diff[x==2])
  return(CV_intraburst)
}

#calculo de Coeficiente de Variación de los disparos entre los burst
cal_CV_burst <- function(diff,x){
  CV_interburst <- sd(1/diff[x==1])/mean(1/diff[x==1])
  return(CV_interburst)
}



# función para unir de forma ordenada los disparos de varias neuronas en grupos neuronales
union <- function(grupos,nombres_grupos,nombres_select){ #grupo <- lista; lista con el tiempo de disparo de cada neurona del grupo.
  x <- which(nombres_grupos%in%nombres_select)           #nombres_grupo <- vector; los nombres de las neuronas que entran en el grupo
  grupo2 <- list()                                      #nomnres_select <- vector; los nombres de los grupos neuronales que se van a generar.
  for(i in 1:length(x)){
    grupo2[[i]] <- grupo[[x[i]]]
  }
  total <- grupo2[[1]]
  total_temp <- c()
  
  for (t in (length(grupo2)-1)) {
    y <- grupo2[[t+1]]
    j=1
    i=1
    while(length(total)>i & length(y)>j){
      if(total[i]>=y[j]){
        total_temp <- c(total_temp,total[i])
        j=j+1}else{
          total_temp <- c(total_temp,y[j])
          i=i+1}
    }
    total <- total_temp
  }
  grupo2[[length(grupo2)+1]] <- total
  return(grupo2)
}


recursiva <- function(numero,ciclos){
  binario <- c()
  while(ciclos>0){
    ciclos=ciclos-1
    binario <- c(binario,numero%%(2))
    numero <- numero%/%2
  }
  return(binario)
}


