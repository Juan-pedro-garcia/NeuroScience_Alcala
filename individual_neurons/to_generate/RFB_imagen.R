correctos <- data.frame(0,0,0,0,0,0,0)
contador <- 0
for(d in seq(-50,-65,by=-1)){
for (periodo in seq(50,1000,by=1)){
  for(amplitud in seq(0.001,0.50,by=0.001)){
    vector_voltajes <- d
    reg <- -13
    w=2*pi/periodo
    a=w
    b=amplitud
    t=0
    
    input <- 0
    x <- data.frame()
    tiempos <- c()
    for(i in 1:2000){
      t=t+1
      vector_voltajes=vector_voltajes+0.5*((0.04*vector_voltajes+5)*vector_voltajes+140-reg+input)
      vector_voltajes=vector_voltajes+0.5*((0.04*vector_voltajes+5)*vector_voltajes+140-reg+input)
      reg <- reg-sin((t)*(a))*(b)
      
      if (vector_voltajes>30) {
        tiempos <- c(tiempos,i)
        vector_voltajes <- d
      }
    }
    if(length(tiempos)==0){next}
    #primera condicion
    if(any(tiempos[1:4]-tiempos[1]==periodo)){contador <- contador+1}else{next}
    
    #segunda condicion
    rafaga <- which((tiempos[1:4]-tiempos[1]==periodo))
    if(rafaga<3){contador=0
      next}
    frec_intra <- tiempos[2:(rafaga-1)]-tiempos[1:(rafaga-2)]
    frec_intra <- 1/(mean(frec_intra)/1000)
    if(frec_intra<130 & frec_intra>90){contador <- contador+1}
    
    if(contador==2){correctos <- rbind(correctos,c(a,b,periodo,rafaga,frec_intra,d,1/periodo*1000))}
    contador <- 0
      
      
    }
    
  }  


}

plot(correctos[-1,3],correctos[-1,1],type="l")
plot(correctos[-1,2],correctos[-1,3],type="p")
colnames(correctos) <- c("a","b","period","number_spikes","freq_intra.ms.","c","freq_inter")

write.csv2(correctos,file="./RFB_valors.csv2")
