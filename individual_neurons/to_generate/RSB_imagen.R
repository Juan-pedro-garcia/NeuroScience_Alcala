correctos <- data.frame(0,0,0,0,0,0)
l=0
contador <- 0

for (d in seq(-68,-75,by=-1)) {
  
  
  
  for (periodo in seq(1800 ,2200,by=1)){
    l=l+1
    print(l)
    for(amplitud in seq(0.004,0.0068,by=0.0001)){
      contador <- 0
      vector_voltajes <- d
      reg <- -13
      w=2*pi/periodo
      a=w
      b=amplitud
      t=0
      
      input <- 0
      x <- data.frame()
      tiempos <- c()
      for(i in 1:4000){
        t=t+1
        vector_voltajes=vector_voltajes+0.5*((0.04*vector_voltajes+5)*vector_voltajes+140-reg+input)
        vector_voltajes=vector_voltajes+0.5*((0.04*vector_voltajes+5)*vector_voltajes+140-reg+input)
        reg <- reg-sin((t)*(a))*(b)
        x[i,1] <- i
        x[i,2] <- vector_voltajes
        if (vector_voltajes>30) {
          tiempos <- c(tiempos,i)
          vector_voltajes <- d
        }
      }
      if(length(tiempos)==0){next}
      #primera condicion
      if(any(tiempos[1:length(tiempos)]-tiempos[1]==periodo)){contador <- contador+1}else{next}
      
      #segunda condicion
      rafaga <- which((tiempos[1:length(tiempos)]-tiempos[1]==periodo))
      if(rafaga<5){contador=0
      next}
      frec_intra <- tiempos[2:(rafaga-1)]-tiempos[1:(rafaga-2)]
      frec_intra <- 1/(mean(frec_intra)/1000)
      if(frec_intra<25 & frec_intra>12){contador <- contador+1}
      
      if(contador==2){correctos <- rbind(correctos,c(a,b,periodo,rafaga,frec_intra,d))}
    }
  }
  
  
}
plot(correctos[-1,3],correctos[-1,1],type="l")
plot(correctos[-1,3],correctos[-1,2],type="l")
write.csv2(correctos,file="./valores_RSB4.csv2")





