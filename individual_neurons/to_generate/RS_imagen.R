


correctos <- data.frame(0,0,0,0)

for (periodo in seq(150,1000,by=1)){
  for(amplitud in seq(0.001,0.50,by=0.001)){
    vector_voltajes <- -65
    reg <- -13
    a=2*pi/periodo
    b=amplitud
    t=0
    
    input <- 0
    x <- data.frame()
    tiempos <- c()
    for(i in 1:1000){
      t=t+1
      vector_voltajes=vector_voltajes+0.5*((0.04*vector_voltajes+5)*vector_voltajes+140-reg+input)
      vector_voltajes=vector_voltajes+0.5*((0.04*vector_voltajes+5)*vector_voltajes+140-reg+input)
      reg <- reg-sin((t)*(a))*(b)
      
      if (vector_voltajes>30) {
        tiempos <- c(tiempos,i)
        vector_voltajes <- -65
        }
    }
    diferencia <- tiempos[-1]-tiempos[-length(tiempos)]
    if (length(diferencia)==0){next}
    if( !(any(diferencia<=(periodo-5)) || any(diferencia>=(periodo+5) ))){
      correctos <- rbind(correctos,c(a,b,periodo,1/periodo*1000))
      
      
    }

  }  
}





plot(correctos[-1,3],correctos[-1,1],type="l")
plot(correctos[-1,3],correctos[-1,2],type="l")
colnames(correctos) <- c("a","b","period","freq(ms)")
write.csv2(correctos,file="./valors_RS.csv2")

