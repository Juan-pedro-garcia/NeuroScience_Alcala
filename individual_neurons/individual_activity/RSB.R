library(plotly)

datos_RSB <- read.csv2("./RSB_valors.csv2")
aceptables_RSB <- which(datos_RSB[,"freq_inter"]>0.4 & datos_RSB[,"freq_inter"]<0.5 &datos_RSB[,"freq_intra.ms."]<20)

a=datos_RSB[200,"a"]
b=datos_RSB[200,"b"]
c=datos_RSB[200,"c"]

vector_voltajes <- c
periodo <- datos_RSB[200,"period"]
reg <- -13

t=0

punto_medio <- reg-(b*cos(a*0)/a-b*cos(a*pi/a)/a)/2
tclave <- acos((-16-punto_medio)*a/b)/a
input <- 0
tiempos <- c()
x <- data.frame()
for(i in 1:4000){
  # if(i==25000 ){input <- +20}else{if(i==1500){input <- 20}
  # else{input <- 0}}
  t=t+1
  vector_voltajes=vector_voltajes+0.5*((0.04*vector_voltajes+5)*vector_voltajes+140-reg+input)
  vector_voltajes=vector_voltajes+0.5*((0.04*vector_voltajes+5)*vector_voltajes+140-reg+input)
  reg <- reg-sin((t)*(a))*(b)
  # print(sin((t)*(a))*(b))
  
  x[i,1] <- i
  x[i,2] <- vector_voltajes
  x[i,3] <- reg
  x[i,4] <- sin((t)*(a))*(b)
  
  if (vector_voltajes>30) {
    tiempos <- c(tiempos,i)
    vector_voltajes <- c
  }

}
plot_ly(x, x=~V1, y=~V3, type ="scatter", mode = 'lines')
plot_ly(x, x=~V1, y=~V4, type ="scatter", mode = 'lines')

plot_ly(x, x=~V1, y=~V2, type ="scatter", mode = 'lines')
plot(x[,1],x[,2],type = "l",xlab="period (ms)",ylab="Volt(mV)")


