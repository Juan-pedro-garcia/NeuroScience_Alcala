library(plotly)

datos_RSB <- read.csv2("./RSB_values.csv2")
neuron_num <- 200

a=datos_RSB[neuron_num,"a"]
b=datos_RSB[neuron_num,"b"]
c=datos_RSB[neuron_num,"c"]

volt <- c
periodo <- datos_RSB[neuron_num,"period"]
reg <- -13

t=0

punto_medio <- reg-(b*cos(a*0)/a-b*cos(a*pi/a)/a)/2
tclave <- acos((-16-punto_medio)*a/b)/a
input <- 0
tiempos <- c()
x <- data.frame()
for(i in 1:4000){
  # if(i==1500){input <- 20}else{input <- 0}}
  t=t+1
  volt=volt+0.5*((0.04*volt+5)*volt+140-reg+input)
  volt=volt+0.5*((0.04*volt+5)*volt+140-reg+input)
  reg <- reg-sin((t)*(a))*(b)
  # print(sin((t)*(a))*(b))
  
  x[i,1] <- i
  x[i,2] <- volt
  # x[i,3] <- reg
  # x[i,4] <- sin((t)*(a))*(b)
  
  if (volt>30) {
    volt <- c
  }

}
# plot_ly(x, x=~V1, y=~V3, type ="scatter", mode = 'lines')
# plot_ly(x, x=~V1, y=~V4, type ="scatter", mode = 'lines')
# plot_ly(x, x=~V1, y=~V2, type ="scatter", mode = 'lines')
plot(x[,1],x[,2],type = "l",xlab="period (ms)",ylab="Volt(mV)")


