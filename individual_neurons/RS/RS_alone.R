library(plotly)
volt <- -65
reg <- -13

datos_RS <- read.csv2("./RS_values.csv2")

neuron_num <- 2 

a=datos_RS[neuron_num,2]
b=datos_RS[neuron_num,3]
periodo=datos_RS[neuron_num,4]
t=0

punto_medio <- reg-(b*cos(a*0)/a-b*cos(a*pi/a)/a)/2
tclave <- acos((-16-punto_medio)*a/b)/a
input <- 0
x <- data.frame()
tiempos <- c()
for(i in 1:500){
  
  # if(i==225 ){input <- +20} else {input <- 0}}    #uncomment to create a input. 
  t=t+1
  volt=volt+0.5*((0.04*volt+5)*volt+140-reg+input)
  volt=volt+0.5*((0.04*volt+5)*volt+140-reg+input)
  reg <- reg-sin((t)*(a))*(b)
  x[i,1] <- i
  x[i,2] <- volt
  # x[i,3] <- reg                                 test
  # x[i,4] <- sin((t)*(a))*(b)                    test
  
  if (volt>30) {
    tiempos <- c(tiempos,i)
    volt <- -65
  }
  if(input!=0){
    print(t)
    t <- round(t+(tclave-t%%(periodo/2)*(input/20)))
    print(t)
    reg <- punto_medio+b*cos(a*t)/a
    print(reg)
  }
}
# plot_ly(x, x=~V1, y=~V3, type ="scatter", mode = 'lines')                test
# plot_ly(x, x=~V1, y=~V4, type ="scatter", mode = 'lines')                test
# plot_ly(x, x=~V1, y=~V2, type ="scatter", mode = 'lines')                test
plot(x[,1],x[,2],type = "l",xlab="period (ms)",ylab="Volt(mV)")

