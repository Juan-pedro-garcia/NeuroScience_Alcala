library(plotly)
vector_voltajes <- -65
reg <- -13
periodo=170
a=0.014
b=0.014/0.6
w=2*pi/periodo
A=w*3.45/(cos(w*periodo/2*pi/180)+1)
a=datos_RFB[3293,2]
b=datos_RFB[3293,3]
t=0

punto_medio <- reg-(b*cos(a*0)/a-b*cos(a*pi/a)/a)/2
tclave <- acos((-16-punto_medio)*a/b)/a
input <- 0
x <- data.frame()
tiempos <- c()
for(i in 1:500){
  # if(i==25000 ){input <- +20}else{if(i==1500){input <- 10}
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
    vector_voltajes <- datos_RFB[3293,7]
  }
  if(input!=0){
    print(t)
    t <- round(t+(tclave-t%%(periodo/2)*(input/20)))
    print(t)
    reg <- punto_medio+b*cos(a*t)/a
    print(reg)
  }
}
plot_ly(x, x=~V1, y=~V3, type ="scatter", mode = 'lines',colors="#000000")
plot_ly(x, x=~V1, y=~V4, type ="scatter", mode = 'lines')

plot_ly(x, x=~V1, y=~V2, type ="scatter", mode = 'lines')
plot(x[,1],x[,2],type = "l",xlab="period (ms)",ylab="Volt(mV)")
length(tiempos)/(i/1000)
