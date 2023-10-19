library(plotly)

datos_RFB <- read.csv2("./RFB_values.csv2")

a=datos_RFB[200,"a"]
b=datos_RFB[200,"b"]
c=datos_RFB[200,"c"]

periodo <- datos_RFB[200,"period"]
vector_voltajes <- c
reg <- -13


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
  x[i,1] <- i
  x[i,2] <- vector_voltajes

  
  if (vector_voltajes>30) {
    tiempos <- c(tiempos,i)
    vector_voltajes <- c
  }
  if(input!=0){
    t <- round(t+(tclave-t%%(periodo/2)*(input/20)))
    reg <- punto_medio+b*cos(a*t)/a
    }
}

plot_ly(x, x=~V1, y=~V2, type ="scatter", mode = 'lines')
plot(x[,1],x[,2],type="l",xlab="period (ms)",ylab="Volt(mV)")
plot(x[,1],x[,2],type = "l")


