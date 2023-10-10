library(plotly)
source("./funciones.R")

set.seed(sample(c(1:9999),1))

datos_RS <- read.csv2("./RS_valors.csv2")
aceptables_RS <- which(datos_RS[,"freq.ms."]>5 &datos_RS[,"freq.ms."]<6)


circuito <- matrix(data=0,nrow=10,ncol=10)
colnames(circuito ) <- rep("RS",10)
rownames(circuito ) <- rep("RS",10)
circuito <-  conexiones(10,10,-13,3)

valid <- sample(aceptables_RS,10,replace = TRUE)


vector_voltajes <- rep(-65,10)
reg <- rep(-13,10)
periodo=rep(datos_RS[valid,"period"],10)
a=datos_RS[valid,"a"]
b=datos_RS[valid,"b"]
c=rep(-65,10)
t=rep(0,10)

punto_medio <- reg-(b*cos(a*0)/a-b*cos(a*pi/a)/a)/2
tclave <- acos((-16-punto_medio)*a/b)/a
input <- rep(0,10)
x <- data.frame()
tiempos <- c()
for(i in 1:10000){
  # if(i==25000 ){input <- +20}else{if(i==1500){input <- 10}
  # else{input <- 0}}
  t=t+1
  vector_voltajes=vector_voltajes+0.5*((0.04*vector_voltajes+5)*vector_voltajes+140-reg+input)
  vector_voltajes=vector_voltajes+0.5*((0.04*vector_voltajes+5)*vector_voltajes+140-reg+input)
  reg <- reg-sin((t)*(a))*(b)
  
  # input <- rep(0, 10)
  x[i,1] <- i
  x[i,2] <- vector_voltajes[5]

  if (any(vector_voltajes>30)) {
    disp <- which(vector_voltajes>30)
    if(any(disp==5)){  tiempos <- c(tiempos,i)}
  
    vector_voltajes[disp] <- -65
    if(length(disp)>1){input <- colSums(circuito[disp,])}else{if(length(disp)==1){
      input <- circuito[disp,]
    }}

    t[disp] <- round(t[disp]+(tclave[disp]-t[disp]%%(periodo[disp]/2)*(input[disp]/20)))
    reg[disp] <- punto_medio[disp]+b[disp]*cos(a[disp]*t[disp])/a[disp]
  }
}

plot_ly(x, x=~V1, y=~V2, type ="scatter", mode = 'lines')
plot(x[,1],x[,2],type = "l")
plot(data.frame(tiempos,1), type = 'o', pch = '|', ylab = '')
