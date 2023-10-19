library(plotly)
source("./funciones.R")
seed <- sample(c(1:9999),1)
set.seed(4854)


circuito <- matrix(data=0,nrow=10,ncol=10)
colnames(circuito ) <- rep("RSB",10)
rownames(circuito ) <- rep("RSB",10)
circuito <-  conexiones(10,10,8,3)

datos_RSB <- read.csv2("./RSB_values.csv2")
aceptables_RSB <- which(datos_RSB[,"freq_inter"]>0.4 &datos_RSB[,"freq_inter"]<0.5 & datos_RSB[,"freq_intra.ms."]<20)

valid <- sample(aceptables_RSB,10,replace = TRUE)


a=datos_RSB[valid,"a"]
b=datos_RSB[valid,"b"]
c=datos_RSB[valid,"c"]
t=rep(0,10)




reg <- rep(-13,10)
periodo=rep(datos_RSB[valid,"period"],10)

vector_voltajes <- c

punto_medio <- reg-(b*cos(a*0)/a-b*cos(a*pi/a)/a)/2
tclave <- acos((-16-punto_medio)*a/b)/a
input <- rep(0,10)
x <- data.frame()
tiempos <- c()
for(i in 1:5000){
  # if(i==25000 ){input <- +20}else{if(i==1500){input <- 10}
  # else{input <- 0}}
  t=t+1
  vector_voltajes=vector_voltajes+0.5*((0.04*vector_voltajes+5)*vector_voltajes+140-reg+input)
  vector_voltajes=vector_voltajes+0.5*((0.04*vector_voltajes+5)*vector_voltajes+140-reg+input)
  reg <- reg-sin((t)*(a))*(b)
  input <- rep(0, 10)
  
  # print(sin((t)*(a))*(b))

  x[i,1] <- i
  x[i,2] <- vector_voltajes[5]

  if (any(vector_voltajes>30)) {
    disp <- which(vector_voltajes>30)
    if(any(disp==5)){  tiempos <- c(tiempos,i)}
    
    vector_voltajes[disp] <- -75
    if(length(disp)>1){input <- colSums(circuito[disp,])}else{if(length(disp)==1){
      input <- circuito[disp,]
    }}
    receptor <- which(input!=0)
    t[receptor] <- round(t[receptor]+(tclave[receptor]-t[receptor]%%(periodo[receptor]/2)*(input[receptor]/20)))
    reg[receptor] <- punto_medio[receptor]+b[receptor]*cos(a[receptor]*t[receptor])/a[receptor]
  }
}


tiempos <- c(-1,tiempos)
plot(data.frame(tiempos,1), type = 'o', pch = '|', ylab = '',xlim=c(1000,3000))



