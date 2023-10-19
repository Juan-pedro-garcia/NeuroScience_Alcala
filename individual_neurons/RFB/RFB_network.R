library(plotly)
source("./funciones.R")
set.seed(sample(c(1:9999),1))
datos_RFB <- read.csv2("./RFB_values.csv2")
aceptables_RFB <- which(datos_RFB[,"freq_inter"]>2 & datos_RFB[,"freq_inter"]<9 &datos_RFB[,"freq_intra.ms."]>100)


circuito <- matrix(data=0,nrow=10,ncol=10)
colnames(circuito ) <- rep("RFB",10)
rownames(circuito ) <- rep("RFB",10)
circuito <-  conexiones(10,10,2,7)

# circuito[sample(c(1:100),60)] <- circuito[sample(c(1:100),60)]*-1


valid <- sample(aceptables_RFB,10,replace = TRUE)


a=datos_RFB[valid,"a"]
b=datos_RFB[valid,"b"]
c=datos_RFB[valid,"c"]
t=rep(0,10)




reg <- rep(-13,10)
periodo=rep(datos_RFB[valid,"period"],10)

vector_voltajes <- c
punto_medio <- reg-(b*cos(a*0)/a-b*cos(a*pi/a)/a)/2
tclave <- acos((-16-punto_medio)*a/b)/a
input <- rep(0,10)
x <- data.frame()
tiempos <- c()
for(i in 1:2000){
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
    
    vector_voltajes[disp] <- c[disp]
    if(length(disp)>1){input <- colSums(circuito[disp,])}else{if(length(disp)==1){
      input <- circuito[disp,]
    }}
    receptor <- which(input!=0)
    t[receptor] <- round(t[receptor]+(tclave[receptor]-t[receptor]%%(periodo[receptor]/2)*(input[receptor]/20)))
    reg[receptor] <- punto_medio[receptor]+b[receptor]*cos(a[receptor]*t[receptor])/a[receptor]
  } 
}

plot_ly(x, x=~V1, y=~V2, type ="scatter", mode = 'lines')
plot(x[,1],x[,2],type = "l")

plot(data.frame(tiempos,2), type = 'o', pch = '|',cex=1, ylab = '',ylim=c(1,3))


