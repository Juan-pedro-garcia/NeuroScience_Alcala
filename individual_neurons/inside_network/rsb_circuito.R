library(plotly)
set.seed(sample(c(1:9999),1))


vector_voltajes <- rep(-75,10)
reg <- rep(-13,10)

circuito <- matrix(data=0,nrow=10,ncol=10)
colnames(circuito ) <- rep("RSB",10)
rownames(circuito ) <- rep("RSB",10)
circuito <-  conexiones(10,10,10,2)

datos_RSB <- read.csv2("./valores_RSB.csv2")
aceptables_RSB <- which(datos_RSB[,7]>0.4 &datos_RSB[,7]<0.5 & datos_RSB[,6]<20)
g <- sample(aceptables_RSB,10,replace = TRUE)


a=datos_RSB[g,2]
b=datos_RSB[g,3]
c <- rep(-75,10)
t=rep(0,10)

punto_medio <- reg-(b*cos(a*0)/a-b*cos(a*pi/a)/a)/2
tclave <- acos((-16-punto_medio)*a/b)/a
input <- rep(0,10)
x <- data.frame()
tiempos <- c()
for(i in 1:3000){
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

# plot_ly(x, x=~V1, y=~V2, type ="scatter", mode = 'lines')
# plot(x[,1],x[,2],type = "l")
# 
# df <- data.frame(c(1:10000), rep(0,10000))
# tiempos
# i=1
# j=1
# while(i<=10000){
#   i=i+1
#   if (df[i,1]==tiempos[j]) {
#     df[i,2] <- 5000
#     j=j+1
#   }
# }
# 
# plot(df[,1],df[,2],type="l")
tiempos <- c(-1,tiempos,2000)
plot(data.frame(tiempos,1), type = 'o', pch = '|', ylab = '',xlim=c(1000,3000))



