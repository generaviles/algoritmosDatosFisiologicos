#%%%%%%LOGÍSTICA%%%%%%%%%%%%%%%%%%%%%

logist <- function(o,x) {
  1/(1+exp(x[,1:4] %*% t(o)))
}


#%%%%%%%%%%%%%%%Familia S-Shaped%%%%%%%%%%%%%%%%%%%%%%%%%%

#S1
s1 <- function(x,o){
  1/(1+exp(x %*% (-2 * t(o))))
}

#S2
s2 <- function(x,o){
  1/(1+exp(-1*o*x))
}

#S3
s3 <- function(x,o){
  1/(1+exp((-1*o*x)/2))
}

#S4
s4 <- function(x,o){
  1/(1+exp((-1*o*x)/3))
}

#%%%%%%%%%%%%%%%Familia V-Shaped%%%%%%%%%%%%%%%%%%%%%%%%%%

#V1
#Primero hay que crear la funcion para el error matematico:

v1<- function(x){
  library(pracma)
  abs(erf((sqrt(pi)/2)*x))
}

#V2
v2 <- function(x){
  abs(tanh(x))
}

#V3
v3 <- function(x){
  abs((x)/(sqrt(1+x^2)))
}

#V4
v4 <- function(x){
  abs((2/pi)*atan((pi/2)*x))
}


#%%%%%%%%%%%%%%%%%%%%%% Integración de Todas en Única Función %%%%%%%%%%%%%%%%%

perceptron<-function(x,o,fun){
    switch(fun,
         s1 = 1/(1+exp(x %*% (-2*t(o)))),
         s2 = 1/(1+exp(x %*% (-1*t(o)))),
         s3 = 1/(1+exp((x %*% (-1*t(o))))/2),
         s4 = 1/(1+exp((x %*% (-1*t(o)))/3)),
         v1 = abs(erf((sqrt(pi)/2)*x %*% t(o))),
         v2 = abs(tanh(x %*% t(o))),
         v3 = abs((x %*% t(o))/(sqrt(1+x^2 %*% t(o)))),
         v4 = abs((2/pi)*atan((pi/2)*x %*% t(o)))
         
  )
}
