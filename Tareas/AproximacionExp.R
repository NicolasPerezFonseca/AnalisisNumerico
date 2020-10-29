x<-1

options(scipen = 999)#quitar la notación científica, parámetro 0 para ponerla
a<-exp(0)+(exp(0)*x)
for(i in 2:10){
  a<-a+(exp(0)/factorial(i))*(x^i)
  print(a)
}