x<-1

options(scipen = 999)#quitar la notaci�n cient�fica, par�metro 0 para ponerla
a<-exp(0)+(exp(0)*x)
for(i in 2:10){
  a<-a+(exp(0)/factorial(i))*(x^i)
  print(a)
}