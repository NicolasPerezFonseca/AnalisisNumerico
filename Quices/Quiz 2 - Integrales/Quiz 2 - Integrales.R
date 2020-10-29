#Función a Integrar
f <- function(x) {
  return(sqrt(1+(cos(x))^2))
}

#Forma alternativa según Wolfram de la derivada Cuarta de la función a Integrar:

d4 <- function(x){
  return((1704*cos(2*x)-100*cos(4*x)+24*cos(6*x)+cos(8*x)+931)/(8*sqrt(2)*(cos(2*x)+3)^(7/2)))
}


#Con base en la fórmula del error por Simpson calculamos el n (cantidad de intervalos)
#necesarios para cumplir con la tolerancia 1e-4.
e <- function(d4,a,b){
  n=0
  for (i in 1:100) {
    n=((b-a)^5)/((180*(i^4))*d4(1))
       
    if(n<=1e-4){
      return(i)
    }
  }
  return(100)
}

# a: límite superior
# b:límite inferior
# f: función a integrar
composite.simpson <- function(f, a, b, n) {
  if (is.function(f) == FALSE) {
    stop('f must be a function with one parameter (variable)')
  }
  
  h <- (b - a) / n
  
  xj <- seq.int(a, b, length.out = n + 1)
  xj <- xj[-1]
  xj <- xj[-length(xj)]
  
  approx <- (h / 3) * (f(a) + 2 * sum(f(xj[seq.int(2, length(xj), 2)])) + 4 * sum(f(xj[seq.int(1, length(xj), 2)])) + f(b))
  
  return(approx)
  
}

n=e(d4,0,2)
composite.simpson(f,0,2,n)