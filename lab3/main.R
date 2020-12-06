# Title     : TODO
# Objective : TODO
# Created by: Костя
# Created on: 28.11.2020

x <- c(0.53, 0.98)
N <- c(6, 7)
fun <- function (x, N, y, a){
  N = N - 1
  print(N)
  y <- y + (cos(x) + a) / a ^ 2
  print(y)
  a = a + 1

  if(N == 0){
    return(0)
  }else {
    return(fun(x, N, y, a))
  }

}
for (i in 1:2){
  fun(x[i], N[i], 0, 1)
  print("--------------------")
}