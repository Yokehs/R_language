# Title     : TODO
# Objective : TODO
# Created by: Костя
# Created on: 04.12.2020

first_mas <- matrix( , 5, 5)
second_mas <- matrix( , 10, 10)

m1 <- function(mas){
  #наполнение 1 массива
  k <- -1
  for (i in 1 : nrow(mas)){
    k <- k + 4
    for (j in 1 : ncol(mas)){
      mas[i,j] <- k + j
    }
  }
  return(mas)
}

m2 <- function (mas){
  #наполнение 2 массива
  k <- 0
  for (i in nrow(mas) : 1){
    for (j in ncol(mas) : 1){
      k <- k + 1
      mas[j,i] <- k
    }
  }
  return(mas)
}

addrows <- function (mas, N_rows){
  for(i in 1 : N_rows){
    mas <- rbind(mas, 0)
  }
  return(mas)
}

addcols <- function (mas, N_cols){
  for(i in 1 : N_cols){
    mas <- cbind(mas, 0)
  }
  return(mas)
}

Addition <- function (mas1, mas2){
  for (i in 1 : nrow(mas1)){
    for (j in 1 : ncol(mas1)){
      mas1[i,j] <- mas1[i,j] + mas2[i,j]
    }
  }
  return(mas1)
}

Subtraction <- function (mas1, mas2){
  for (i in 1 : nrow(mas1)){
    for (j in 1 : ncol(mas1)){
      mas1[i,j] <- mas1[i,j] - mas2[i,j]
    }
  }
  return(mas1)
}

Multiplication <- function (mas1, mas2){
  mas <- matrix(,nrow(mas1),ncol(mas1))
  for (i in 1 : nrow(mas1)){
    for (j in 1 : ncol(mas1)){
      mas[i,j] = 0
      for (k in 1 : nrow(mas1)){
        mas[i,j] <- mas[i,j] + mas1[i,k] * mas2[k,j]
      }
    }
  }
  return(mas)
}



InMas <- function (collection){
  if(sqrt(length(collection)) %%2 == 0 || sqrt(length(collection)) %%2 == 1){
    mas <- matrix(, sqrt(length(collection)), sqrt(length(collection)))
    k <- length(collection)
    for (i in 1 : sqrt(length(collection))){
      for (j in 1 : ncol(mas)){
        mas[i,j] <- collection[k]
        k <- k - 1
      }
    }
    return(mas)
  } else {
    mas <- matrix(, sqrt(length(collection)) + 1, sqrt(length(collection)) + 1)
    k <- length(collection)
    for (i in 1 : nrow(mas)){
      for (j in 1 : ncol(mas)){
        if (k > 0){
          mas[i,j] <- collection[k]
          k <- k - 1
        }else{
          mas[i,j] <- NA
        }
      }
    }
    return(mas)
  }
}

mas1 <- m1(first_mas)
mas2 <- m2(second_mas)

mas3 <- addrows(mas1, 5)
mas3 <- addcols(mas3, 5)
print(mas3)
F1 <- Addition(mas3, mas2)
print(F1)

F2 <- Subtraction(mas2, mas3)
print(F2)

F3 <- mas2 * mas3
print(F3)


F4 <- Multiplication(mas2, mas3)
print("f4:")
print(F4)

F5 <- c(addrows(mas1, 1), addcols(mas2,1))
print(F5)
InMas(F5)