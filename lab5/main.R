# Title     : TODO
# Objective : TODO
# Created by: Костя
# Created on: 02.12.2020
dt <- 0
first_mas <- matrix(dt, 5, 5)
second_mas <- matrix(dt, 10, 10)

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

buff <- function(mas, row, col){
  buf <- matrix(0, row, col)
  for (i in 1 : nrow(mas)){
    for (j in 1 : ncol(mas)){
      buf[i,j] <- mas[i,j]
    }
  }
  return(buf)
}

Addition <- function (mas1, mas2){
  for (i in 1 : nrow(mas1)){
    for (j in 1 : ncol(mas1)){
      mas1[i,j] <- mas1[i,j] + mas2[i,j]
    }
  }
  return(mas1)
}

subtraction <- function (mas1, mas2){
  for (i in 1 : nrow(mas1)){
    for (j in 1 : ncol(mas1)){
      mas1[i,j] <- mas1[i,j] - mas2[i,j]
    }
  }
  return(mas1)
}

multiplication <- function (mas1, mas2){
  for (i in 1 : nrow(mas1)){
    for (j in 1 : ncol(mas1)){
      mas1[i,j] <- mas1[i,j] * mas2[i,j]
    }
  }
  return(mas1)
}

first_mas <- m1(first_mas)
second_mas <- m2(second_mas)

if (nrow(first_mas) > nrow(second_mas) && ncol(first_mas) > ncol(second_mas)){
  F1 <- matrix( , nrow(first_mas), ncol(first_mas))
  F1 <- buff(second_mas, nrow(first_mas), ncol(first_mas))
  F1 <- Addition(F1, first_mas)
} else if (nrow(first_mas) < nrow(second_mas) && ncol(first_mas) < ncol(second_mas)) {
  F1 <- matrix( , nrow(first_mas), ncol(first_mas))
  F1 <- buff(first_mas, nrow(second_mas), ncol(second_mas))
  F1 <- Addition(F1, second_mas)
} else {
  print("Количество строк и столбцов совпадает")
  F1 <- Addition(m1(first_mas), m2(second_mas))
}

print(first_mas)
print(second_mas)
print(F1)

first_mas <- buff(first_mas, nrow(second_mas), ncol(second_mas))

F2 <- subtraction(second_mas, first_mas)
print(F2)

F3 <- multiplication(second_mas, first_mas)
print(F3)

F4 <- multiplication(first_mas, second_mas)
print(F4)

