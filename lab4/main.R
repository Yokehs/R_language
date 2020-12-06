# Title     : TODO
# Objective : TODO
# Created by: Костя
# Created on: 30.11.2020
R <- function (str, delimeter){
  return(paste(unlist(strsplit(str, ' ')), collapse = delimeter))
}
R("Мы все договорились о встрече", "-")