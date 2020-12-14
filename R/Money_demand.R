#' some money demand variables for Mexico
#'
#' @format  364 x 5 data frame
#'
#' \describe
#' \item{index}{Monetary Aggregate}
#' \item{column}
#' }

"Money_demand"

Money_demand <- read.csv("C://Users//marco//Desktop//UCLA//441A Applied Data Managment//Final project//Rproject//uclanomics//Money_Demand.csv", header=TRUE)

usethis::use_data(Money_demand, overwrite = TRUE)


