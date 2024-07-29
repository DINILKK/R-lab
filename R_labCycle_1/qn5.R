seriesSum <- function(term) {
  sum <- 1
  denominator <- 3
  if (term == 1) {
    return(1)
  } else {
    for (i in 2:term) {
      sum <- sum + ((-1)^(i-1)) * i / denominator
      denominator <- denominator + 2
    }
  }
  return(sum)
}

main <- function() {
  termInput <- readline(prompt = "Enter the number of terms to calculate: ")
  term <- as.integer(termInput)
  if (!is.na(term) && term > 0) {
    cat("The sum is: ", seriesSum(term), "\n")
  } else {
    cat("Invalid input. Please enter a positive integer.\n")
  }
}

main()
