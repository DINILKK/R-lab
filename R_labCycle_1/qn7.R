generateFibonacci <- function() {
  range <- as.integer(readline(prompt = "Enter the range of numbers to generate: "))
  
  if (!is.na(range) && range > 0) {
    if (range == 1) {
      fibonacci <- c(1)
    } else if (range == 2) {
      fibonacci <- c(1, 1)
    } else if (range == 3) {
      fibonacci <- c(1, 1, 1)
    } else {
      fibonacci <- c(1, 1, 1)
      for (i in 4:range) {
        nextNumber <- fibonacci[i-1] + fibonacci[i-2] + fibonacci[i-3]
        fibonacci <- append(fibonacci, nextNumber)
      }
    }
    cat("The series is:", fibonacci, "\n")
  } else {
    cat("Invalid input. Please enter a positive integer.\n")
  }
}

generateFibonacci()
