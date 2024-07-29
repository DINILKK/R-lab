checkPrime <- function(number) {
  if (number <= 1) {
    return(FALSE)
  }
  if (number == 2) {
    return(TRUE)
  }
  if (number %% 2 == 0) {
    return(FALSE)
  }
  
  sqrt_n <- floor(sqrt(number))
  for (i in seq(3, sqrt_n, by = 2)) {
    if (number %% i == 0) {
      return(FALSE)
    }
  }
  
  return(TRUE)
}

generatePrime <- function(maxRange) {
  primes <- c()
  
  if (maxRange >= 2) {
    for (i in 2:maxRange) {
      if (checkPrime(i)) {
        primes <- append(primes, i)
      }
    }
  }
  
  return(primes)
}

main <- function() {
  repeat {
    cat("1. Check Prime\n")
    cat("2. Generate a Series of Primes\n")
    cat("3. Quit\n")
    userChoice <- as.integer(readline(prompt = "Choose an option: "))
    
    if (!is.na(userChoice)) {
      if (userChoice == 1) {
        num <- as.integer(readline(prompt = "Enter a number to check: "))
        if (!is.na(num)) {
          if (checkPrime(num)) {
            cat(num, "is a prime number.\n")
          } else {
            cat(num, "is not a prime number.\n")
          }
        } else {
          cat("Invalid input. Please enter a valid integer.\n")
        }
        
      } else if (userChoice == 2) {
        range <- as.integer(readline(prompt = "Enter the range to generate primes up to: "))
        if (!is.na(range) && range > 1) {
          primes <- generatePrime(range)
          cat("Prime numbers up to", range, "are:", primes, "\n")
        } else {
          cat("Invalid input. Please enter a valid integer greater than 1.\n")
        }
        
      } else if (userChoice == 3) {
        cat("Quitting...\n")
        break
        
      } else {
        cat("Invalid option. Please choose 1, 2, or 3.\n")
      }
    } else {
      cat("Invalid input. Please enter a number.\n")
    }
  }
}

main()
