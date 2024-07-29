passGenerator <- function(passLength) {
  if (passLength < 8) {
    return("The length should be at least 8 characters")
  }
  
  lowerChars <- letters
  upperChars <- toupper(letters)
  numericChars <- as.character(0:9)
  specialChars <- strsplit("@#$&*!", "")[[1]]
  
  # Ensure password contains at least one character from each category
  password <- c(
    sample(lowerChars, 1),
    sample(upperChars, 1),
    sample(numericChars, 1),
    sample(specialChars, 1)
  )
  
  allChars <- c(lowerChars, upperChars, numericChars, specialChars)
  password <- c(password, sample(allChars, passLength - 4, replace = TRUE))
  
  # Shuffle the password characters and collapse them into a single string
  password <- sample(password)
  return(paste(password, collapse = ""))
}

repeat {
  lengthInput <- readline(prompt = 'Enter the length of the password: ')
  passLength <- as.integer(lengthInput)
  
  if (!is.na(passLength) && passLength >= 8) {
    generatedPassword <- passGenerator(passLength)
    cat("Generated password is: ", generatedPassword, "\n")
    break
  } else {
    cat("Invalid input. The length should be at least 8 characters.\n")
  }
}
