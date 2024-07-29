applyCaesarCipher <- function(inputText, shiftAmount) {
  cipheredText <- ""
  
  for (charIndex in 1:nchar(inputText)) {
    currentChar <- substr(inputText, charIndex, charIndex)
    if (grepl("[A-Za-z]", currentChar)) {
      asciiOffset <- ifelse(currentChar >= 'A' & currentChar <= 'Z', 65, 97)
      shiftedChar <- intToUtf8((utf8ToInt(currentChar) - asciiOffset + shiftAmount) %% 26 + asciiOffset)
      cipheredText <- paste0(cipheredText, shiftedChar)
    } else {
      cipheredText <- paste0(cipheredText, currentChar)
    }
  }
  
  return(cipheredText)
}

repeat {
  cat('1: Cipher Text\n')
  cat('2: Quit\n')
  
  userChoice <- as.integer(readline(prompt = 'Choose one option: '))
  if (userChoice == 1) {
    textInput <- readline(prompt = 'Enter the text: ')
    shiftValue <- as.integer(readline(prompt = 'Enter the shift value: '))
    cipheredResult <- applyCaesarCipher(textInput, shiftValue)
    cat('Ciphered text is: ', cipheredResult, '\n')
  } else if (userChoice == 2) {
    cat('Thank you\n')
    break
  } else {
    cat('Invalid option\n')
  }
}
