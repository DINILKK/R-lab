calculateCharLength <- function(wordList) {
  charLengths <- numeric(length(wordList))
  for (index in seq_along(wordList)) {
    charLengths[index] <- nchar(wordList[index])
  }
  return(charLengths)
}

calculateMean <- function(numList) {
  totalSum <- sum(numList)
  return(totalSum / length(numList))
}

findLongestWord <- function(wordList) {
  maxLength <- 0
  longestWord <- NULL
  for (word in wordList) {
    if (nchar(word) > maxLength) {
      maxLength <- nchar(word)
      longestWord <- word
    }
  }
  return(longestWord)
}

substituteWord <- function(targetWord, newWord, textList) {
  upperTarget <- toupper(targetWord)
  for (index in seq_along(textList)) {
    upperText <- toupper(textList[index])
    if (upperTarget == upperText) {
      textList[index] <- newWord
    }
  }
  return(textList)
}

processInputText <- function() {
  userInput <- readline(prompt = "Enter the text: ")
  words <- unlist(strsplit(userInput, "\\s+"))
  
  continue <- TRUE
  while (continue) {
    cat("1: Total number of words\n")
    cat("2: Average word length\n")
    cat("3: Longest word\n")
    cat("4: Replace specific word\n")
    cat("5: Change text\n")
    cat("6: Quit\n")
    cat("Choose one: ")
    choice <- scan(what = integer(), nmax = 1, quiet = TRUE)
    
    if (choice == 1) {
      cat("Total number of words = ", length(words), "\n")
    } else if (choice == 2) {
      wordLengths <- calculateCharLength(words)
      cat("Average word length = ", calculateMean(wordLengths), "\n")
    } else if (choice == 3) {
      cat("Longest Word is = ", findLongestWord(words), "\n")
    } else if (choice == 4) {
      oldWord <- readline(prompt = "Enter the word to replace: ")
      newWord <- readline(prompt = "Enter the new word to replace with: ")
      newText <- substituteWord(oldWord, newWord, words)
      cat("After replacement, text is: ", paste(newText, collapse = " "), "\n")
    } else if (choice == 5) {
      userInput <- readline(prompt = "Enter the new text: ")
      words <- unlist(strsplit(userInput, "\\s+"))
    } else if (choice == 6) {
      cat("Thank you\n")
      break
    } else {
      cat("Invalid choice\n")
    }
  }
}

processInputText()
