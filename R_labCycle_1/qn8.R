checkPalindrome <- function() {
  string <- readline(prompt = "Enter the string to check: ")
  string <- tolower(gsub("\\s+", "", string))
  reversedString <- tolower(paste(rev(strsplit(string, "")[[1]]), collapse = ""))
  
  if (string == reversedString) {
    cat("The given string is a palindrome.\n")
  } else {
    cat("The given string is not a palindrome.\n")
  }
}

checkPalindrome()
