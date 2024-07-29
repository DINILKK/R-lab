isAgeValid <- function(age) {
  return(!is.na(as.integer(age)) && as.integer(age) >= 0)
}

isGradeValid <- function(grade) {
  return(grade %in% c("A", "B", "C", "D", "F"))
}

manageStudentRecords <- function() {
  studentData <- data.frame(Name = character(), Age = integer(), Grade = character(), stringsAsFactors = FALSE)
  
  repeat {
    cat("1. Enter Record\n")
    cat("2. Calculate Average Age\n")
    cat("3. View Details\n")
    cat("4. Quit\n")
    userChoice <- readline(prompt = 'Enter an option: ')
    userChoice <- as.integer(userChoice)
    
    if (!is.na(userChoice)) {
      if (userChoice == 1) {
        studentName <- readline(prompt = 'Enter the name of the Student: ')
        validAge <- FALSE
        while (!validAge) {
          studentAge <- as.integer(readline(prompt = 'Enter the age: '))
          if (isAgeValid(studentAge)) {
            validAge <- TRUE
          } else {
            cat("Invalid age. Age must be a positive number.\n")
          }
        }
        validGrade <- FALSE
        while (!validGrade) {
          studentGrade <- readline(prompt = 'Enter the grade: ')
          if (isGradeValid(studentGrade)) {
            validGrade <- TRUE
          } else {
            cat("Invalid grade. Grade must be one of [A, B, C, D, F].\n")
          }
        }
        studentData <- rbind(studentData, data.frame(Name = studentName, Age = studentAge, Grade = studentGrade, stringsAsFactors = FALSE))
        cat("Record added successfully.\n")
        
      } else if (userChoice == 2) {
        if (nrow(studentData) > 0) {
          averageAge <- mean(studentData$Age)
          cat("Average age of valid student records: ", averageAge, "\n")
        } else {
          cat("No valid student records available to calculate the average age.\n")
        }
        
      } else if (userChoice == 3) {
        if (nrow(studentData) > 0) {
          print(studentData)
          cat("\n")
        } else {
          cat("No student records to display.\n")
        }
        
      } else if (userChoice == 4) {
        cat("Quitting the program.\n")
        break
        
      } else {
        cat("Invalid option. Please select a valid option (1, 2, 3, or 4).\n")
      }
    } else {
      cat("Invalid input. Please enter a number.\n")
    }
  }
}

manageStudentRecords()

