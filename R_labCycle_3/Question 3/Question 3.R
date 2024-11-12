# Load the dataset
data(mtcars)

# View the first few rows of the dataset to confirm the columns
head(mtcars)

# Perform Pearson correlation test between horsepower (hp) and miles per gallon (mpg)
correlation_test <- cor.test(mtcars$hp, mtcars$mpg, method = "pearson")

# Output the results
print(correlation_test)
