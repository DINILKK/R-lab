# Load the dataset
data(mtcars)

# Convert gear to a factor since it's a categorical variable
mtcars$gear <- as.factor(mtcars$gear)

# Perform one-way ANOVA to test if there are significant differences in displacement (disp) across gear types
anova_test <- aov(disp ~ gear, data = mtcars)

# Output the ANOVA summary
summary(anova_test)
