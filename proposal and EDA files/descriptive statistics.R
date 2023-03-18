getwd()
setwd('DATA 583/583-project')

# Load the data from the CSV file
data <- read.csv("data_whole.csv", header = TRUE, check.names = FALSE)
dim(data) # 2857, 33

# remove y2, x6, x8, x9, x22, x29
data <- data[, -c(3, 9, 11, 12, 25, 32)]
head(data)

# remove rows with missing values
data <- na.omit(data)
dim(data) # 352, 27
head(data)

# renme y1 to y
names(data)[1] <- "y"

# output the data to a csv file
# write.csv(data, "data_cleaned.csv", row.names = FALSE)


# Calculate descriptive statistics for the response variable y1
summary(data$y1)
sd(data$y1)

# Calculate descriptive statistics for the response variable y2
summary(data$y2)
sd(data$y2)

# Calculate descriptive statistics for the explanatory variables x1 through x30
summary(data[,4:33], na.rm = TRUE)
sapply(data[,4:33], sd, na.rm = TRUE)

# Create a correlation matrix for the explanatory variables
cor(data[,4:33], use = "pairwise.complete.obs")


# Create a histogram of the response variable y1
hist(data$y1, main = "Histogram of y1")
# Create a density plot of the response variable y1
plot(density(data$y1), main = "Density plot of y1")


# Create a histogram of the response variable y2
hist(data$y2, main = "Histogram of y2")
# Create a density plot of the response variable y2
plot(density(na.omit(data$y2)), main = "Density plot of y2")


# Loop over x1 to x30 and create a histogram and a density plot for each variable
for (i in 4:33) {
  # Create a histogram of the explanatory variable
  hist(data[,i], main = paste("Histogram of", names(data)[i]),
       xlab = "Values")
  # Create a density plot of the explanatory variable
  plot(density(na.omit(data[,i])), main = paste("Density plot of",
                                                names(data)[i]))
}