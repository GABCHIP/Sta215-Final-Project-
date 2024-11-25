# Set Working Directory setwd(H:/"sta215")
setwd("H:/sta215")

# Install "haven" package 
install.packages("haven")

# Load "haven" package 
library(haven)
library(psych)
library(dplyr)

# Load Chipelo_final_project as an object called "data"
chipelo_final_project <- read.csv("chipelo_final_project.csv")

data <- chipelo_final_project %>%
  filter(complete.cases(.))

# Create a contingency table
contingency_table <- table(data$gender, data$grammy)
print(contingency_table)

chisq.test(data$gender, data$grammy)

# descriptive
mean(data$gender)
sd(data$gender)
table(data$gender)
describe(data$gender)
summary(data$gender)
min(data$gender)
max(data$gender)
describe(data$gender)

# descriptive
mean(data$song_length)
sd(data$song_length)
min(data$song_length)
max(data$song_length)
describe(data$song_length)

# descriptive
mean(data$use_in_media)
sd(data$use_in_media)
min(data$use_in_media)
max(data$use_in_media)
describe(data$use_in_media)

# descriptive
mean(data$pop)
sd(data$pop)
min(data$pop)
max(data$pop)
describe(data$pop)

# figure 1
lm(time_signature ~ bpm, data=data)
aov(time_signature ~ bpm, data=data)
summary(time_signature ~ bpm, data=data)
boxplot(time_signature ~ bpm, data=data)

# Figure 2
plot(album_sales ~ song_length, data=data)
linear_regression <- lm(album_sales ~ song_length, data=data)
summary(linear_regression) 
summary(album_sales ~ song_length, data=data)
abline(linear_regression)
mean_length <- mean(data$song_length)
mean_album_sales <- mean(data$album_sales)
abline(v = mean_length, col = "purple")
abline(h = mean_album_sales, col = "green")

hist(residuals(linear_regression))
# Figure 3
plot(data$song_length, residuals(linear_regression))
abline(h=0, col = "red")