data("airquality")
aq = airquality

# 1
# a. Use a loop to perform the counting
# Initialize counter
na_count <- 0
# Loop through each row of the Ozone column
for (i in 1:nrow(aq)) {
  if (is.na(aq$Ozone[i])) { 
    na_count <- na_count + 1
  }
}
na_count

# b. Perform the same counting without using any loop
sum(is.na(aq$Ozone))

# 2
# Convert Fahrenheit to Celsius and store in new column
aq$Temp_C <- (aq$Temp - 32) / 1.8
# Compute mean Celsius temperature for Month = 7 (July), ignoring NAs
mean_july_temp <- mean(aq$Temp_C[aq$Month == 7], na.rm = TRUE)
mean_july_temp

# 3
?tapply
# I got help from Microsoft Copilot to find out tapply function.
# I looked for ways to compute monthly maximum values without using a loop.

# Compute monthly maximum Ozone values for months 5 to 9
monthly_max <- tapply(aq$Ozone, aq$Month, max, na.rm = TRUE)
# Extract only May–September (months 5 to 9)
selected_max <- monthly_max[as.character(5:9)]
print(selected_max)
# Sum the monthly maximum values
sum_max <- sum(selected_max)
sum_max

# 4
# The function that checks how many entries in a numeric vector are less than 0 or greater than 20
check_range <- function(x) {
  sum(x < 0 | x > 20, na.rm = TRUE) # counts how many are TRUE while ignoring missing values
}
# Apply this function to the Wind column
count_wind <- check_range(aq$Wind)
count_wind