library(ggplot2)
library(corrplot)
library(GGally)
library(tidyverse)          # Load the tidyverse package
library("dplyr")
#library(dplyr)
library(tidyr)
library(stringr)
#library('ggplot2')
library(SmartEDA)
library(DataExplorer)
library(dlookr)
plotCorrelationMatrix <- function(df, graphWidth) {
  dfNumeric <- df[sapply(df, is.numeric)]
  dfNumeric <- dfNumeric[, sapply(dfNumeric, sd) != 0]  # Exclude columns with zero standard deviation
  
  if (ncol(dfNumeric) < 2) {
    cat("No correlation plots shown: The number of non-NaN or constant numeric columns (", ncol(dfNumeric), ") is less than 2\n")
    return(NULL)
  }
  
  corr <- cor(dfNumeric)
  corrplot(corr, method = "color", tl.col = "black", tl.srt = 90, addCoef.col = "black", number.cex = 0.7, order = "hclust", tl.cex = 0.7)
}


# Function to plot per column distribution
plotPerColumnDistribution <- function(df, nGraphShown, nGraphPerRow) {
  df <- df[sapply(df, function(col) length(unique(col)) > 1 & length(unique(col)) < 50)]
  nCol <- ncol(df)
  columnNames <- colnames(df)
  nGraphRow <- ceiling(nCol / nGraphPerRow)
  
  par(mfrow = c(nGraphRow, nGraphPerRow), mar = c(4, 4, 2, 1))
  
  for (i in 1:min(nCol, nGraphShown)) {
    columnData <- df[, i]
    if (!is.numeric(columnData[1])) {
      valueCounts <- table(columnData)
      barplot(valueCounts, main = paste(columnNames[i], "(column", i, ")", sep = " "), col = "skyblue", las = 2)
    } else {
      hist(columnData, main = paste(columnNames[i], "(column", i, ")", sep = " "), col = "lightblue", xlab = "", ylab = "counts")
    }
  }
}


# Function to plot correlation matrix
plotCorrelationMatrix <- function(df, graphWidth) {
  df <- df[complete.cases(df), ]
  df <- df[sapply(df, function(col) length(unique(col)) > 1)]
  
  if (ncol(df) < 2) {
    cat("No correlation plots shown: The number of non-NaN or constant columns (", ncol(df), ") is less than 2\n")
    return(NULL)
  }
  
  corr <- cor(df)
  corrplot(corr, method = "color", tl.col = "black", tl.srt = 90, addCoef.col = "black", number.cex = 0.7, order = "hclust", tl.cex = 0.7)
}


plotScatterMatrix <- function(df, plotSize, textSize) {
  numericCols <- sapply(df, is.numeric)
  dfNumeric <- df[, numericCols]
  dfNumeric <- dfNumeric[sapply(dfNumeric, function(col) length(unique(col)) > 1)]
  
  columnNames <- colnames(dfNumeric)
  if (length(columnNames) > 10) {
    columnNames <- columnNames[1:10]
  }
  dfNumeric <- dfNumeric[, columnNames]
  
  ggpairs(dfNumeric, columns = columnNames, axisLabels = "internal", title = "Scatter and Density Plot") +
    theme(text = element_text(size = textSize))
}



accident <- read.csv('C:\\Users\\Srinidhi\\Downloads\\archive (3)\\Accident_Information.csv', header = TRUE, nrows = 1000)
View(accident)
vehicle <- read.csv('C:\\Users\\Srinidhi\\Downloads\\archive (3)\\Vehicle_Information.csv', header = TRUE, nrows = 1000)

# Display information about the first dataframe
cat("Accident_Information.csv\n")
cat("There are", nrow(accident), "rows and", ncol(accident), "columns\n")

# Display information about the second dataframe
cat("Vehicle_Information.csv\n")
cat("There are", nrow(vehicle), "rows and", ncol(vehicle), "columns\n")

# Plot distributions for the first dataframe
plotPerColumnDistribution(accident, 10, 5)



# Plot scatter matrix for the first dataframe
plotScatterMatrix(accident, 20, 10)

# Plot distributions for the second dataframe
plotPerColumnDistribution(vehicle, 10, 5)



# Plot scatter matrix for the second dataframe
plotScatterMatrix(vehicle, 9, 10)






#Load the vehicleset
# Use the correct file path with either forward slashes or double backslashes
#vehicle <- read.csv("C:\\Users\\Srinidhi\\Downloads\\archive (3)\\Vehicle_Information.csv")
#vehicle
#View(vehicle)

#accident <- read.csv("C:\\Users\\Srinidhi\\Downloads\\archive (3)\\Accident_Information.csv")
#accident

#cat("Column Names: ", colnames(vehicle), "\n")

# Assuming 'Vehicle_Information' is your vehicleset
# Replace this with the actual name of your vehicleset

# Load necessary libraries


diagnose_web_report(vehicle)
diagnose_web_report(accident)

all_column_names <- names(vehicle)
all_column_names1 <- names(accident)


column_names <- names(vehicle)
print(column_names)

introduce(vehicle) %>% t() 

plot_intro(vehicle)


plot_bar(vehicle)

plot_histogram(vehicle)

plot_density(vehicle)

colSums(is.na(vehicle)) 


column_names1 <- names(accident)
print(column_names1)

introduce(accident) %>% t() 

plot_intro(accident)


plot_bar(accident)

plot_histogram(accident)

plot_density(accident)

colSums(is.na(accident))

# Function to calculate mode



mean1 <- round(mean(vehicle$Driver_IMD_Decile, na.rm = TRUE))
vehicle$Driver_IMD_Decile[is.na(vehicle$Driver_IMD_Decile)] <- mean1

mean2 <- round(mean(vehicle$Age_of_Vehicle, na.rm = TRUE))
vehicle$Age_of_Vehicle[is.na(vehicle$Age_of_Vehicle)] <- mean2



mean3 <- round(mean(vehicle$ Engine_Capacity_.CC. , na.rm = TRUE))
vehicle$ Engine_Capacity_.CC. [is.na(vehicle$ Engine_Capacity_.CC. )] <- mean3

library(DescTools)
# Mode function definition
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

View(vehicle)
# Replace NaN values with mode
mode_value <- Mode(vehicle$Propulsion_Code[!is.nan(vehicle$Propulsion_Code)])
vehicle$Propulsion_Code[is.nan(vehicle$Propulsion_Code)] <- mode_value

# Install and load the necessary libraries if not already installed
# install.packages(c("dplyr", "DescTools"))
# Install and load the necessary libraries if not already installed
# install.packages(c("dplyr", "DescTools"))
# Install and load the necessary libraries if not already installed
# install.packages(c("dplyr", "DescTools"))
# Install and load the necessary libraries if not already installed
# install.packages(c("dplyr", "DescTools"))
library(dplyr)
library(DescTools)

# Define the Mode function
Mode <- function(x, na.rm = FALSE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Assuming 'vehicle' is your data frame
vehicle <- vehicle %>%
  mutate_all(~ if (is.numeric(.)) ifelse(is.na(.), mean(., na.rm = TRUE), .) else ifelse(is.na(.), Mode(., na.rm = TRUE), .))

accident <- accident %>%
  mutate_all(~ if (is.numeric(.)) ifelse(is.na(.), mean(., na.rm = TRUE), .) else ifelse(is.na(.), Mode(., na.rm = TRUE), .))

# Create a new column 'Age_Midpoint' by taking the midpoint of the range
vehicle$Age_Midpoint <- sapply(strsplit(vehicle$Age_Band_of_Driver, " - "), function(x) mean(as.numeric(x)))
hist(vehicle$Age_Midpoint)





mean5 <- round(mean(vehicle$Vehicle_Location.Restricted_Lane , na.rm = TRUE))
vehicle$Vehicle_Location.Restricted_Lane [is.na(vehicle$Vehicle_Location.Restricted_Lane )] <- mean5
colSums(is.na(vehicle)) 

mean6 <- round(mean(vehicle$Age_Midpoint , na.rm = TRUE))
vehicle$Age_Midpoint [is.na(vehicle$Age_Midpoint )] <- mean6
colSums(is.na(vehicle)) 
colSums(is.na(accident)) 


# Replace values below a certain threshold with NA
#vehicle$Journey_Purpose_of_Driver[vehicle$Journey_Purpose_of_Driver < lower_threshold] <- NA

# Replace values above a certain threshold with NA
#vehicle$Journey_Purpose_of_Driver[vehicle$Journey_Purpose_of_Driver > upper_threshold] <- NA


vehicle <- vehicle %>%
  mutate(
    Vehicle_Age_Group = case_when(
      Age_of_Vehicle <= 5 ~ 'New',
      between(Age_of_Vehicle, 6, 10) ~ 'Moderately Old',
      TRUE ~ 'Old'
    ),
    Engine_Power_Category = case_when(
      Engine_Capacity_.CC. < 1500 ~ 'Small',
      between(Engine_Capacity_.CC., 1500, 2500) ~ 'Medium',
      TRUE ~ 'Large'
    ),
    Vehicle_Age_in_2023 = 2023 - Year  # Assuming the current year is 2023
    # Add more numerical features based on your requirements
  )

vehicle <- vehicle %>%
  mutate(
    Driver_Young_Adult = as.integer(Age_Band_of_Driver == 'Young Adult'),
    Driver_Middle_Aged = as.integer(Age_Band_of_Driver == 'Middle Aged'),
    Driver_Senior = as.integer(Age_Band_of_Driver == 'Senior'),
    Driver_Urban_Area = as.integer(Driver_Home_Area_Type == 'Urban'),
    Driver_Rural_Area = as.integer(Driver_Home_Area_Type == 'Rural'),
    Driver_Commute = as.integer(Journey_Purpose_of_Driver == 'Commute'),
    Driver_Leisure = as.integer(Journey_Purpose_of_Driver == 'Leisure'),
    Fuel_Petrol = as.integer(Propulsion_Code == 'Petrol'),
    Fuel_Diesel = as.integer(Propulsion_Code == 'Diesel'),
    Skid_Occurred = as.integer(Skidding_and_Overturning != 'None'),
    Towing_Vehicle = as.integer(Towing_and_Articulation == 'Towing'),
    Articulated_Vehicle = as.integer(Towing_and_Articulation == 'Articulated'),
    Lane_Change = as.integer(Vehicle_Manoeuvre == 'Change Lane'),
    Turning = as.integer(Vehicle_Manoeuvre == 'Turn')
    # Add more categorical features following a similar pattern
  )


hist(vehicle$Driver_Young_Adult)

hist(vehicle$Driver_Middle_Aged)
hist(vehicle$Driver_Senior)
hist(vehicle$Skid_Occurred)
hist(vehicle$Driver_Urban_Area)
hist(vehicle$Driver_Rural_Area)



# Iterate through each column and replace NA values with mean (if numeric)
for (col in names(vehicle)) {
  if (is.numeric(vehicle[[col]])) {
    mean_value <- mean(vehicle[[col]], na.rm = TRUE)
    vehicle[[col]][is.na(vehicle[[col]])] <- mean_value
  }
}


# Assuming 'vehicle' is your vehicleframe

# Vehicle_Type
vehicle <- vehicle %>%
  mutate(
    Car_Type = as.integer(Vehicle_Type == 'Car'),
    Truck_Type = as.integer(Vehicle_Type == 'Truck'),
    Motorcycle_Type = as.integer(Vehicle_Type == 'Motorcycle')
    # Add more vehicle type categories as needed
  )

# Sex_of_Driver
vehicle <- vehicle %>%
  mutate(
    Male_Driver = as.integer(Sex_of_Driver == 'Male'),
    Female_Driver = as.integer(Sex_of_Driver == 'Female')
    # Add more gender categories as needed
  )
dev.off()

hist(vehicle$Male_Driver)

hist(vehicle$Female_Driver)



# X1st_Point_of_Impact
vehicle <- vehicle %>%
  mutate(
    Front_Impact = as.integer(X1st_Point_of_Impact == 'Front'),
    Rear_Impact = as.integer(X1st_Point_of_Impact == 'Rear'),
    Side_Impact = as.integer(X1st_Point_of_Impact == 'Side')
    # Add more impact categories as needed
  )

# Junction_Location
vehicle <- vehicle %>%
  mutate(
    Junction_Inside = as.integer(Junction_Location == 'Inside'),
    Junction_Outside = as.integer(Junction_Location == 'Outside')
    # Add more junction location categories as needed
  )

# Vehicle_Reference
vehicle <- vehicle %>%
  mutate(
    Vehicle_Count = n_distinct(Vehicle_Reference)  # Count of vehicles involved in the accident
    # Add more numerical features based on the 'Vehicle_Reference' column if applicable
  )

# Vehicle_Location.Restricted_Lane
vehicle <- vehicle %>%
  mutate(
    Restricted_Lane = as.integer(Vehicle_Location.Restricted_Lane == 'Yes')
    # Add more binary features based on 'Vehicle_Location.Restricted_Lane' if needed
  )


column_names <- names(vehicle)
print(column_names)


# Assuming 'vehicle' is the name of your dataset
library(tidyverse)



# Selecting relevant features
selected_features <- vehicle %>%
  select(
    Age_Band_of_Driver,
    Engine_Capacity_.CC.,
    Journey_Purpose_of_Driver,
    Junction_Location,
    Vehicle_Type,
    Skidding_and_Overturning,
    Vehicle_Leaving_Carriageway
  )

# Summary statistics
summary(selected_features)

# Visualizing data
# Example: Bar plot for Vehicle Types
vehicle %>%
  ggplot(aes(x = Vehicle_Type)) +
  geom_bar() +
  labs(title = "Distribution of Vehicle Types in Accidents", x = "Vehicle Type", y = "Count")

# Example: Box plot for Engine Capacity
vehicle %>%
  ggplot(aes(x = "Engine_Capacity_.CC.", y = Engine_Capacity_.CC.)) +
  geom_boxplot() +
  labs(title = "Distribution of Engine Capacities in Accidents", x = "Engine Capacity", y = "CC")

vehicle %>%
  ggplot(aes(x = "Age_of_Vehicle", y = Age_of_Vehicle)) +
  geom_boxplot() +
  labs(title = "Distribution of Age of Vehicle  in Accidents", x = "Age_of_Vehicle", y = "frequency")

vehicle %>%
  ggplot(aes(x = "Year", y = Year)) +
  geom_boxplot() +
  labs(title = "Distribution of year in Accidents", x = "Year", y = "frequency")

vehicle %>%
  ggplot(aes(x = "Driver_IMD_Decile", y = Driver_IMD_Decile)) +
  geom_boxplot() +
  labs(title = "Distribution of Driver IMD Decile in Accidents", x = " driver imd", y = "frequency")

vehicle %>%
  ggplot(aes(x = "Vehicle_Reference", y = Vehicle_Reference)) +
  geom_boxplot() +
  labs(title = "Distribution of Vehicle Reference in Accidents", x = "Vehicle_Reference", y = "frequency")



# Add more visualizations based on your selected features

vehicle %>%
  ggplot(aes(x = Journey_Purpose_of_Driver, fill = Journey_Purpose_of_Driver)) +
  geom_bar() +
  labs(title = "Distribution of Journey Purposes in Accidents", x = "Journey Purpose", y = "Count") +
  theme(legend.position = "none")

# Example: Box plot for Engine Capacity by Vehicle Type
vehicle %>%
  ggplot(aes(x = Vehicle_Type, y = Engine_Capacity_.CC., fill = Vehicle_Type)) +
  geom_boxplot() +
  labs(title = "Engine Capacity Distribution by Vehicle Type", x = "Vehicle Type", y = "Engine Capacity (CC)")

vehicle %>%
  ggplot(aes(x = Vehicle_Type, y = Year, fill = Vehicle_Type)) +
  geom_boxplot() +
  labs(title = "Engine Capacity Distribution by Vehicle Type", x = "Vehicle Type", y = "Year")

vehicle %>%
  ggplot(aes(x = Year, y = Engine_Capacity_.CC., fill = Year)) +
  geom_boxplot() +
  labs(title = "Engine Capacity Distribution by year ", x = "Year", y = "Engine Capacity (CC)")



plot_boxplot(vehicle, by = "Age_Band_of_Driver")

ExpNumViz(vehicle, target = "Age_Band_of_Driver", Page = c(2,2))

#View(vehicle)




# Assuming your data frame is named 'vehicle'
# Replace 'vehicle' with your actual data frame name

# Create a new variable 'Road_Condition' based on related attributes
vehicle$Road_Condition <- NA

# Infer road conditions based on relevant attributes
# Assuming "vehicle" is your data frame

# Replace "Road_Condition" with "good" where conditions are met
vehicle$Road_Condition[
  vehicle$Hit_Object_in_Carriageway == "None" &
    vehicle$Vehicle_Location.Restricted_Lane == "0" &
    vehicle$Skid_Occurred == "0"
] <- "good"

# Assign a different value when conditions are not met
vehicle$Road_Condition[!(vehicle$Hit_Object_in_Carriageway == "None" &
                           vehicle$Vehicle_Location.Restricted_Lane == "0" &
                           vehicle$Skid_Occurred == "0")] <- "not good"


# Add more conditions as needed

# Check the unique values in the newly created 'Road_Condition' variable
unique(vehicle$Road_Condition)

condition <- table(vehicle$Road_Condition)
pie(condition, 
    main = "Pie Chart of Accident Severity", 
    col = rainbow(length(condition)), 
    labels = names(condition))
# Explore and analyze the data based on inferred road conditions
summary(vehicle$Road_Condition)
# Perform further analysis and visualizations as needed

# Example: Count of accidents based on inferred road conditions
library(ggplot2)
ggplot(vehicle, aes(x = Road_Condition)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Accidents Based on Inferred Road Conditions",
       x = "Road Condition",
       y = "Count")


# Assuming 'vehicle' is your data frame


# Print the updated data frame
print(vehicle)


# Assuming "vehicle" is your data frame
# Convert "Skidding_and_Overturning" to integers
vehicle$Skidding_and_Overturning <- as.integer(vehicle$Skidding_and_Overturning == "Skidded")

vehicle$Road_Condition_num <- as.integer(vehicle$Road_Condition == "good")
# Assuming you have a dataframe called vehicle

# Define weights for each attribute (adjust as needed)
# Assuming "vehicle" is your data frame
attribute_weights <- c(
  Driver_Age = 0.2,
  Road_Conditions = 0.2,
  Skid_Occurred = 1
)

# Create a new column for overall performance
vehicle$overall_performance <- rowSums(
  vehicle[, c("Age_Midpoint", "Road_Condition_num", "Skid_Occurred")] * attribute_weights,
  na.rm = TRUE
)


# Display histogram of overall performance
hist(vehicle$overall_performance, main = "Overall Performance Histogram", xlab = "Overall Performance")

# Assuming 'vehicle' is your data frame



# Install and load the plotly library




plotPerColumnDistribution(vehicle, 10, 5)


#ACCIDENT INFO

# Assuming 'accident' is your data frame and 'Date' is the column containing dates
accident$Date <- as.Date(accident$Date, format = "%Y-%m-%d")

accident$Month <- format(accident$Date, "%b")  # Extract month
accident$Day <- weekdays(accident$Date)  # Extract day of the week
accident$Year <- format(accident$Date, "%Y")  # Extract year

# Remove the original 'Date' column if needed
accident <- accident[, !names(accident) %in% c("Date")]

# Assuming 'accident' is your data frame and 'Time' is the column containing times


summary(accident)

cor_matrix1 <- cor(accident[, sapply(accident, is.numeric)], use = "complete.obs")
print(cor_matrix1)

# Increase the device size
#par(mar = c(0,0,0,0) + 0)

# Bar plot of Accident Severity
barplot(table(accident$Accident_Severity), 
        main = "Accident Severity", 
        xlab = "Severity", 
        ylab = "Count", 
        col = "skyblue")

# Assuming 'Accident' is your data frame and 'Latitude' and 'Longitude' are the columns of interest
plot(accident$Longitude, accident$Latitude, pch = 16, col = "blue", main = "Accident Locations", xlab = "Longitude", ylab = "Latitude")



# Sample data for illustration purposes
set.seed(123)
accident <- data.frame(
  X1st_Road_Class = sample(1:3, 100, replace = TRUE),
  Accident_Severity = sample(1:3, 100, replace = TRUE),
  Road_Surface_Conditions = sample(1:3, 100, replace = TRUE),
  Speed_limit = sample(30:70, 100, replace = TRUE)
)

# Attribute weights
attribute_weights <- c(
  X1st_Road_Class = 0.2,
  Accident_Severity = 0.3,
  Road_Surface_Conditions = 0.2,
  Speed_limit = 1
)

# Create a new column for overall performance
accident$Overall_Performance <- rowSums(
  accident[, names(attribute_weights)] * attribute_weights,
  na.rm = TRUE
)

# Display the resulting data frame with the overall performance column
head(accident)


# Bar plot for 'X1st_Road_Class'
barplot(table(accident$X1st_Road_Class), 
        main = "Distribution of X1st_Road_Class", 
        xlab = "Road Class", 
        ylab = "Count", 
        col = "skyblue")

# Pie chart for 'Accident_Severity'
severity_counts <- table(accident$Accident_Severity)
pie(severity_counts, 
    main = "Pie Chart of Accident Severity", 
    col = rainbow(length(severity_counts)), 
    labels = names(severity_counts))

# Scatter plot for 'Road_Surface_Conditions' and 'Speed_limit'
plot(accident$Road_Surface_Conditions, 
     accident$Speed_limit, 
     pch = 16, 
     col = "blue", 
     main = "Scatter Plot of Road Surface Conditions vs. Speed Limit", 
     xlab = "Road Surface Conditions", 
     ylab = "Speed Limit")


# Scatter plot for 'Road_Surface_Conditions' and 'Speed_limit'
plot(accident$Road_Surface_Conditions, 
     accident$Speed_limit, 
     pch = 16, 
     col = "purple", 
     main = "Scatter Plot (Base R)",
     xlab = "Road Surface Conditions", 
     ylab = "Speed Limit")

# Load the ggplot2 library if not already loaded
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

# ggplot2 Scatter plot for 'Road_Surface_Conditions' and 'Speed_limit'
ggplot(accident, aes(x = Road_Surface_Conditions, y = Speed_limit)) +
  geom_point(color = "blue") +
  labs(title = "Scatter Plot (ggplot2)",
       x = "Road Surface Conditions",
       y = "Speed Limit") +
  theme_minimal()




accident %>%
  ggplot(aes(x = X1st_Road_Number)) +
  geom_bar() +
  labs(title = "Distribution of X1st_Road_Number in Accidents", x = " X1st_Road_Number", y = "frequency")

# Example: Box plot for Engine Capacity
accident %>%
  ggplot(aes(x = "Number_of_casualities", y = Number_of_Casualties)) +
  geom_boxplot() +
  labs(title = "Distribution of No_of_casualities in Accidents", x = "No_of_casualities", y = "frequency")

accident %>%
  ggplot(aes(x = "Number_of_Vehicles", y = Number_of_Vehicles)) +
  geom_boxplot() +
  labs(title = "Distribution of Number_of_Vehicles  in Accidents", x = "Number_of_Vehicles", y = "frequency")

accident %>%
  ggplot(aes(x = "Year", y = Year)) +
  geom_boxplot() +
  labs(title = "Distribution of year in Accidents", x = "Year", y = "frequency")

accident %>%
  ggplot(aes(x = "Speed_limit", y = Speed_limit)) +
  geom_boxplot() +
  labs(title = "Distribution of Speed_limit in Accidents", x = " Speed_limit", y = "frequency")




# Add more visualizations based on your selected features

accident %>%
  ggplot(aes(x = Speed_limit, y = Number_of_Casualties, fill = Speed_limit)) +
  geom_bar() +
  labs(title = "Distribution of Speed_limit in Accidents", x = "Speed_limit", y = "Count") +
  theme(legend.position = "none")

# Example: Box plot for Engine Capacity by Vehicle Type

accident %>%
  ggplot(aes(x = Speed_limit, y = Year, fill = Speed_limit)) +
  geom_boxplot() +
  labs(title = "year Distribution by Speed_limit", x = "Speed_limit", y = "Year")

accident %>%
  ggplot(aes(x = Year, y = Number_of_Casualties, fill = Year)) +
  geom_boxplot() +
  labs(title = "Number_of_casualities Distribution by year ", x = "Year", y = "Number_of_casualities")



plot_boxplot(vehicle, by = "Age_Band_of_Driver")

ExpNumViz(vehicle, target = "Age_Band_of_Driver", Page = c(2,2))






# Assuming 'Accident' is your data frame and 'Road_Type' is a categorical column
# Create dummy variables for Road_Type
dummies <- model.matrix(~ Road_Type - 1, data = accident)
colnames(dummies) <- gsub("Road_Type", "", colnames(dummies))
accident <- cbind(accident, dummies)

# Assuming 'Accident' is your data frame
numeric_columns <- sapply(accident, is.numeric)
numeric_data <- accident[, numeric_columns]

# Box plot for numeric features
boxplot(numeric_data, main = "Box Plot of Numeric Features", col = "lightblue", las = 2)


# Assuming 'Accident' is your data frame
numeric_columns <- sapply(accident, is.numeric)
numeric_data <- accident[, numeric_columns]

# Pairwise scatter plot for numeric features
pairs(numeric_data, main = "Pairwise Scatter Plot of Numeric Features", pch = 16, col = "blue")

# Assuming 'Accident' is your data frame and 'Weather_Conditions' is a categorical column
# Create a pie chart for Weather Conditions

# Increase the device size
options(repr.plot.width = 6, repr.plot.height = 6)

# Adjust the margins
par(mar = c(0, 0, 0, 0) + 0.1)

# Create a pie chart for Weather Conditions
weather_counts <- table(accident$Weather_Conditions)
pie(weather_counts, main = "Pie Chart of Weather Conditions", col = rainbow(length(weather_counts)))


# Assuming 'Accident' is your data frame with the 'Overall_Performance' column
hist(accident$Overall_Performance, 
     main = "Histogram of Overall Performance", 
     xlab = "Overall Performance", 
     col = "skyblue")


create_report(vehicle)
create_report(accident)
