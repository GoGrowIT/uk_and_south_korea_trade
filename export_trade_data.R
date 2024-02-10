################################################################################
###############################################################################

#© 2024 - GoGrow.it All Rights are Reserved

#interesting in using the computations? Write an email at general@gogrow.it

#data source: gov.uk




#Uncomment & Install if the required packages are not present

#install.packages("extrafont")
#install.packages("ggplot2")
#install.packages("dplyr")

################################################################################
################################################################################


# Load necessary libraries
library(ggplot2)
library(dplyr)
library(extrafont)

# Create a data frame
trade_data <- data.frame(
  Year = c(2017, 2018, 2019, 2020, 2021, 2022, 2023),
  Volume_UK_to_SK = c(7.5, 7.79, 4.8, 4.91, 6.46, 6.11, 10.96),
  Volume_SK_to_UK = c(8.63, 6.36, 5.54, 4.49, 5.96, 6.36, 8.31)
)

# Fit a linear regression model
model_UK_to_SK <- lm(Volume_UK_to_SK ~ Year, data = trade_data)
model_SK_to_UK <- lm(Volume_SK_to_UK ~ Year, data = trade_data)

# Make predictions
future_years <- data.frame(Year = c(2024, 2025, 2026))
predictions_UK_to_SK <- predict(model_UK_to_SK, newdata = future_years)
predictions_SK_to_UK <- predict(model_SK_to_UK, newdata = future_years)

# Add predictions to the data frame
trade_data_future <- data.frame(
  Year = future_years$Year,
  Volume_UK_to_SK = predictions_UK_to_SK,
  Volume_SK_to_UK = predictions_SK_to_UK
)

# Combine past data and future predictions
trade_data_all <- rbind(trade_data, trade_data_future)



# Create a ggplot2 graph
ggplot(trade_data_all, aes(x = Year)) +
  geom_point(aes(y = Volume_UK_to_SK), color = ifelse(trade_data_all$Year > 2023, "red", "blue")) +
  geom_line(aes(y = Volume_UK_to_SK), color = "black") +
  geom_point(aes(y = Volume_SK_to_UK), color = ifelse(trade_data_all$Year > 2023, "red", "blue")) +
  geom_line(aes(y = Volume_SK_to_UK), color = "black") +  
  geom_text(data = subset(trade_data_all, Year > 2023), aes(y = Volume_UK_to_SK, label = round(Volume_UK_to_SK, 2)), vjust = -1.5, color = "red") +
  geom_text(data = subset(trade_data_all, Year > 2023), aes(y = Volume_SK_to_UK, label = round(Volume_SK_to_UK, 2)), vjust = -1.5, color = "red") +
  scale_color_manual(values = c("blue", "red"), labels = c("Actual", "Predicted")) +
  theme_minimal() +
  xlab("Year") + ylab("Trade Volume ($US Bilions)") + ggtitle("Trade volume between UK and South Korea") +
  theme(plot.title=element_text(hjust = 0.5, family = "Rockwell"), 
        axis.title.x=element_text(color = "blue", size = 15, family="Rockwell"),
        axis.title.y=element_text(color = "blue", size = 15, family="Rockwell"),
        
        legend.title = element_text(size=13),
        legend.text=element_text(size=10, family="Rockwell" ))
