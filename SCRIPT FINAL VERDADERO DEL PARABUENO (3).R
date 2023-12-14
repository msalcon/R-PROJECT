# Upload both libraries for data cleaning and manipulation
library(dplyr)  
library(tidyr)  
library(ggplot2)
library(forcats)
library(scales)
library(plotly)

# Data cleaning function
cleanData <- function(data) {
  # Create a copy of the data to avoid modifying the original dataset
  cleaned_data <- data
  
  # Change empty values in the fuel type column to electric cars and the "-" to gasoline
  cleaned_data$fuel_type[is.na(cleaned_data$fuel_type)] <- "Electric"
  cleaned_data$fuel_type[cleaned_data$fuel_type == "–"] <- 'Gasoline'
  
  # Change empty values in the engine column
  cleaned_data$engine[cleaned_data$engine == "–"] <- '292.0HP 2.0L 4 Cylinder Engine Gasoline Fuel'
  
  # Change empty values in the clean_title column
  cleaned_data$clean_title[is.na(cleaned_data$clean_title)] <- "No"
  
  # Change empty values in the accident column
  cleaned_data$accident[is.na(cleaned_data$accident)] <- "None reported"
  
  # Change the color of the car from "-" to white and gray
  cleaned_data$int_col[cleaned_data$int_col == "–"] <- 'White'
  cleaned_data$ext_col[cleaned_data$ext_col == "–"] <- 'Gray'
  
  # Change the transmission from A/T to automatic 
  cleaned_data$transmission[cleaned_data$transmission == "A/T"] <- 'Automatic'
  
  # Delete the $ symbol to make the value numeric and operable
  cleaned_data$price <- as.numeric(gsub("[^0-9.]", "", cleaned_data$price))
  
  # Change millage to kilometer
  cleaned_data$milage <- as.numeric(gsub("[^0-9.]", "", cleaned_data$milage)) * 1.60934
  
  # Change column names "price" and "milage"
  names(cleaned_data)[names(cleaned_data) == "price"] <- "price($)"
  names(cleaned_data)[names(cleaned_data) == "milage"] <- "distance_traveled(km)"
  
  return(cleaned_data)
}


# Perform data cleaning and get cleaned data
cleaned_data <- cleanData(used_cars)
# This is the end of the Data cleaning

# Now we start analyzing the data to obtain the most relevant insights
# We want to start knowing statistical data about the price of the cars
# Summary statistics of the price column
summary(cleaned_data$`price($)`)
# The first thing that we want to know is the 10 brands we more cars in this Data set
# Load the ggplot2 library
# Create a summary with the count of cars per brand
brand_summary <- data.frame(brand = names(sort(table(cleaned_data$brand), decreasing = TRUE)[1:10]),
                            count = as.numeric(sort(table(cleaned_data$brand), decreasing = TRUE)[1:10]))

# Create the bar plot
ggplot(brand_summary, aes(x = reorder(brand, -count), y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Top 10 Brands with Most Cars",
       x = "Brand",
       y = "Number of Cars") +
  theme_minimal()

# Now we want to know which are the most common colors for a car.
# Obtain the 10 most frequent colors
# Create a summary with the count of cars per color
color_summary <- data.frame(
  ext_col = names(sort(table(cleaned_data$ext_col), decreasing = TRUE)[1:10]),
  count = as.numeric(sort(table(cleaned_data$ext_col), decreasing = TRUE)[1:10])
)

# Define colors for each category using standard color names or hex codes
color_mapping <- c("black", "white", "gray", "#C0C0C0", "blue", "red", "green", "brown", "#FFD700", "#F5F5DC")

# Convert ext_col to a factor with levels in the desired order
color_summary$ext_col <- factor(color_summary$ext_col, levels = color_summary$ext_col)

# Create the bar plot with specific colors and black borders
ggplot(color_summary, aes(x = reorder(ext_col, -count), y = count, fill = ext_col)) +
  geom_bar(stat = "identity", color = "black") +  # Add black borders
  scale_fill_manual(values = color_mapping) +  # Assign specific colors
  labs(title = "Top 10 Most Common Car Colors",
       x = "Color",
       y = "Number of Cars") +
  theme_minimal()


# Now we are going to make a dispersion chart to know the correlation between the price of the cars and the distance traveled
ggplot(cleaned_data, aes(x = `distance_traveled(km)`, y = `price($)`)) +
  geom_point(alpha = 0.6, size = 2) +  # Add scatter plot points with transparency
  stat_smooth(method = "lm", col = "red", se = FALSE) +  # Add a linear regression line
  labs(title = "Average Price per kilometer",
       x = "km",
       y = "Price") +
  theme_minimal() +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma)


# Now we want to know which is the average price per type of fuel required in a car
ggplot(cleaned_data, aes(x = fct_reorder(fuel_type, `price($)`, .fun = mean), y = `price($)`)) +
  geom_bar(stat = "summary", fun = "mean", fill = "steelblue", alpha = 0.7, position = "identity") +
  labs(title = "Correlation between the type of fuel requierd by the car and the price",
       x = "Type of fuel",
       y = "Average Price") +
  theme_minimal() +
  coord_flip()  # Put horizontal bars


# Create a summary with the 10 most frequent cars
resumen_motores <- cleaned_data %>%
  group_by(engine) %>%
  summarize(mean_price = mean(`price($)`), count = n()) %>%
  top_n(10, wt = count)  # Select 10 cars

# Create point chart
ggplot(resumen_motores, aes(x = fct_reorder(engine, mean_price), y = mean_price)) +
  geom_point(color = "steelblue", size = 3) +
  labs(title = "average price of the engines",
       x = "Engine",
       y = "Average price (euros)") +
  theme_minimal() +
  coord_flip()

# The next thing that we want to know is the distribution of the price of the 10 best brands
# Create a summary with the mean price per brand for the top 5 most used brands
brand_summary <- cleaned_data %>%
  group_by(brand) %>%
  summarize(mean_price = mean(`price($)`), count = n()) %>%
  top_n(5, wt = count)  # Select the top 5 most used brands

# Filter the dataset to include only the top 5 brands
cleaned_data_filtered <- cleaned_data %>%
  filter(brand %in% brand_summary$brand)

# Create the boxplot with log-scale y-axis and formatted labels
ggplot(cleaned_data_filtered, aes(x = brand, y = `price($)`)) +
  geom_boxplot(fill = "steelblue", color = "darkblue", alpha = 0.7) +
  geom_point(data = brand_summary, aes(x = brand, y = mean_price), color = "red", size = 3) +
  labs(title = "Price Distribution by Top 5 Brands",
       x = "Brand",
       y = "Mean Price (Thousands dollars)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  scale_y_continuous(trans = "log10", labels = scales::number_format(scale = 1e-3))  # Set log scale on y-axis with formatted labels


# The next thing that we want to do is to know how many cars per year were selling to know the demand through the years.
# Create a summary with the count of cars per year
year_summary <- cleaned_data %>%
  group_by(model_year) %>%
  summarize(count = n())

# Convert model_year to numeric
year_summary$model_year <- as.numeric(as.character(year_summary$model_year))

# Create the bar plot with trend line starting from 1995
ggplot(year_summary, aes(x = model_year, y = count)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "red", size = 1) +
  labs(title = "Number of Cars per Year",
       x = "Model Year",
       y = "Number of Cars") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  coord_cartesian(xlim = c(1995, max(year_summary$model_year)))  # Set x-axis limits

# The next thing that we want to discover is the relation between the motors of the cars and the accidents
# Create a summary with the count of accidents per engine
accident_summary <- data.frame(
  engine = names(sort(table(cleaned_data$engine), decreasing = TRUE))[1:5],
  accident_count = as.numeric(sort(table(cleaned_data$engine), decreasing = TRUE))[1:5]
)

# Create a scatter plot with a trend line for the count of accidents per engine
ggplot(accident_summary, aes(x = engine, y = accident_count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red", size = 1) +
  labs(title = "Correlation between Accidents and Engine Count",
       x = "Engine",
       y = "Accident Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# The next thing in which we want to focus is to compare the transmission of the car, the prices and along the year to know the trends
# Filter the top 10 most common transmissions
top_transmissions <- cleaned_data %>%
  group_by(transmission) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  slice_head(n = 10) %>%
  pull(transmission)

# Filter the dataframe to include only the top 10 transmissions
filtered_cars <- cleaned_data %>%
  filter(transmission %in% top_transmissions)

# Create a scatter plot with color-coded transmission
ggplot(filtered_cars, aes(x = model_year, y = `price($)`, color = transmission)) +
  geom_point() +
  labs(title = "Comparison of Top 10 Transmissions, Price, and Model Year",
       x = "Model Year",
       y = "Price",
       color = "Transmission") +
  theme_minimal() +
  scale_y_continuous(labels = comma_format())

# The next thing that it is important for the company is to know the propotion of clean title in the cars:
clean_title_counts <- cleaned_data %>%
  count(clean_title)

# Calculate the percentage
clean_title_counts <- clean_title_counts %>%
  mutate(percentage = n / sum(n) * 100)

# Craete a cake chart
ggplot(clean_title_counts, aes(x = "", y = percentage, fill = clean_title)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y") +
  labs(title = "Percentage on clean title",
       y = "Porcentaje") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  geom_text(aes(label = paste(round(percentage), "%")), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("Yes" = "green", "No" = "red"))

# Now we want to now how big is the fall of the price of a car after having an accident, so we chose Ford because is the most common brand in this dataset
# Filter data for Ford cars
ford_data <- cleaned_data %>%
  filter(brand == "Ford")

# Calculate the average price for Ford cars with and without accidents
avg_price_accident <- ford_data %>%
  filter(accident == "At least 1 accident or damage reported") %>%
  summarise(avg_price_accident = mean(`price($)`))

avg_price_no_accident <- ford_data %>%
  filter(accident == "None reported") %>%
  summarise(avg_price_no_accident = mean(`price($)`))

# Combine the results into a data frame
resultados <- data.frame(
  Condición = c("Accident", "No Accident"),
  Precio_Promedio = c(avg_price_accident$avg_price_accident, avg_price_no_accident$avg_price_no_accident)
)

# Create the ggplot object
gg <- ggplot(resultados, aes(x = Condición, y = Precio_Promedio, fill = Condición)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Price for Ford Cars with/without Accidents",
       x = "Suffered an accident",
       y = "Average Price (dollars)") +
  theme_minimal() +
  theme(legend.position = "none")  # Hide the legend because there are only two variables

# Convert ggplot object to plotly object
p <- ggplotly(gg)

# Display the interactive plot
p

# But lets see the evolution across the years
# Filter data for Ford cars from the year 2000 onwards
ford_data_2000 <- cleaned_data %>%
  filter(brand == "Ford", model_year >= 2000)

# Map the 'accident' column to more descriptive labels
ford_data_2000$accident <- recode(ford_data_2000$accident,
                                  "None reported" = "No Accident",
                                  "At least 1 accident or damage reported" = "Accident")

# Calculate the median price for Ford cars with and without accidents over the years
price_summary_2000 <- ford_data_2000 %>%
  group_by(model_year, accident) %>%
  summarise(median_price = median(`price($)`)) %>%
  spread(key = accident, value = median_price)

# Create a line chart with shaded areas
ggplot(price_summary_2000, aes(x = model_year, y = `Accident`, fill = "Accident")) +
  geom_ribbon(aes(ymin = `No Accident`, ymax = `Accident`), fill = "red", alpha = 0.3) +
  geom_line(aes(y = `Accident`, color = "Accident"), size = 1.5) +
  geom_line(aes(y = `No Accident`, color = "No Accident"), size = 1.5) +
  labs(title = "Median Price for Ford Cars with/without Accidents Since 2000",
       x = "Model Year",
       y = "Median Price (dollars)",
       color = "Accident") +
  scale_color_manual(values = c("Accident" = "blue", "No Accident" = "green")) +
  theme_minimal()