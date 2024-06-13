# Load all necessary libraries

library(tidyverse) # For data manipulation and visualization
install.packages("plotly")
library(plotly)
library(ggplot2)

# Load the dataset
data <- read.csv("C:/Users/abhin/Downloads/archive (1)/zomato.csv")
#print(data[1:5, ])
# Drop unnecessary columns
data <- data %>%
  select(-c(url, address, phone, menu_item))

# Remove duplicates
# data <- unique(data)

# Clean the rate column
data$rate <- as.numeric(gsub("/", "", as.character(data$rate)))
data$rate[data$rate == "NEW" | data$rate == "-"] <- NA

# Convert cost to numeric
data$`approx_cost_for_two_people` <- as.numeric(gsub(",", "", as.character(data$`approx_cost_for_two_people`)))

# Drop rows with missing values
data <- na.omit(data)

# Plot top 10 restaurant names
data %>%
  count(name) %>%
  top_n(10, n) %>%
  ggplot(aes(x = reorder(name, -n), y = n)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.5, size = 3) +
  labs(x = "Top 10 Restaurant Names", y = "Number of Sales Points", title = "Top 10 Restaurants - Number of Sales Point") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Pie chart for online order rate
data %>%
  count(online_order) %>%
  mutate(percentage = n / sum(n) * 100) %>%  # Calculate percentages
  ggplot(aes(x = "", y = n, fill = online_order, label = paste0(round(percentage, 1), "%"))) +
  geom_bar(stat = "identity") +
  geom_text(position = position_stack(vjust = 0.5)) +  # Add labels
  coord_polar("y", start = 0) +
  labs(title = "ONLINE ORDER RATE") +
  theme_void()

# Pie chart for book table rate
data %>%
  count(book_table) %>%
  mutate(percentage = n / sum(n) * 100) %>%  # Calculate percentages
  ggplot(aes(x = "", y = n, fill = book_table, label = paste0(round(percentage, 1), "%"))) +
  geom_bar(stat = "identity") +
  geom_text(position = position_stack(vjust = 0.5)) +  # Add labels
  coord_polar("y", start = 0) +
  labs(title = "BOOK TABLE RATE") +
  theme_void()

# Bar plot for most preferred locations

data %>%
  count(location) %>%
  top_n(10, n) %>%
  ggplot(aes(x = reorder(location, n), y = n)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.5, size = 3) +  
  labs(x = "Top 10 Locations", y = "Number of Sales Points", title = "Most Preferred Locations") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Distribution plot for rates
ggplot(data, aes(x = rate)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Rating")

# Bar plot for restaurant types
data %>%
  count(rest_type) %>%
  top_n(20, n) %>%
  ggplot(aes(x = reorder(rest_type, n), y = n)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.5, size = 3) +
  labs(x = "Restaurant Types", y = "Number of Restaurant Types", title = "Restaurant Types") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Bar plot for cuisines
data %>%
  separate_rows(cuisines, sep = ", ") %>%
  count(cuisines) %>%
  top_n(10, n) %>%
  ggplot(aes(x = reorder(cuisines, n), y = n)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.5, size = 3)+
  labs(x = "Cuisines", y = "Number of Cuisines", title = "Cuisines") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Bar plot for locations
data %>%
  count(location) %>%
  top_n(10, n) %>%
  ggplot(aes(x = reorder(location, n), y = n)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.5, size = 3) +
  labs(x = "Cities", y = "Number of Cities", title = "Location") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#.........................................................



# Bar plot for service type
data %>%
  count(listed_in_type) %>%
  top_n(10, n) %>%
  ggplot(aes(x = reorder(listed_in_type, n), y = n)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.5, size = 3) +
  labs(x = "Service Type", y = "Number of Service Type", title = "Service Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Bar plot for cities
data %>%
  count(listed_in_city) %>%
  top_n(10, n) %>%
  ggplot(aes(x = reorder(listed_in_city, n), y = n)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.5, size = 3) +
  labs(x = "Cities", y = "Number of Cities", title = "Cities") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#...........Linear Regression........
# Load necessary packages
library(dplyr)
library(ggplot2)

# Assuming 'data' is your dataset, let's fit a linear regression model to predict 'rate' based on 'approx_cost_for_two_people'
model <- lm(rate ~ approx_cost_for_two_people, data = data)

# Summary of the regression model
summary(model)


#..............................

# Assuming 'data' is your dataset

# Set the seed for reproducibility
set.seed(123)

# Determine the proportion of data to allocate to the training set
train_prop <- 0.8  # 80% of data for training, 20% for testing

# Create an index to split the data
train_index <- sample(seq_len(nrow(data)), size = round(train_prop * nrow(data)))

# Split the data into training and test sets
train_data <- data[train_index, ]
test_data <- data[-train_index, ]






# Plotting the regression line
ggplot(data, aes(x = approx_cost_for_two_people, y = rate)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Linear Regression: Rate vs. Approximate Cost for Two People", y="Rating", x="Approximate cost for two people")

# Fit a linear regression model on the training data
model <- lm(approx_cost_for_two_people ~ rate, data = train_data)

# Predict 'approx_cost_for_two_people' for the test set
predictions <- predict(model, newdata = test_data)

# Evaluate the performance of the model
# For example, you can calculate the Mean Squared Error (MSE)
mse <- mean((test_data$approx_cost_for_two_people - predictions)^2)
cat("Mean Squared Error (MSE):", mse, "\n")
# Calculate the Mean Squared Error (MSE) as a percentage
mse_percentage <- mse / mean(test_data$approx_cost_for_two_people) * 100
cat("Mean Squared Error (MSE) as percentage:", mse_percentage, "%\n")

# Calculate the Root Mean Squared Error (RMSE) as a percentage
rmse_percentage <- rmse / mean(test_data$approx_cost_for_two_people) * 100
cat("Root Mean Squared Error (RMSE) as percentage:", rmse_percentage, "%\n")


# Or calculate the Root Mean Squared Error (RMSE)
rmse <- sqrt(mse)
cat("Root Mean Squared Error (RMSE):", rmse, "\n")

# You can also calculate R-squared to assess the goodness of fit
rsquared <- summary(model)$r.squared
cat("R-squared:", rsquared, "\n")
#....................

# Train linear regression model
lm_model <- lm(rate ~ `approx_cost_for_two_people`, data = data)

# Predict ratings for all restaurants
data$predicted_rating <- predict(lm_model, newdata = data)

# Calculate score for each restaurant (predicted_rating / approximate cost)
data$score <- data$predicted_rating / data$`approx_cost_for_two_people`

# Filter out restaurants with high scores (adjust threshold as needed)
best_restaurant <- data %>%
  filter(score == max(score))


# Print name, rating, and cost of the best restaurant
print(best_restaurant[, c("name", "rate", "approx_cost_for_two_people")])

#.....................................


# Train linear regression model
lm_model <- lm(rate ~ approx_cost_for_two_people, data = data)

# Predict ratings for all restaurants
data$predicted_rating <- predict(lm_model, newdata = data)

# Calculate score for each restaurant (predicted_rating / approximate cost)
data$score <- data$predicted_rating / data$approx_cost_for_two_people

# Filter out restaurants with high scores (adjust threshold as needed)
top_restaurants <- data %>%
  distinct(name, .keep_all = TRUE) %>%
  arrange(desc(score)) %>%
  head(5)

# Print top 5 best restaurants
print(top_restaurants[, c("name", "rate", "approx_cost_for_two_people")])

