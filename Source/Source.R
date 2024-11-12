# Load necessary libraries
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(shiny)

# ==========================
# 1. Load the Dataset
# ==========================
# Specify the path to the CSV file
df <- read_csv("IMDb_Top_Rated_Titles.csv")

# ==========================
# 2. Data Cleaning
# ==========================
# Check for missing values
missing_values <- colSums(is.na(df))
print("Missing values in each column:")
print(missing_values[missing_values > 0])

# Handle missing values (Example: fill with the mean for averageRating)
df$averageRating[is.na(df$averageRating)] <- mean(df$averageRating, na.rm = TRUE)

# Check for outliers
Q1 <- quantile(df$averageRating, 0.25)
Q3 <- quantile(df$averageRating, 0.75)
IQR <- Q3 - Q1

# Identify outliers
outliers <- df %>% filter(averageRating < (Q1 - 1.5 * IQR) | averageRating > (Q3 + 1.5 * IQR))
print("Outliers found:")
print(outliers)

# Handle outliers (Example: remove them)
df <- df %>% filter(averageRating >= (Q1 - 1.5 * IQR) & averageRating <= (Q3 + 1.5 * IQR))

# ==========================
# 3. Data Transformation
# ==========================
# Create a new column for the release year
df <- df %>% mutate(releaseYear = as.integer(releaseYear))

# Filter movies with a rating greater than 8
high_rated_movies <- df %>% filter(averageRating > 8)

# Group by genres and calculate the average rating
average_rating_by_genre <- df %>%
  separate_rows(genres, sep = ", ") %>%  # Split multiple genres into separate rows
  group_by(genres) %>%
  summarise(average_rating = mean(averageRating, na.rm = TRUE))

# ==========================
# 4. Data Exploration
# ==========================
# Correlation analysis
correlation_matrix <- cor(df %>% select_if(is.numeric))
print("Correlation matrix:")
print(correlation_matrix)

# Visualization of the distribution of average ratings
ggplot(df, aes(x = averageRating)) +
  geom_histogram(bins = 20, fill = "blue", alpha = 0.7) +
  geom_density(color = "red") +
  labs(title = 'Distribution of Average Ratings', x = 'Average Rating', y = 'Frequency')

# Visualization of average rating by genre
ggplot(average_rating_by_genre, aes(x = average_rating, y = reorder(genres, average_rating))) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = 'Average Rating by Genre', x = 'Average Rating', y = 'Genre')

# ==========================
# 5. Web Application
# ==========================
# Define UI
ui <- fluidPage(
  titlePanel("IMDB Top Rated Movies and TV Shows"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Filters"),
      sliderInput("ratingInput", "Select Rating:", 
                  min = min(df$averageRating), max = max(df$averageRating), value = c(8, 10))
    ),
    
    mainPanel(
      plotOutput("ratingDist"),
      plotOutput("avgRatingByGenre")
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$ratingDist <- renderPlot({
    ggplot(df, aes(x = averageRating)) +
      geom_histogram(bins = 20, fill = "blue", alpha = 0.7) +
      geom_density(color = "red") +
      labs(title = 'Distribution of Average Ratings', x = 'Average Rating', y = 'Frequency')
  })
  
  output$avgRatingByGenre <- renderPlot({
    avg_rating_filtered <- average_rating_by_genre %>%
      filter(average_rating >= input$ratingInput[1] & average_rating <= input$ratingInput[2])
    
    ggplot(avg_rating_filtered, aes(x = average_rating, y = reorder(genres, average_rating))) + 
      geom_bar(stat = "identity", fill = "lightblue") +
      labs(title = 'Average Rating by Genre', x = 'Average Rating', y = 'Genre')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)