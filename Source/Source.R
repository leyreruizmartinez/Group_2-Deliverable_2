# Load necessary libraries
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(shiny)
library(ggcorrplot)
library(plotly)

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

# Check for outliers using IQR
Q1 <- quantile(df$averageRating, 0.25)
Q3 <- quantile(df$averageRating, 0.75)
IQR <- Q3 - Q1

# Identify and remove outliers
df <- df %>% filter(averageRating >= (Q1 - 1.5 * IQR) & averageRating <= (Q3 + 1.5 * IQR))

# Check for duplicates and remove them
duplicates <- df %>% filter(duplicated(.))
print("Duplicados encontrados:")
print(duplicates)
df <- df %>% distinct()

# Check for implicit missing values
implicit_missing <- df %>% filter(genres == "" | is.na(genres))
print("Valores implícitamente faltantes:")
print(implicit_missing)

# ==========================
# 3. Data Transformation
# ==========================
# Create a new column for the release year as integer
df <- df %>% mutate(releaseYear = as.integer(releaseYear))

# Create a new column for rating categories
df <- df %>% mutate(ratingCategory = case_when(
  averageRating >= 9 ~ "Excellent",
  averageRating >= 8 ~ "Good",
  averageRating >= 7 ~ "Average",
  TRUE ~ "Below Average"
))

# Group by genres and calculate the average rating
average_rating_by_genre <- df %>%
  separate_rows(genres, sep = ", ") %>%  # Split multiple genres into separate rows
  group_by(genres) %>%
  summarise(average_rating = mean(averageRating, na.rm = TRUE))

# ==========================
# 4. Data Exploration
# ==========================
# Correlation matrix
correlation_matrix <- cor(df %>% select_if(is.numeric), use = "complete.obs")

# Distribution of average ratings
dist_plot <- ggplot(df, aes(x = averageRating)) +
  geom_histogram(bins = 20, fill = "blue", alpha = 0.7) +
  geom_density(color = "red") +
  labs(title = 'Distribution of Average Ratings', x = 'Average Rating', y = 'Frequency')

# Titles by genre
titles_by_genre <- df %>%
  separate_rows(genres, sep = ", ") %>%
  group_by(genres) %>%
  summarise(count = n())

titles_by_genre_plot <- ggplot(titles_by_genre, aes(x = reorder(genres, count), y = count)) +
  geom_bar(stat = "identity", fill = "green") +
  coord_flip() +
  labs(title = "Number of Titles by Genre", x = "Genre", y = "Number of Titles")

# Trend of average ratings by release year
rating_by_year <- df %>%
  group_by(releaseYear) %>%
  summarise(avg_rating = mean(averageRating, na.rm = TRUE))

rating_by_year_plot <- ggplot(rating_by_year, aes(x = releaseYear, y = avg_rating)) +
  geom_line(color = "blue") +
  labs(title = "Trend of Ratings by Year", x = "Release Year", y = "Average Rating")

# Heatmap for correlation matrix
correlation_plot <- ggcorrplot(correlation_matrix, lab = TRUE, title = "Correlation Map")

# 3D Scatter Plot: Votes, Ratings, and Release Year
scatter3d_plot <- plot_ly(
  data = df,
  x = ~releaseYear,
  y = ~numVotes,
  z = ~averageRating,
  type = "scatter3d",
  mode = "markers",
  marker = list(size = 5, color = ~averageRating, colorscale = "Viridis", showscale = TRUE)
) %>%
  layout(
    title = "3D Scatter Plot of Ratings, Votes, and Release Year",
    scene = list(
      xaxis = list(title = "Release Year"),
      yaxis = list(title = "Number of Votes"),
      zaxis = list(title = "Average Rating")
    )
  )

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
                  min = min(df$averageRating), max = max(df$averageRating), value = c(8, 10)),
      sliderInput("yearInput", "Select Year Range:", 
                  min = min(df$releaseYear, na.rm = TRUE), 
                  max = max(df$releaseYear, na.rm = TRUE), 
                  value = c(2000, 2023)),
      downloadButton("downloadData", "Download Filtered Data")
    ),
    
    mainPanel(
      plotOutput("ratingDist"),
      plotOutput("avgRatingByGenre"),
      plotlyOutput("interactivePlot"),
      plotlyOutput("scatter3d")
    )
  )
)

# Define server logic
server <- function(input, output) {
  filtered_data <- reactive({
    df %>%
      filter(releaseYear >= input$yearInput[1] & releaseYear <= input$yearInput[2] &
               averageRating >= input$ratingInput[1] & averageRating <= input$ratingInput[2])
  })
  
  output$ratingDist <- renderPlot({
    ggplot(filtered_data(), aes(x = averageRating)) +
      geom_histogram(bins = 20, fill = "blue", alpha = 0.7) +
      geom_density(color = "red") +
      labs(title = 'Distribution of Average Ratings', x = 'Average Rating', y = 'Frequency')
  })
  
  output$avgRatingByGenre <- renderPlot({
    avg_rating_filtered <- average_rating_by_genre %>%
      filter(average_rating >= input$ratingInput[1] & average_rating <= input$ratingInput[2])
    
    ggplot(avg_rating_filtered, aes(x = average_rating, y = reorder(genres, average_rating))) + 
      geom_bar(stat = "identity", fill = "purple") +
      labs(title = 'Average Rating by Genre', x = 'Average Rating', y = 'Genre')
  })
  
  output$interactivePlot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = releaseYear, y = averageRating)) +
      geom_point(aes(color = genres)) +
      labs(title = "Rating by Year", x = "Year", y = "Average Rating")
    ggplotly(p)
  })
  
  output$scatter3d <- renderPlotly({
    scatter3d_plot
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { paste("filtered_data", Sys.Date(), ".csv", sep="") },
    content = function(file) {
      write.csv(filtered_data(), file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)