#load and exploring the dataset
install.packages("recommenderlab")
install.packages("aws.s3")

# Load necessary libraries
library(tidyverse) # For data manipulation
library(readr)     # For reading data
library(DT)        # For displaying data in a tabular format

# Load the dataset
data <- read.csv("C:\\Users\\HP\\OneDrive\\Documents\\movie_recommandation\\Data.csv")

# Check the structure of the dataset
str(data)


# Display the first few rows of the dataset
head(data)

#Data cleaning and preprocessing
# Remove duplicates
data <- distinct(data)

# Handling missing values
data <- na.omit(data) # or use more advanced imputation methods

# Feature engineering (if needed, for instance, average rating by movie)
data <- data %>%
  group_by(Genre) %>%
  mutate(avg_budget = mean(Budget.INR., na.rm = TRUE))
# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(skimr)
library(corrplot)
library(shiny)
library(caret)
library(proxy)


# Assuming your dataset is called 'data'
# View the structure of the data
str(data)

# 1. Data Inspection: Summary of the dataset
summary(data)
names(data)

# Check for missing values in each column
colSums(is.na(data))

# Check for duplicate movie entries
sum(duplicated(data$Movie.Name))

# 3. Visualizing Revenue Distribution
ggplot(data, aes(x = `Revenue.INR.`)) + 
  geom_histogram(binwidth = 100000000, fill = "purple", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Movie Revenue", x = "Revenue (INR)", y = "Frequency") +
  theme_minimal()

# 4. Visualizing Budget Distribution
ggplot(data, aes(x = `Budget.INR.`)) + 
  geom_histogram(binwidth = 50000000, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Movie Budget", x = "Budget (INR)", y = "Frequency") +
  theme_minimal()

# 5. Revenue vs Budget: Scatter plot and Correlation Analysis
ggplot(data, aes(x = `Budget.INR.`, y = `Revenue.INR.`)) +
  geom_point(color = "purple", alpha = 0.5) +
  labs(title = "Revenue vs Budget", x = "Budget (INR)", y = "Revenue (INR)") +
  theme_minimal()

# Calculate the correlation between Revenue and Budget
cor(data$`Revenue.INR.`, data$`Budget.INR.`, use = "complete.obs")

# 6. Genre Analysis: Revenue and Budget by Genre
ggplot(data, aes(x = reorder(Genre, `Revenue.INR.`), y = `Revenue.INR.`)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(title = "Revenue by Genre", x = "Genre", y = "Revenue (INR)") +
  coord_flip() +  # Flip the plot for better readability
  theme_minimal()

# Budget by Genre
ggplot(data, aes(x = reorder(Genre, `Budget.INR.`), y = `Budget.INR.`)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Budget by Genre", x = "Genre", y = "Budget (INR)") +
  coord_flip() +
  theme_minimal()

# 7. Top 10 Movies by Revenue
top_movies <- data %>%
  arrange(desc(`Revenue.INR.`)) %>%
  slice_head(n = 10) %>%
  select(`Movie.Name`, `Revenue.INR.`)

ggplot(top_movies, aes(x = reorder(`Movie.Name`, `Revenue.INR.`), y = `Revenue.INR.`)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(title = "Top 10 Revenue-Generating Movies", x = "Movie Name", y = "Revenue (INR)") +
  coord_flip() +
  theme_minimal()

# One-hot encoding the Genre column
genre_dummies <- model.matrix(~ Genre - 1, data = data)

# Normalizing the Revenue column (scaling it to a range between 0 and 1)
revenue_scaled <- scale(data$Revenue.INR.)

# Combine the Genre and Revenue features
movie_features <- cbind(genre_dummies, Revenue = revenue_scaled)

# Calculate cosine similarity between movies
similarity_matrix <- proxy::dist(movie_features, method = "cosine")

# Convert similarity to a matrix (lower value means more similarity)
similarity_matrix <- as.matrix(similarity_matrix)

# Find the index of the movie you're interested in (e.g., Movie1)
movie_index <- which(data$Movie.Name == "Dangal")

# Get the similarity scores for this movie (row corresponding to "Movie1")
movie_similarities <- similarity_matrix[movie_index, ]

# Sort movies by similarity (ascending order, so the smallest values are most similar)
recommended_movies <- order(movie_similarities)

# Exclude the movie itself from the recommendations
recommended_movies <- recommended_movies[recommended_movies != movie_index]

# Get the names of the top 3 recommended movies
top_recommendations <- data$Movie.Name[recommended_movies[1:3]]

print(top_recommendations)
library(shiny)
library(shinythemes)
library(plotly)
library(DT)
library(dplyr)

image_link1 <- "https://img.freepik.com/free-photo/entertainment-movie-theatre-film-show_53876-147668.jpg?ga=GA1.1.1565817211.1731590818&semt=ais_hybrid"
image_link2 <- "https://img.freepik.com/free-photo/cinema-still-life_23-2148017207.jpg?ga=GA1.1.1565817211.1731590818&semt=ais_hybrid"
image_link3 <- "https://img.freepik.com/premium-photo/mobile-cinema-modern-technologies-watching-movies-your-smartphone_407474-20.jpg?ga=GA1.1.1565817211.1731590818&semt=ais_hybrid"
image_link4 <- "https://img.freepik.com/free-vector/cinema-icons-set_1284-11717.jpg?ga=GA1.1.1565817211.1731590818&semt=ais_hybrid"

# Path to store credentials
credentials_file <- file.path(getwd(), "credentials.csv")

# Create the credentials file if it doesn't exist
if (!file.exists(credentials_file)) {
  write.csv(data.frame(username = character(), password = character(), stringsAsFactors = FALSE), 
            credentials_file, row.names = FALSE)
}

# Load credentials from the file
credentials <- read.csv(credentials_file, stringsAsFactors = FALSE)

# Define UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  uiOutput("main_ui")
)

# Define Server
server <- function(input, output, session) {
  # Reactive values to manage state
  page_state <- reactiveVal("login") # Initial state is login
  logged_in_user <- reactiveVal(NULL) # Store logged-in user's name
  
  # Helper to update credentials
  update_credentials <- function(new_user, new_password) {
    credentials <<- rbind(credentials, data.frame(username = new_user, password = new_password, stringsAsFactors = FALSE))
    write.csv(credentials, credentials_file, row.names = FALSE)
  }
  
  # Dynamic UI
  output$main_ui <- renderUI({
    if (page_state() == "login") {
      fluidPage(
        h2("Login or Register", style = "text-align: center;"),
        div(style = "margin: auto; max-width: 300px;",
            textInput("username", "Username:", placeholder = "Enter your username"),
            passwordInput("password", "Password:", placeholder = "Enter your password"),
            actionButton("login", "Login", class = "btn btn-primary"),
            actionButton("register", "Register", class = "btn btn-success"),
            textOutput("login_message")
        )
      )
    } else if (page_state() == "welcome") {
      # Welcome Page UI
      fluidPage(
        tags$style(HTML("
          body { font-family: 'Comic Sans MS', cursive, sans-serif; background-color: #f9f9f9; }
          h1 { color: #FF4500; text-shadow: 2px 2px #000; text-align: center; }
          p { font-size: 16px; line-height: 1.6; color: #444; }
          .fun-button { font-size: 18px; font-weight: bold; margin: 15px; }
        ")),
        
        h1("Welcome to Movie Mania 🎥" , logged_in_user()),
        
        div(style = "display: flex; justify-content: center; flex-wrap: wrap; gap: 20px; margin-top: 20px;",
            img(src = image_link1, height = "200px", style = "border-radius: 15px; box-shadow: 0 4px 8px rgba(0,0,0,0.2);"),
            img(src = image_link2, height = "200px", style = "border-radius: 15px; box-shadow: 0 4px 8px rgba(0,0,0,0.2);"),
            img(src = image_link3, height = "200px", style = "border-radius: 15px; box-shadow: 0 4px 8px rgba(0,0,0,0.2);"),
            img(src = image_link4, height = "200px", style = "border-radius: 15px; box-shadow: 0 4px 8px rgba(0,0,0,0.2);")
        ),
        
        p("Welcome to Movie Mania! This is your one-stop destination for personalized movie recommendations and exploring the latest data visualizations about your favorite films."),
        
        p("Here's what you can do here:"),
        tags$ul(
          tags$li("🔍 Get movie recommendations tailored to your taste."),
          tags$li("📊 Explore 2D and 3D interactive visualizations of movie data."),
          tags$li("🎬 Discover insights into budgets, revenues, genres, and cast members.")
        ),
        
        div(style = "text-align: center; margin-top: 30px;",
            actionButton("go_to_recommendation", "Recommendations 🎥", class = "btn btn-danger fun-button"),
            actionButton("go_to_visualization", "Visualizations 📊", class = "btn btn-primary fun-button")
        )
      )
      
    } else if (page_state() == "recommendation") {
      # Recommendation Page UI
      fluidPage(
        actionButton("back_to_welcome", "Back", class = "btn btn-light"),
        h2("Movie Recommendations", style = "text-align: center; color: #4CAF50;"),
        sidebarLayout(
          sidebarPanel(
            selectInput("recommend_type", "Choose Recommendation Type:",
                        choices = c("Watched Movie", "Genre")),
            
            conditionalPanel(
              condition = "input.recommend_type == 'Watched Movie'",
              selectInput("movie", "Select a Movie:", choices = data$Movie.Name)
            ),
            
            conditionalPanel(
              condition = "input.recommend_type == 'Genre'",
              selectInput("genre", "Select a Genre:", choices = unique(data$Genre))
            ),
            
            actionButton("recommend", "Get Recommendations", class = "btn btn-success")
          ),
          mainPanel(
            h4("Recommended Movies:"),
            DTOutput("recommendations")
          )
        )
      )
      
    } else if (page_state() == "visualization") {
      # Visualization Page UI
      fluidPage(
        actionButton("back_to_welcome", "Back", class = "btn btn-light"),
        h2("Data Visualizations", style = "text-align: center; color: #007BFF;"),
        sidebarLayout(
          sidebarPanel(
            selectInput("x_var", "Select X-axis:", choices = c("Budget.INR.", "Revenue.INR.", "Genre")),
            selectInput("y_var", "Select Y-axis:", choices = c("Revenue.INR.", "Budget.INR.", "New.Actor")),
            selectInput("z_var", "Select Z-axis (for 3D):", 
                        choices = c("Budget.INR.", "Revenue.INR.", "New.Actor"), selected = "New.Actor")
          ),
          mainPanel(
            h4("2D Visualization:"),
            plotlyOutput("plot_2d"),
            h4("3D Visualization:"),
            plotlyOutput("plot_3d")
          )
        )
      )
    }
  })
  
  # Login logic
  observeEvent(input$login, {
    user <- input$username
    pass <- input$password
    if (nrow(credentials[credentials$username == user & credentials$password == pass, ]) > 0) {
      logged_in_user(user)
      page_state("welcome")
    } else {
      output$login_message <- renderText("Incorrect username or password. Please try again.")
    }
  })
  
  # Registration logic
  observeEvent(input$register, {
    user <- input$username
    pass <- input$password
    if (user == "" || pass == "") {
      output$login_message <- renderText("Username and password cannot be empty.")
    } else if (nrow(credentials[credentials$username == user, ]) > 0) {
      output$login_message <- renderText("Username already exists. Please choose another.")
    } else {
      update_credentials(user, pass)
      output$login_message <- renderText("Registration successful! You can now log in.")
    }
  })
  
  # Logout logic
  observeEvent(input$logout, {
    logged_in_user(NULL)
    page_state("login")
  })
  
  # Navigation logic
  observeEvent(input$go_to_recommendation, {
    page_state("recommendation")
  })
  observeEvent(input$go_to_visualization, {
    page_state("visualization")
  })
  observeEvent(input$back_to_welcome, {
    page_state("welcome")
  })
  
  # Recommendation logic
  observeEvent(input$recommend, {
    top_recommendations <- data.frame(Movie.Name = character(), Revenue.INR. = numeric())
    
    if (input$recommend_type == "Watched Movie") {
      movie_index <- which(data$Movie.Name == input$movie)
      if (length(movie_index) > 0) {
        movie_similarities <- similarity_matrix[movie_index, ]
        recommended_movies <- order(movie_similarities)
        recommended_movies <- recommended_movies[recommended_movies != movie_index]
        top_recommendations <- data.frame(
          Movie.Name = data$Movie.Name[recommended_movies[1:3]],
          Revenue.INR. = data$Revenue.INR.[recommended_movies[1:3]]
        )
      }
    }  else if (input$recommend_type == "Genre") {
      genre_movies <- data %>%
        filter(Genre == input$genre) %>%
        arrange(desc(Revenue.INR.)) %>%
        head(3)
      top_recommendations <- genre_movies %>%
        select(Movie.Name, Revenue.INR.)
    }
    
    output$recommendations <- renderDT({
      datatable(top_recommendations, options = list(pageLength = 3), rownames = FALSE) %>%
        formatCurrency("Revenue.INR.", currency = "₹")
    })
  })
  
  # Visualization logic
  output$plot_2d <- renderPlotly({
    plot_ly(data, x = ~get(input$x_var), y = ~get(input$y_var), type = 'scatter', mode = 'markers',
            marker = list(size = 10, color = 'rgba(0,123,255,0.6)', line = list(color = 'rgba(0,123,255,1)', width = 2))) %>%
      layout(title = "2D Visualization", xaxis = list(title = input$x_var), yaxis = list(title = input$y_var))
  })
  
  output$plot_3d <- renderPlotly({
    plot_ly(data, x = ~get(input$x_var), y = ~get(input$y_var), z = ~get(input$z_var), type = 'scatter3d', mode = 'markers',
            marker = list(size = 5, color = 'rgba(0,123,255,0.7)')) %>%
      layout(title = "3D Visualization", scene = list(
        xaxis = list(title = input$x_var),
        yaxis = list(title = input$y_var),
        zaxis = list(title = input$z_var)
      ))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
