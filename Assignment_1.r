#Q1
library(rvest)

# Specify the URL of the webpage
url <- "https://www.moneyworks4me.com/best-index/nse-stocks/top-nifty50-companies-list/"

# Read the HTML content of the webpage
page <- read_html(url)

# Extract the table containing the company information
table <- html_nodes(page, "table")

# Extract the data from each row of the table
data <- html_table(table, fill = TRUE)[[1]]

# Remove unwanted rows (e.g., header and footer rows)
data <- data[-c(1, nrow(data)), ]

# Select the relevant columns
data <- data[, 1:12]

# Rename the columns
colnames(data) <- c("Company Name (M.Cap)", "CMP", "Price Change", "Market Cap (Cr)", "52 Week High", "52 Week Low", "ROE", "P/E", "P/BV", "EV/EBITDA", "5YSales Gr(%)", "5YProfit Gr(%)")

# Print the resulting data frame
print(data)



#Q2
library(rvest)
library(dplyr)

# Function to extract data from a company's page
extract_company_data <- function(url) {
  page <- read_html(url)
  
  # Extract the data you need for the specific company
  # Modify the code below to scrape the required information
  # Example:
  sales <- page %>%
    html_nodes("your_selector") %>%
    html_text() %>%
    trimws()
  
  yoy_gr_rt <- page %>%
    html_nodes("your_selector") %>%
    html_text() %>%
    trimws()
  
  adj_eps <- page %>%
    html_nodes("your_selector") %>%
    html_text() %>%
    trimws()
  
  # ... continue extracting other attributes
  
  # Create a data frame for the company's data
  data <- data.frame(
    Sales = sales,
    YoY Gr. Rt. = yoy_gr_rt,
    Adj EPS = adj_eps,
    # ... include other attributes
    stringsAsFactors = FALSE
  )
  
  return(data)
}

# URLs of the chosen companies' pages
company_urls <- c(
  "url_of_company_1",
  "url_of_company_2",
  "url_of_company_3",
  "url_of_company_4",
  "url_of_company_5"
)

# Initialize an empty list to store the data for each company
company_data <- list()

# Scrape data for each company
for (url in company_urls) {
  company_data[[url]] <- extract_company_data(url)
}

# Combine the data for all companies into a single data frame
combined_data <- bind_rows(company_data, .id = "Company")

# Print the resulting data frame
print(combined_data)


#Q3(a)
tennis <- function(p) {
  x <- 0  # Initialize the number of sets played
  while (x < 5) {
    rand <- sample(c(TRUE, FALSE), 1, prob = c(p, 1-p))
    if (rand) {
      x <- x + 1
    } else {
      break
    }
  }
  return(x)
}

#Q3(b)
matches <- numeric(1000)  # Create an empty vector to store the number of matches
for (i in 1:1000) {
  matches[i] <- tennis(0.70)  # Simulate a tennis match and store the number of matches played
}
ans <- mean(matches)  # Calculate the average number of matches

ans  # Output the result



#Q4(1)
MontyHall <- function() {
  doors <- c("car", "goat", "goat")  # Three doors: one with a car and two with goats
  contestant_choice <- sample(1:3, 1)  # Contestant chooses one of the doors randomly
  
  # Monty opens one of the other doors with a goat
  monty_choice <- sample(setdiff(1:3, contestant_choice)[doors[-contestant_choice] == "goat"], 1)
  
  # Contestant switches their choice to the remaining door
  new_choice <- setdiff(1:3, c(contestant_choice, monty_choice))
  
  # Check if the new choice is the door with the car
  if (doors[new_choice] == "car") {
    return(1)  # Contestant wins
  } else {
    return(0)  # Contestant loses
  }
}

#Q4(2)
wins <- 0  # Initialize a counter for wins
for (i in 1:1000) {
  wins <- wins + MontyHall()  # Increment the win counter if the contestant wins
}
probability <- wins / 1000  # Calculate the probability of winning

probability  # Output the result



#Q5
library(rvest)

# URL of the website to scrape
url <- "https://editorial.rottentomatoes.com/guide/best-netflix-movies-to-watch-rightnow/"

# Read the HTML content of the webpage
page <- read_html(url)

# Scrape the ranking, movie name, tomato score, and year
ranking <- page %>% html_nodes(".countdown-index") %>% html_text()
movie_name <- page %>% html_nodes(".article_movie_title a") %>% html_text()
tomato_score <- page %>% html_nodes(".tMeterScore") %>% html_text()
year <- page %>% html_nodes(".start-year") %>% html_text()

# Combine the scraped data into a data frame
movies_data <- data.frame(
  Ranking = ranking,
  Movie_Name = movie_name,
  Tomato_Score = tomato_score,
  Year = year
)

# Print the scraped data
print(movies_data)



#Q6
