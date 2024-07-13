#question1(a) 
library(rvest)
url <- "https://www.moneyworks4me.com/best-index/nse-stocks/top-nifty50-companies-list/"

page <- read_html(url)
table <- html_nodes(page, "table")
data <- html_table(table, fill = TRUE)[[1]]
data <- data[-c(1, nrow(data)), ]
data <- data[, 1:12]
colnames(data) <- c("Company Name (M.Cap)", "CMP", "Price Change", "Market Cap (Cr)", "52 Week High", "52 Week Low", "ROE", "P/E", "P/BV", "EV/EBITDA", "5YSales Gr(%)", "5YProfit Gr(%)")
print(data)


#question2(b)
library(rvest)
library(dplyr)

extract_company_data <- function(url) {
  page <- read_html(url)
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
  
  data <- data.frame(
    Sales = sales,
    YoY Gr. Rt. = yoy_gr_rt,
    Adj EPS = adj_eps,
    stringsAsFactors = FALSE
  )
  
  return(data)
}

company_urls <- c(
  "url_of_company_1",
  "url_of_company_2",
  "url_of_company_3",
  "url_of_company_4",
  "url_of_company_5"
)

company_data <- list()

for (url in company_urls) {
  company_data[[url]] <- extract_company_data(url)
}

combined_data <- bind_rows(company_data, .id = "Company")

print(combined_data)


#question3(c1)
tennis <- function(p) {
  x <- 0 
  winner_found <- FALSE
  while (x < 5 && !winner_found) {
    if (runif(1) < p) {
      x <- x + 1
    } else {
      winner_found <- TRUE
    }
  }
  return(x)
}

#question4(c2)

matches <- numeric(1000) 
for (i in 1:1000) {
  matches[i] <- tennis(0.70)  
  }
ans <- mean(matches) 
print(ans)

#question5(d)

MontyHall <- function() {
  doors <- c("car", "goat", "goat") 
  contestant_choice <- sample(1:3, 1)
  
  # Monty's action: He opens one of the other doors with a goat
  monty_choice <- sample(which(doors != "car" & doors != contestant_choice), 1)
  
  # Contestant switches their choice to the remaining door
  final_choice <- setdiff(1:3, c(contestant_choice, monty_choice))
  
  # Check if the final choice is the car
  if (doors[final_choice] == "car") {
    return(1)  
  } else {
    return(0)  
  }
}

print(MontyHall())

#question6(e)

library(rvest)

url <- "https://editorial.rottentomatoes.com/guide/best-netflix-movies-to-watch-rightnow/"

page <- read_html(url)

ranking <- page %>%
  html_nodes(".countdown-index") %>%
  html_text()

movie_name <- page %>%
  html_nodes(".article_movie_title") %>%
  html_text()

tomato_score <- page %>%
  html_nodes(".tMeterScore") %>%
  html_text()

year <- page %>%
  html_nodes(".start-year") %>%
  html_text()

movies_data <- data.frame(
  Ranking = ranking,
  Movie_Name = movie_name,
  Tomato_Score = tomato_score,
  Year = year
)

print(movies_data)
