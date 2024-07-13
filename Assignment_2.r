#Assignment-2
#Q1
data(iris)
install.packages("ggplot2")
library(ggplot2)
ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_boxplot() +
  labs(title = "Boxplots of Sepal Length by Species", x = "Species", y = "Sepal Length")

ggplot(iris, aes(x = Species, y = Petal.Length, fill = Species)) +
  geom_boxplot() +
  labs(title = "Boxplots of Petal Length by Species", x = "Species", y = "Petal Length")
ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
  geom_point() +
  labs(title = "Scatterplot of Sepal Length vs. Petal Length", x = "Sepal Length", y = "Petal Length")

#Q2
install.packages("imager")
library(imager)

flip <- function(image) {
  width <- width(image)
  height <- height(image)
  
  flipped_image <- as.cimg(array(0, dim=c(width, height, 3)))
  
  for (x in 1:width) {
    flipped_image[, x, ] <- image[, width - x + 1, ]
  }
  
  return(flipped_image)
}

#Q3
library(MASS)
data(ships)
accident_counts <- table(ships$type)

barplot(accident_counts, 
        main = "Accidents by Ship Type",
        xlab = "Ship Type",
        ylab = "Number of Accidents",
        col = "skyblue",
        ylim = c(0, max(accident_counts) + 5),
        cex.names = 0.8)

#Q4
install.packages("rvest")
library(rvest)

url <- "https://stats.stackexchange.com/questions?tab=Votes"

page <- read_html(url)

titles <- page %>% 
  html_nodes(".question-hyperlink") %>% 
  html_text()

views <- page %>% 
  html_nodes(".views") %>% 
  html_text() %>% 
  gsub(" views", "", .) %>% 
  as.integer()

answers <- page %>% 
  html_nodes(".status") %>% 
  html_text() %>% 
  gsub(" answers", "", .) %>% 
  as.integer()

votes <- page %>% 
  html_nodes(".vote-count-post") %>% 
  html_text() %>% 
  as.integer()

data <- data.frame(
  Title = titles,
  Views = views,
  Answers = answers,
  Votes = votes
)
print(data)


#Q5
simulate_half_tablet <- function() {
  tablets <- rep(c("whole", "half"), times = c(100, 0))
  
  days <- 0
  
  while (length(tablets) > 0) {
    days <- days + 1
    
    pulled_tablet <- sample(tablets, size = 1)
    
    if (pulled_tablet == "half") {
      break
    } else {
      tablets <- c(tablets, "half")
    }
  }
  
  return(days)
}

num_simulations <- 10000
total_days <- 0
for (i in 1:num_simulations) {
  total_days <- total_days + simulate_half_tablet()
}
average_days <- total_days / num_simulations
average_days


