#Question1
library(ggplot2)

data(iris)
ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
  geom_point() +
  labs(x = "Sepal Length", y = "Petal Length", title = "Scatterplot of Sepal Length vs. Petal Length") +
  scale_color_manual(values = c("setosa" = "red", "versicolor" = "green", "virginica" = "blue"))


#Question2
library(ggplot2)
data(txhousing)
str(txhousing)
summary(txhousing)

missing_values <- sum(!complete.cases(txhousing))
cat("Number of missing values:", missing_values, "\n")

ggplot(txhousing, aes(x = year, y = median)) +
  geom_point() +
  labs(x = "Year", y = "Median Housing Price") +
  ggtitle("Scatterplot of Median Housing Price vs. Year")

ggplot(txhousing, aes(x = factor(month), y = median)) +
  geom_boxplot() +
  labs(x = "Month", y = "Median Housing Price") +
  ggtitle("Boxplot of Median Housing Price by Month")

ggplot(txhousing, aes(x = factor(year), fill = factor(month))) +
  geom_bar(position = "dodge") +
  labs(x = "Year", y = "Number of Houses Sold") +
  ggtitle("Barplot of Number of Houses Sold by Year and Month")



#Question3
library(ggplot2)
titanic <- read.csv("titanic.csv")

titanic <- na.omit(titanic)
ggplot(titanic, aes(x = Fare, y = factor(Survived), fill = factor(Sex))) +
  geom_boxplot() +
  scale_y_discrete(labels = c("Survived", "Died")) +
  scale_fill_manual(values = c("male" = "lightblue", "female" = "pink")) +
  ylab("Survival") +
  xlab("Fare") +
  ggtitle("Box Plot of Survival vs Fare by Gender") +
  theme_minimal()
