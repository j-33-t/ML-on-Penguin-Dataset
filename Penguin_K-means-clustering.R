library('stats')
library('dplyr')
library('ggplot2')
library('ggfortify')

library(palmerpenguins)
data("penguins")
head(penguins)


#important to remove na values
clean_penguins <- na.omit(penguins)

mydata <- select(clean_penguins,c(3,4,5,6))



wssplot <- function(data, nc=15, seed = 1234) 
{
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers = i)$withinss)
  } 
  plot(1:nc, wss, type ="b", xlab = "Number of Clusters", 
       ylab="Within group of sum squares")
}




wssplot(mydata)


model_kmeans_cluster <- kmeans(mydata, 2)


autoplot(model_kmeans_cluster, mydata, frame=TRUE)


df_center <- data.frame(model_kmeans_cluster$centers)

df_center

