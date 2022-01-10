library(palmerpenguins)
data("penguins")
head(penguins)

clean_penguins <- na.omit(penguins)

table(is.na(clean_penguins))

plot(clean_penguins$species)

table(clean_penguins$species)

table(clean_penguins$sex)

summary(clean_penguins[c(3,4,5,6)])

library(dplyr)


table(clean_penguins$species)

set.seed(9850)


penguins_random <- runif(nrow(clean_penguins))


clean_penguins <- clean_penguins[order(penguins_random),]

summary(clean_penguins[,c(3,4,5,6)])

normalize <- function(x) { 
  + return ( (x - min(x))/ (max(x)- min(x)) ) }

penguins_n <- as.data.frame(lapply(clean_penguins[,c(3,4,5,6)], normalize))

summary(penguins_n)

penguins_train <- penguins_n[1:232, ] # 1:129 are rows and , means all columns are inlcuded

penguins_test <- penguins_n[233:333, ] #taking data from iris_n because it has a normalized range

# we will isolate the species to create a training dataset for the target feature

penguins_train_target <- clean_penguins$species[c(1:232)] 
penguins_test_target <- clean_penguins$species[c(233:333)]

require(class)

sqrt(333)

m1 <- knn(train=penguins_train, test=penguins_test, cl=penguins_train_target, k=13)


#if you see knn error train and class have different lengths its due to example_train_target being a df instead of a vector


m1

#checking model accuracy

table(penguins_test_target, m1)


