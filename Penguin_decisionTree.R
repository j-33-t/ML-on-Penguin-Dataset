install.packages('rpart', repos = "http://cran.us.r-project.org")
install.packages('rpart.plot', repos = "http://cran.us.r-project.org")

library(rpart)
library(rpart.plot)




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


model_decisonTree <- rpart(species ~ ., data=clean_penguins[1:333,], method="class")

model_decisonTree

rpart.plot(model_decisonTree)

rpart.plot(model_decisonTree, type=3, extra=101, fallen.leaves=T)
