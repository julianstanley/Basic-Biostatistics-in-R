# Assignment 3
Madeleine Tomasic, Julian Stanley, Patrick Garrity, Emily Navarrete  

Instructions
------------

Please follow the instructions below to complete your assignment:

1. Open the R markdown template **biostatistics-assignmentTemplate.Rmd** in RStudio
2. Insert the relevant information (i.e., assignment number, names of all students in the group)
3. Save the file as **assnNUMBER-yourLastNames.Rmd** replacing **NUMBER** with the assignment \# and replacing **yourLastNames** with the last names of all individuals in the Group. For example **assn1-SmithJohnson.Rmd**
4. Your answers should consist of the R code used to generate the results and their interpretation
5. Do not use any special symbols such as \%, \#, \$, and \& in your answers
6. Generate an HTML version of the Markdown document by clicking on the **Knit** icon in RStudio
7. ONE PERSON in the group: Email an HTML and Markdown version of your completed assignment to your TA
8. ONE PERSON in the group: Upload the Markdown version of your completed assignment to Turnitin on Blackboard. (Failure to complete this will result in a 10% deduction off your grade.)

### Statement of Author contributions
We were each assigned four problems each. After we solved the problems individually (and inevitably messed up in different ways), we went through
each problem together and talked about the questions and corrected mistakes.

Madeleine - Problem 2

Patrick - Problem 1.4-1.6

Emily - Problem 3

Julian - Problem 1, 1.1-1.3

Problem 1
---------

```r
# Read all of the data
benguela.upw <- read.csv("http://faraway.neu.edu/data/assn4_upwelling_benguela.csv")
california.upw <- read.csv("http://faraway.neu.edu/data/assn4_upwelling_california.csv")
canary.upw <- read.csv("http://faraway.neu.edu/data/assn4_upwelling_canary.csv")
humboldt.upw <- read.csv("http://faraway.neu.edu/data/assn4_upwelling_humboldt.csv")

benguela.tmp <- read.csv("http://faraway.neu.edu/data/assn4_temperature_benguela.csv")
humboldt.tmp <- read.csv("http://faraway.neu.edu/data/assn4_temperature_humboldt.csv")
canary.tmp <- read.csv("http://faraway.neu.edu/data/assn4_temperature_canary.csv")
california.tmp <- read.csv("http://faraway.neu.edu/data/assn4_temperature_california.csv")
```


### Part 1

```r
# Upwelling means
benguelaUpwellingMeans <- data.frame(benguela.upw$year, rowMeans(benguela.upw[, 1:22]))
humboldtUpwellingMeans <- data.frame(humboldt.upw$year, rowMeans(humboldt.upw[, 1:22]))
canaryUpwellingMeans <- data.frame(canary.upw$year, rowMeans(canary.upw[, 1:22]))
californiaUpwellingMeans <- data.frame(california.upw$year, rowMeans(california.upw[, 1:22]))

# Temperature means
benguelaTemperatureMeans <- data.frame(benguela.tmp$year, rowMeans(benguela.tmp[, 1:22]))
humboldtTemperatureMeans <- data.frame(humboldt.tmp$year, rowMeans(humboldt.tmp[, 1:22]))
canaryTemperatureMeans <- data.frame(canary.tmp$year, rowMeans(canary.tmp[, 1:22]))
californiaTemperatureMeans <- data.frame(california.tmp$year, rowMeans(california.tmp[, 1:22]))

# Merging upwelling and temperature means
benguela <- merge(benguelaTemperatureMeans, benguelaUpwellingMeans, all.x = TRUE, all.y = TRUE)
benguela$benguela.upw.year <- NULL
colnames(benguela) <- c("Year", "Temperature", "Upwelling")

humboldt <- merge(humboldtTemperatureMeans, humboldtUpwellingMeans, all.x = TRUE, all.y = TRUE)
humboldt$humboldt.upw.year <- NULL
colnames(humboldt) <- c("Year", "Temperature", "Upwelling")

canary <- merge(canaryTemperatureMeans, canaryUpwellingMeans, all.x = TRUE, all.y = TRUE)
canary$canary.upw.year <- NULL
colnames(canary) <- c("Year", "Temperature", "Upwelling")

california <- merge(californiaTemperatureMeans, californiaUpwellingMeans, all.x = TRUE, all.y = TRUE)
california$california.upw.year <- NULL
colnames(california) <- c("Year", "Temperature", "Upwelling")
```


### Part 2


```r
benguela <- cbind(benguela, rep("Benguela", length(benguela)))
colnames(benguela) <- c("Year", "Temperature", "Upwelling", "System")

humboldt <- cbind(humboldt, rep("Humboldt", length(humboldt)))
colnames(humboldt) <- c("Year", "Temperature", "Upwelling", "System")

canary <- cbind(canary, rep("Canary", length(canary)))
colnames(canary) <- c("Year", "Temperature", "Upwelling", "System")

california <- cbind(california, rep("California", length(california)))
colnames(california) <- c("Year", "Temperature", "Upwelling", "System")
```


```r
# Construct main dataframe
```


