#=======================================================================

# Rattle is Copyright (c) 2006-2018 Togaware Pty Ltd.
# It is free (as in libre) open source software.
# It is licensed under the GNU General Public License,
# Version 2. Rattle comes with ABSOLUTELY NO WARRANTY.
# Rattle was written by Graham Williams with contributions
# from others as acknowledged in 'library(help=rattle)'.
# Visit https://rattle.togaware.com/ for details.

#=======================================================================
# Rattle timestamp: 2019-05-08 09:53:41 x86_64-w64-mingw32 

# Rattle version 5.2.0 user 'Akash'

# This log captures interactions with Rattle as an R script. 

# For repeatability, export this activity log to a 
# file, like 'model.R' using the Export button or 
# through the Tools menu. Th script can then serve as a 
# starting point for developing your own scripts. 
# After xporting to a file called 'model.R', for exmample, 
# you can type into a new R Console the command 
# "source('model.R')" and so repeat all actions. Generally, 
# you will want to edit the file to suit your own needs. 
# You can also edit this log in place to record additional 
# information before exporting the script. 
 
# Note that saving/loading projects retains this log.

# We begin most scripts by loading the required packages.
# Here are some initial packages to load and others will be
# identified as we proceed through the script. When writing
# our own scripts we often collect together the library
# commands at the beginning of the script here.

library(rattle)   # Access the weather dataset and utilities.
library(magrittr) # Utilise %>% and %<>% pipeline operators.

# This log generally records the process of building a model. 
# However, with very little effort the log can also be used 
# to score a new dataset. The logical variable 'building' 
# is used to toggle between generating transformations, 
# when building a model and using the transformations, 
# when scoring a dataset.

building <- TRUE
scoring  <- ! building

# A pre-defined value is used to reset the random seed 
# so that results are repeatable.

crv$seed <- 42 

#=======================================================================
# Rattle timestamp: 2019-05-08 10:18:08 x86_64-w64-mingw32 

# Load a dataset from file.

fname         <- "file:///C:/Users/hp/Desktop/svyasa/R/Datasets/Salary_Data.csv" 
crs$dataset <- read.csv(fname,
			na.strings=c(".", "NA", "", "?"),
			strip.white=TRUE, encoding="UTF-8")

#=======================================================================
# Rattle timestamp: 2019-05-08 10:18:09 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=30 train=21 validate=4 test=5

set.seed(crv$seed)

crs$nobs <- nrow(crs$dataset)

crs$train <- sample(crs$nobs, 0.7*crs$nobs)

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  sample(0.15*crs$nobs) ->
crs$validate

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  setdiff(crs$validate) ->
crs$test

# The following variable selections have been noted.

crs$input     <- c("YearsExperience", "Salary")

crs$numeric   <- c("YearsExperience", "Salary")

crs$categoric <- NULL

crs$target    <- NULL
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- NULL
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2019-05-08 10:23:10 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=30 train=21 validate=4 test=5

set.seed(crv$seed)

crs$nobs <- nrow(crs$dataset)

crs$train <- sample(crs$nobs, 0.7*crs$nobs)

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  sample(0.15*crs$nobs) ->
crs$validate

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  setdiff(crs$validate) ->
crs$test

# The following variable selections have been noted.

crs$input     <- "YearsExperience"

crs$numeric   <- "YearsExperience"

crs$categoric <- NULL

crs$target    <- "Salary"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- NULL
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2019-05-08 10:24:15 x86_64-w64-mingw32 

# The 'Hmisc' package provides the 'contents' function.

library(Hmisc, quietly=TRUE)

# Obtain a summary of the dataset.

contents(crs$dataset[crs$train, c(crs$input, crs$risk, crs$target)])
summary(crs$dataset[crs$train, c(crs$input, crs$risk, crs$target)])

#=======================================================================
# Rattle timestamp: 2019-05-08 10:25:39 x86_64-w64-mingw32 

# Display histogram plots for the selected variables. 

# Use ggplot2 to generate histogram plot for YearsExperience

# Generate the plot.

p01 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::select(YearsExperience) %>%
  ggplot2::ggplot(ggplot2::aes(x=YearsExperience)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("YearsExperience\n\nRattle 2019-May-08 10:25:39 Akash") +
  ggplot2::ggtitle("Distribution of YearsExperience (sample)") +
  ggplot2::labs(y="Density")

# Use ggplot2 to generate histogram plot for Salary

# Generate the plot.

p02 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::select(Salary) %>%
  ggplot2::ggplot(ggplot2::aes(x=Salary)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("Salary\n\nRattle 2019-May-08 10:25:39 Akash") +
  ggplot2::ggtitle("Distribution of Salary (sample)") +
  ggplot2::labs(y="Density")

# Display the plots.

gridExtra::grid.arrange(p01, p02)

#=======================================================================
# Rattle timestamp: 2019-05-08 10:29:28 x86_64-w64-mingw32 

# Generate a correlation plot for the variables. 

# The 'corrplot' package provides the 'corrplot' function.

library(corrplot, quietly=TRUE)

# Correlations work for numeric variables only.

crs$cor <- cor(crs$dataset[crs$train, crs$numeric], use="pairwise", method="pearson")

# Order the correlations by their strength.

crs$ord <- order(crs$cor[1,])
crs$cor <- crs$cor[crs$ord, crs$ord]

# Display the actual correlations.

print(crs$cor)

# Graphically display the correlations.

corrplot(crs$cor, mar=c(0,0,1,0))
title(main="Correlation Salary_Data.csv using Pearson",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

#=======================================================================
# Rattle timestamp: 2019-05-08 10:35:38 x86_64-w64-mingw32 

# Regression model 

# Build a Regression model.

crs$glm <- lm(Salary ~ ., data=crs$dataset[crs$train,c(crs$input, crs$target)])

# Generate a textual view of the Linear model.

print(summary(crs$glm))
cat('==== ANOVA ====

')
print(anova(crs$glm))
print("
")

# Time taken: 0.07 secs

#=======================================================================
# Rattle timestamp: 2019-05-08 10:53:01 x86_64-w64-mingw32 

# Save the project data (variable crs) to file.

save(crs, file="C:\Users\hp\Desktop\Salary_Data.rattle", compress=TRUE)

#=======================================================================
# Rattle timestamp: 2019-05-08 11:43:22 x86_64-w64-mingw32 

# Display histogram plots for the selected variables. 

# Use ggplot2 to generate histogram plot for YearsExperience

# Generate the plot.

p01 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::select(YearsExperience) %>%
  ggplot2::ggplot(ggplot2::aes(x=YearsExperience)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("YearsExperience\n\nRattle 2019-May-08 11:43:22 Akash") +
  ggplot2::ggtitle("Distribution of YearsExperience (sample)") +
  ggplot2::labs(y="Density")

# Use ggplot2 to generate histogram plot for Salary

# Generate the plot.

p02 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::select(Salary) %>%
  ggplot2::ggplot(ggplot2::aes(x=Salary)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("Salary\n\nRattle 2019-May-08 11:43:22 Akash") +
  ggplot2::ggtitle("Distribution of Salary (sample)") +
  ggplot2::labs(y="Density")

# Display the plots.

gridExtra::grid.arrange(p01, p02)

#=======================================================================
# Rattle timestamp: 2019-05-08 11:43:33 x86_64-w64-mingw32 

# Display histogram plots for the selected variables. 

# Use ggplot2 to generate histogram plot for YearsExperience

# Generate the plot.

p01 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::select(YearsExperience) %>%
  ggplot2::ggplot(ggplot2::aes(x=YearsExperience)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("YearsExperience\n\nRattle 2019-May-08 11:43:33 Akash") +
  ggplot2::ggtitle("Distribution of YearsExperience (sample)") +
  ggplot2::labs(y="Density")

# Use ggplot2 to generate histogram plot for Salary

# Generate the plot.

p02 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::select(Salary) %>%
  ggplot2::ggplot(ggplot2::aes(x=Salary)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("Salary\n\nRattle 2019-May-08 11:43:33 Akash") +
  ggplot2::ggtitle("Distribution of Salary (sample)") +
  ggplot2::labs(y="Density")

# Display the plots.

gridExtra::grid.arrange(p01, p02)

#=======================================================================
# Rattle timestamp: 2019-05-08 11:44:00 x86_64-w64-mingw32 

# Load a dataset from file.

fname         <- "file:///C:/Users/hp/Desktop/svyasa/R/Datasets/Advertising.csv" 
crs$dataset <- read.csv(fname,
			na.strings=c(".", "NA", "", "?"),
			strip.white=TRUE, encoding="UTF-8")

#=======================================================================
# Rattle timestamp: 2019-05-08 11:44:00 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=200 train=140 validate=30 test=30

set.seed(crv$seed)

crs$nobs <- nrow(crs$dataset)

crs$train <- sample(crs$nobs, 0.7*crs$nobs)

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  sample(0.15*crs$nobs) ->
crs$validate

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  setdiff(crs$validate) ->
crs$test

# The following variable selections have been noted.

crs$input     <- c("Sl", "TV", "radio", "newspaper", "sales")

crs$numeric   <- c("Sl", "TV", "radio", "newspaper", "sales")

crs$categoric <- NULL

crs$target    <- NULL
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- NULL
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2019-05-08 11:44:12 x86_64-w64-mingw32 

# The 'Hmisc' package provides the 'contents' function.

library(Hmisc, quietly=TRUE)

# Obtain a summary of the dataset.

contents(crs$dataset[crs$train, c(crs$input, crs$risk, crs$target)])
summary(crs$dataset[crs$train, c(crs$input, crs$risk, crs$target)])

#=======================================================================
# Rattle timestamp: 2019-05-08 11:44:25 x86_64-w64-mingw32 

# Display histogram plots for the selected variables. 

# Use ggplot2 to generate histogram plot for Sl

# Generate the plot.

p01 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::select(Sl) %>%
  ggplot2::ggplot(ggplot2::aes(x=Sl)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("Sl\n\nRattle 2019-May-08 11:44:25 Akash") +
  ggplot2::ggtitle("Distribution of Sl (sample)") +
  ggplot2::labs(y="Density")

# Use ggplot2 to generate histogram plot for TV

# Generate the plot.

p02 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::select(TV) %>%
  ggplot2::ggplot(ggplot2::aes(x=TV)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("TV\n\nRattle 2019-May-08 11:44:25 Akash") +
  ggplot2::ggtitle("Distribution of TV (sample)") +
  ggplot2::labs(y="Density")

# Use ggplot2 to generate histogram plot for radio

# Generate the plot.

p03 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::select(radio) %>%
  ggplot2::ggplot(ggplot2::aes(x=radio)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("radio\n\nRattle 2019-May-08 11:44:25 Akash") +
  ggplot2::ggtitle("Distribution of radio (sample)") +
  ggplot2::labs(y="Density")

# Use ggplot2 to generate histogram plot for newspaper

# Generate the plot.

p04 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::select(newspaper) %>%
  ggplot2::ggplot(ggplot2::aes(x=newspaper)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("newspaper\n\nRattle 2019-May-08 11:44:25 Akash") +
  ggplot2::ggtitle("Distribution of newspaper (sample)") +
  ggplot2::labs(y="Density")

# Use ggplot2 to generate histogram plot for sales

# Generate the plot.

p05 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::select(sales) %>%
  ggplot2::ggplot(ggplot2::aes(x=sales)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("sales\n\nRattle 2019-May-08 11:44:25 Akash") +
  ggplot2::ggtitle("Distribution of sales (sample)") +
  ggplot2::labs(y="Density")

# Display the plots.

gridExtra::grid.arrange(p01, p02, p03, p04, p05)

#=======================================================================
# Rattle timestamp: 2019-05-08 11:50:08 x86_64-w64-mingw32 

# Load a dataset from file.

fname         <- "file:///C:/Users/hp/Desktop/svyasa/R/Datasets/divusa.csv" 
crs$dataset <- read.csv(fname,
			na.strings=c(".", "NA", "", "?"),
			strip.white=TRUE, encoding="UTF-8")

#=======================================================================
# Rattle timestamp: 2019-05-08 11:50:08 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=77 train=54 validate=12 test=11

set.seed(crv$seed)

crs$nobs <- nrow(crs$dataset)

crs$train <- sample(crs$nobs, 0.7*crs$nobs)

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  sample(0.15*crs$nobs) ->
crs$validate

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  setdiff(crs$validate) ->
crs$test

# The following variable selections have been noted.

crs$input     <- c("X", "year", "divorce", "unemployed", "femlab",
                   "marriage", "birth", "military")

crs$numeric   <- c("X", "year", "divorce", "unemployed", "femlab",
                   "marriage", "birth", "military")

crs$categoric <- NULL

crs$target    <- NULL
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- NULL
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2019-05-08 11:50:24 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=77 train=54 validate=12 test=11

set.seed(crv$seed)

crs$nobs <- nrow(crs$dataset)

crs$train <- sample(crs$nobs, 0.7*crs$nobs)

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  sample(0.15*crs$nobs) ->
crs$validate

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  setdiff(crs$validate) ->
crs$test

# The following variable selections have been noted.

crs$input     <- c("X", "year", "divorce", "unemployed", "femlab",
                   "marriage", "birth", "military")

crs$numeric   <- c("X", "year", "divorce", "unemployed", "femlab",
                   "marriage", "birth", "military")

crs$categoric <- NULL

crs$target    <- NULL
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- NULL
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2019-05-08 11:50:32 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=77 train=54 validate=12 test=11

set.seed(crv$seed)

crs$nobs <- nrow(crs$dataset)

crs$train <- sample(crs$nobs, 0.7*crs$nobs)

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  sample(0.15*crs$nobs) ->
crs$validate

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  setdiff(crs$validate) ->
crs$test

# The following variable selections have been noted.

crs$input     <- c("X", "year", "divorce", "unemployed", "femlab",
                   "marriage", "birth", "military")

crs$numeric   <- c("X", "year", "divorce", "unemployed", "femlab",
                   "marriage", "birth", "military")

crs$categoric <- NULL

crs$target    <- NULL
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- NULL
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2019-05-08 11:50:39 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=77 train=54 validate=12 test=11

set.seed(crv$seed)

crs$nobs <- nrow(crs$dataset)

crs$train <- sample(crs$nobs, 0.7*crs$nobs)

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  sample(0.15*crs$nobs) ->
crs$validate

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  setdiff(crs$validate) ->
crs$test

# The following variable selections have been noted.

crs$input     <- c("X", "year", "divorce", "unemployed", "femlab",
                   "marriage", "birth", "military")

crs$numeric   <- c("X", "year", "divorce", "unemployed", "femlab",
                   "marriage", "birth", "military")

crs$categoric <- NULL

crs$target    <- NULL
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- NULL
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2019-05-08 11:50:43 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=77 train=54 validate=12 test=11

set.seed(crv$seed)

crs$nobs <- nrow(crs$dataset)

crs$train <- sample(crs$nobs, 0.7*crs$nobs)

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  sample(0.15*crs$nobs) ->
crs$validate

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  setdiff(crs$validate) ->
crs$test

# The following variable selections have been noted.

crs$input     <- c("X", "year", "divorce", "unemployed", "femlab",
                   "marriage", "birth", "military")

crs$numeric   <- c("X", "year", "divorce", "unemployed", "femlab",
                   "marriage", "birth", "military")

crs$categoric <- NULL

crs$target    <- NULL
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- NULL
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2019-05-08 11:51:51 x86_64-w64-mingw32 

# Display histogram plots for the selected variables. 

# Use ggplot2 to generate histogram plot for X

# Generate the plot.

p01 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::select(X) %>%
  ggplot2::ggplot(ggplot2::aes(x=X)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("X\n\nRattle 2019-May-08 11:51:51 Akash") +
  ggplot2::ggtitle("Distribution of X (sample)") +
  ggplot2::labs(y="Density")

# Use ggplot2 to generate histogram plot for year

# Generate the plot.

p02 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::select(year) %>%
  ggplot2::ggplot(ggplot2::aes(x=year)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("year\n\nRattle 2019-May-08 11:51:51 Akash") +
  ggplot2::ggtitle("Distribution of year (sample)") +
  ggplot2::labs(y="Density")

# Use ggplot2 to generate histogram plot for divorce

# Generate the plot.

p03 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::select(divorce) %>%
  ggplot2::ggplot(ggplot2::aes(x=divorce)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("divorce\n\nRattle 2019-May-08 11:51:51 Akash") +
  ggplot2::ggtitle("Distribution of divorce (sample)") +
  ggplot2::labs(y="Density")

# Use ggplot2 to generate histogram plot for unemployed

# Generate the plot.

p04 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::select(unemployed) %>%
  ggplot2::ggplot(ggplot2::aes(x=unemployed)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("unemployed\n\nRattle 2019-May-08 11:51:51 Akash") +
  ggplot2::ggtitle("Distribution of unemployed (sample)") +
  ggplot2::labs(y="Density")

# Use ggplot2 to generate histogram plot for femlab

# Generate the plot.

p05 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::select(femlab) %>%
  ggplot2::ggplot(ggplot2::aes(x=femlab)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("femlab\n\nRattle 2019-May-08 11:51:51 Akash") +
  ggplot2::ggtitle("Distribution of femlab (sample)") +
  ggplot2::labs(y="Density")

# Use ggplot2 to generate histogram plot for marriage

# Generate the plot.

p06 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::select(marriage) %>%
  ggplot2::ggplot(ggplot2::aes(x=marriage)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("marriage\n\nRattle 2019-May-08 11:51:51 Akash") +
  ggplot2::ggtitle("Distribution of marriage (sample)") +
  ggplot2::labs(y="Density")

# Use ggplot2 to generate histogram plot for birth

# Generate the plot.

p07 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::select(birth) %>%
  ggplot2::ggplot(ggplot2::aes(x=birth)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("birth\n\nRattle 2019-May-08 11:51:51 Akash") +
  ggplot2::ggtitle("Distribution of birth (sample)") +
  ggplot2::labs(y="Density")

# Use ggplot2 to generate histogram plot for military

# Generate the plot.

p08 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::select(military) %>%
  ggplot2::ggplot(ggplot2::aes(x=military)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("military\n\nRattle 2019-May-08 11:51:51 Akash") +
  ggplot2::ggtitle("Distribution of military (sample)") +
  ggplot2::labs(y="Density")

# Display the plots.

gridExtra::grid.arrange(p01, p02, p03, p04, p05, p06, p07, p08)

#=======================================================================
# Rattle timestamp: 2019-05-08 11:53:11 x86_64-w64-mingw32 

# Save the project data (variable crs) to file.

save(crs, file="C:/Users\hp\Desktop\divusa.rattle", compress=TRUE)

#=======================================================================
# Rattle timestamp: 2019-05-08 12:02:54 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=77 train=54 validate=12 test=11

set.seed(crv$seed)

crs$nobs <- nrow(crs$dataset)

crs$train <- sample(crs$nobs, 0.7*crs$nobs)

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  sample(0.15*crs$nobs) ->
crs$validate

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  setdiff(crs$validate) ->
crs$test

# The following variable selections have been noted.

crs$input     <- c("unemployed", "femlab", "marriage", "birth",
                   "military")

crs$numeric   <- c("unemployed", "femlab", "marriage", "birth",
                   "military")

crs$categoric <- NULL

crs$target    <- "divorce"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- c("X", "year")
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2019-05-08 12:03:00 x86_64-w64-mingw32 

# The 'Hmisc' package provides the 'contents' function.

library(Hmisc, quietly=TRUE)

# Obtain a summary of the dataset.

contents(crs$dataset[crs$train, c(crs$input, crs$risk, crs$target)])
summary(crs$dataset[crs$train, c(crs$input, crs$risk, crs$target)])

#=======================================================================
# Rattle timestamp: 2019-05-08 12:03:07 x86_64-w64-mingw32 

# Display histogram plots for the selected variables. 

# Use ggplot2 to generate histogram plot for X

# Generate the plot.

p01 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::select(X) %>%
  ggplot2::ggplot(ggplot2::aes(x=X)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("X\n\nRattle 2019-May-08 12:03:07 Akash") +
  ggplot2::ggtitle("Distribution of X (sample)") +
  ggplot2::labs(y="Density")

# Use ggplot2 to generate histogram plot for year

# Generate the plot.

p02 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::select(year) %>%
  ggplot2::ggplot(ggplot2::aes(x=year)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("year\n\nRattle 2019-May-08 12:03:07 Akash") +
  ggplot2::ggtitle("Distribution of year (sample)") +
  ggplot2::labs(y="Density")

# Use ggplot2 to generate histogram plot for divorce

# Generate the plot.

p03 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::select(divorce) %>%
  ggplot2::ggplot(ggplot2::aes(x=divorce)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("divorce\n\nRattle 2019-May-08 12:03:07 Akash") +
  ggplot2::ggtitle("Distribution of divorce (sample)") +
  ggplot2::labs(y="Density")

# Use ggplot2 to generate histogram plot for unemployed

# Generate the plot.

p04 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::select(unemployed) %>%
  ggplot2::ggplot(ggplot2::aes(x=unemployed)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("unemployed\n\nRattle 2019-May-08 12:03:07 Akash") +
  ggplot2::ggtitle("Distribution of unemployed (sample)") +
  ggplot2::labs(y="Density")

# Use ggplot2 to generate histogram plot for femlab

# Generate the plot.

p05 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::select(femlab) %>%
  ggplot2::ggplot(ggplot2::aes(x=femlab)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("femlab\n\nRattle 2019-May-08 12:03:07 Akash") +
  ggplot2::ggtitle("Distribution of femlab (sample)") +
  ggplot2::labs(y="Density")

# Use ggplot2 to generate histogram plot for marriage

# Generate the plot.

p06 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::select(marriage) %>%
  ggplot2::ggplot(ggplot2::aes(x=marriage)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("marriage\n\nRattle 2019-May-08 12:03:07 Akash") +
  ggplot2::ggtitle("Distribution of marriage (sample)") +
  ggplot2::labs(y="Density")

# Use ggplot2 to generate histogram plot for birth

# Generate the plot.

p07 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::select(birth) %>%
  ggplot2::ggplot(ggplot2::aes(x=birth)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("birth\n\nRattle 2019-May-08 12:03:07 Akash") +
  ggplot2::ggtitle("Distribution of birth (sample)") +
  ggplot2::labs(y="Density")

# Use ggplot2 to generate histogram plot for military

# Generate the plot.

p08 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::select(military) %>%
  ggplot2::ggplot(ggplot2::aes(x=military)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("military\n\nRattle 2019-May-08 12:03:07 Akash") +
  ggplot2::ggtitle("Distribution of military (sample)") +
  ggplot2::labs(y="Density")

# Display the plots.

gridExtra::grid.arrange(p01, p02, p03, p04, p05, p06, p07, p08)

#=======================================================================
# Rattle timestamp: 2019-05-08 12:03:15 x86_64-w64-mingw32 

# Regression model 

# Build a Regression model.

crs$glm <- lm(divorce ~ ., data=crs$dataset[crs$train,c(crs$input, crs$target)])

# Generate a textual view of the Linear model.

print(summary(crs$glm))
cat('==== ANOVA ====

')
print(anova(crs$glm))
print("
")

# Time taken: 0.02 secs

#=======================================================================
# Rattle timestamp: 2019-05-08 12:11:25 x86_64-w64-mingw32 

# Load a dataset from file.

fname         <- "file:///C:/Users/hp/Desktop/svyasa/R/Datasets/Advertising.csv" 
crs$dataset <- read.csv(fname,
			na.strings=c(".", "NA", "", "?"),
			strip.white=TRUE, encoding="UTF-8")

#=======================================================================
# Rattle timestamp: 2019-05-08 12:11:26 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=200 train=140 validate=30 test=30

set.seed(crv$seed)

crs$nobs <- nrow(crs$dataset)

crs$train <- sample(crs$nobs, 0.7*crs$nobs)

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  sample(0.15*crs$nobs) ->
crs$validate

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  setdiff(crs$validate) ->
crs$test

# The following variable selections have been noted.

crs$input     <- c("Sl", "TV", "radio", "newspaper", "sales")

crs$numeric   <- c("Sl", "TV", "radio", "newspaper", "sales")

crs$categoric <- NULL

crs$target    <- NULL
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- NULL
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2019-05-08 12:11:39 x86_64-w64-mingw32 

# The 'Hmisc' package provides the 'contents' function.

library(Hmisc, quietly=TRUE)

# Obtain a summary of the dataset.

contents(crs$dataset[crs$train, c(crs$input, crs$risk, crs$target)])
summary(crs$dataset[crs$train, c(crs$input, crs$risk, crs$target)])

#=======================================================================
# Rattle timestamp: 2019-05-08 12:12:02 x86_64-w64-mingw32 

# Display histogram plots for the selected variables. 

# Use ggplot2 to generate histogram plot for Sl

# Generate the plot.

p01 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::select(Sl) %>%
  ggplot2::ggplot(ggplot2::aes(x=Sl)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("Sl\n\nRattle 2019-May-08 12:12:02 Akash") +
  ggplot2::ggtitle("Distribution of Sl (sample)") +
  ggplot2::labs(y="Density")

# Use ggplot2 to generate histogram plot for TV

# Generate the plot.

p02 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::select(TV) %>%
  ggplot2::ggplot(ggplot2::aes(x=TV)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("TV\n\nRattle 2019-May-08 12:12:02 Akash") +
  ggplot2::ggtitle("Distribution of TV (sample)") +
  ggplot2::labs(y="Density")

# Use ggplot2 to generate histogram plot for radio

# Generate the plot.

p03 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::select(radio) %>%
  ggplot2::ggplot(ggplot2::aes(x=radio)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("radio\n\nRattle 2019-May-08 12:12:02 Akash") +
  ggplot2::ggtitle("Distribution of radio (sample)") +
  ggplot2::labs(y="Density")

# Use ggplot2 to generate histogram plot for newspaper

# Generate the plot.

p04 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::select(newspaper) %>%
  ggplot2::ggplot(ggplot2::aes(x=newspaper)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("newspaper\n\nRattle 2019-May-08 12:12:02 Akash") +
  ggplot2::ggtitle("Distribution of newspaper (sample)") +
  ggplot2::labs(y="Density")

# Use ggplot2 to generate histogram plot for sales

# Generate the plot.

p05 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::select(sales) %>%
  ggplot2::ggplot(ggplot2::aes(x=sales)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("sales\n\nRattle 2019-May-08 12:12:02 Akash") +
  ggplot2::ggtitle("Distribution of sales (sample)") +
  ggplot2::labs(y="Density")

# Display the plots.

gridExtra::grid.arrange(p01, p02, p03, p04, p05)

#=======================================================================
# Rattle timestamp: 2019-05-08 12:14:41 x86_64-w64-mingw32 

# Load a dataset from file.

fname         <- "file:///C:/Users/hp/Desktop/svyasa/R/Datasets/HeatCapacity.csv" 
crs$dataset <- read.csv(fname,
			na.strings=c(".", "NA", "", "?"),
			strip.white=TRUE, encoding="UTF-8")

#=======================================================================
# Rattle timestamp: 2019-05-08 12:14:42 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=12 train=8 validate=2 test=2

set.seed(crv$seed)

crs$nobs <- nrow(crs$dataset)

crs$train <- sample(crs$nobs, 0.7*crs$nobs)

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  sample(0.15*crs$nobs) ->
crs$validate

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  setdiff(crs$validate) ->
crs$test

# The following variable selections have been noted.

crs$input     <- c("X.U.FEFF.T...C.", "Sp..Enth...kJ.kg.")

crs$numeric   <- c("X.U.FEFF.T...C.", "Sp..Enth...kJ.kg.")

crs$categoric <- NULL

crs$target    <- NULL
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- NULL
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2019-05-08 12:14:49 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=12 train=8 validate=2 test=2

set.seed(crv$seed)

crs$nobs <- nrow(crs$dataset)

crs$train <- sample(crs$nobs, 0.7*crs$nobs)

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  sample(0.15*crs$nobs) ->
crs$validate

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  setdiff(crs$validate) ->
crs$test

# The following variable selections have been noted.

crs$input     <- "X.U.FEFF.T...C."

crs$numeric   <- "X.U.FEFF.T...C."

crs$categoric <- NULL

crs$target    <- "Sp..Enth...kJ.kg."
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- NULL
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2019-05-08 12:14:55 x86_64-w64-mingw32 

# The 'Hmisc' package provides the 'contents' function.

library(Hmisc, quietly=TRUE)

# Obtain a summary of the dataset.

contents(crs$dataset[crs$train, c(crs$input, crs$risk, crs$target)])
summary(crs$dataset[crs$train, c(crs$input, crs$risk, crs$target)])

#=======================================================================
# Rattle timestamp: 2019-05-08 12:15:04 x86_64-w64-mingw32 

# Display histogram plots for the selected variables. 

# Use ggplot2 to generate histogram plot for X.U.FEFF.T...C.

# Generate the plot.

p01 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::select(X.U.FEFF.T...C.) %>%
  ggplot2::ggplot(ggplot2::aes(x=X.U.FEFF.T...C.)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("X.U.FEFF.T...C.\n\nRattle 2019-May-08 12:15:04 Akash") +
  ggplot2::ggtitle("Distribution of X.U.FEFF.T...C. (sample)") +
  ggplot2::labs(y="Density")

# Use ggplot2 to generate histogram plot for Sp..Enth...kJ.kg.

# Generate the plot.

p02 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::select(Sp..Enth...kJ.kg.) %>%
  ggplot2::ggplot(ggplot2::aes(x=Sp..Enth...kJ.kg.)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("Sp..Enth...kJ.kg.\n\nRattle 2019-May-08 12:15:04 Akash") +
  ggplot2::ggtitle("Distribution of Sp..Enth...kJ.kg. (sample)") +
  ggplot2::labs(y="Density")

# Display the plots.

gridExtra::grid.arrange(p01, p02)

#=======================================================================
# Rattle timestamp: 2019-05-08 12:15:17 x86_64-w64-mingw32 

# Decision Tree 

# The 'rpart' package provides the 'rpart' function.

library(rpart, quietly=TRUE)

# Reset the random number seed to obtain the same results each time.

set.seed(crv$seed)

# Build the Decision Tree model.

crs$rpart <- rpart(Sp..Enth...kJ.kg. ~ .,
    data=crs$dataset[crs$train, c(crs$input, crs$target)],
    method="anova",
    parms=list(split="information"),
    control=rpart.control(usesurrogate=0, 
        maxsurrogate=0),
    model=TRUE)

# Generate a textual view of the Decision Tree model.

print(crs$rpart)
printcp(crs$rpart)
cat("\n")

# Time taken: 0.03 secs

#=======================================================================
# Rattle timestamp: 2019-05-08 12:15:30 x86_64-w64-mingw32 

# Regression model 

# Build a Regression model.

crs$glm <- lm(Sp..Enth...kJ.kg. ~ ., data=crs$dataset[crs$train,c(crs$input, crs$target)])

# Generate a textual view of the Linear model.

print(summary(crs$glm))
cat('==== ANOVA ====

')
print(anova(crs$glm))
print("
")

# Time taken: 0.02 secs
