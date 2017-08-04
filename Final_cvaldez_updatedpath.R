#######################################################################
### Name: Christopher Valdez
### Date: December 15, 2016
### MET CS 544, Fall 2 2016 - Final Project
#######################################################################

#### Install packages and load libraries.####
# Install packages.
install.packages("data.table", dependencies = TRUE)
install.packages("stringr", dependencies = TRUE)
install.packages("tidyr", dependencies = TRUE)
install.packages("plyr", dependencies = TRUE)
install.packages("dplyr", dependencies = TRUE)
install.packages("RColorBrewer", dependencies = TRUE)
install.packages("sampling", dependencies = TRUE)

# Load libraries.
library(stringr)
suppressMessages(library(data.table))
suppressMessages(library(plyr))
suppressMessages(library(dplyr))
library(tidyr)
suppressMessages(library(RColorBrewer))
library(sampling)
library(boot)
### ***** End installing packages and loading libraries. ***** ###

#### ***** Start importing data. ***** ####
path = "~/Documents/Education/Grad School/Fall 2 2016/MET CS 544/Final Project/CS544Final_valdez/project_data/2006_01_03/"
setwd(path) ; getwd();

file.list <- list.files(recursive = TRUE, include.dirs = FALSE)
length(file.list)

# Create a progress bar; output visible in RStudio.
# Source: M. Heckmann. (2009). "R: Monitoring the function progress with a progress bar."
# Retrieved from: https://ryouready.wordpress.com/2009/03/16/r-monitor-function-progress-with-a-progress-bar/
n <- length(file.list)
pb <- txtProgressBar(min = 0, max = n, style = 3)

# Set start time.
ptm <- proc.time()

## --- Begin outer for loop; data processing   
for (i in 1:length(file.list)) {
    
    # Use conditional to identify first data frame to be created.
    if (i == 1) {
        
        # Assign file path to variable.
        file <- file.list[i]
        
        # Increment progress bar.
        setTxtProgressBar(pb, i)

        # Read in file contents using scan function to wow.data list; ignore first two lines.
        wow.data <- scan(file, what = character(), sep = ",", skip = 2, skipNul = TRUE ) ; wow.data
        
        # Find the location of the closing braces
        list <- gregexpr(pattern = "}", wow.data)

        # Store the entries in 'list' which match the closing brace pattern.
        index.list <- which(list[]>0)
        
        # Create a new list removing the contents of wow.data starting with the first closing bracket.
        new.wow.data <- wow.data[c(-index.list[1]:-index.list[2])]

        # Further trim list by removing the double quotes; based on consistent pattern starting with string and double quotes.
        new.wow.data <- new.wow.data[seq(from = 1, to = length(new.wow.data), by = 2)]

        # Split new.wow.data at each comma.
        data.sep <- strsplit(new.wow.data, ",")
        
        # Remove white space before and after each element in data.sep.
        for (i in 1:length(data.sep)) {    
            for (j in 1:length(data.sep[[i]])) {     
                data.sep[[i]][j] <- trimws(data.sep[[i]][j], which = "both")     
            }
        }

        # Replace empty strings with NA.
        for (i in 1:length(data.sep)) {
            is.na(data.sep[[i]]) <- data.sep[[i]] == ""
        }
        
        # Remove 'dummy' columns by subsetting columns want to keep (2:4,6:9).
        for (i in 1:length(data.sep)) {    
            data.sep[[i]] <- data.sep[[i]][c(2:9)]    
        }
        
        # Collapse all of the values of 'data.sep' into a vector.
        data.sep_L <- lapply(data.sep, 
                            function(r_L) {
                                sapply(r_L, function(x) x[1])
                            })
        
        # Pack into data.frame.
        wow_data <- ldply(data.sep_L, function(ls) ls)
    
        # Assign appropriate column names to data for data frame, dat.
        colnames(wow_data) <- c("Query_Time", "Query_Seq_#", "Avatar_ID", "Guild", "Level", "Race", "Class", "Zone")
                                
    }  ### -- End if clause --
    
    # Start block if other data frames need to be processed.
    else {
        
        # Assign file path to variable.
        file <- file.list[i]
        
        # Increment progress bar.
        setTxtProgressBar(pb, i)
 
        # Read in file contents using scan function to wow.data list; ignore first two lines.
        temp_data <- scan(file, what = character(), sep = ",", skip = 2, skipNul = TRUE )

        # Find the location of the closing braces
        list <- gregexpr(pattern = "}", temp_data)

        # Store the entries in 'list' which match the closing brace pattern.
        index.list <- which(list[]>0);

        # Create a new list removing the contents of wow.data starting with the first closing bracket.
        new.wow.data <- temp_data[c(-index.list[1]:-index.list[2])]

        # Further trim list by removing the double quotes; based on consistent pattern starting with string and double quotes.
        new.wow.data <- new.wow.data[seq(from = 1, to = length(new.wow.data), by = 2)]

        # Split new.wow.data at each comma.
        data.sep <- strsplit(new.wow.data, ",")

        # Remove white space before and after each element in data.sep.
        for (i in 1:length(data.sep)) {    
            for (j in 1:length(data.sep[[i]])) {     
                data.sep[[i]][j] <- trimws(data.sep[[i]][j], which = "both")     
            }
        }

        # Replace empty strings with NA.
        for (i in 1:length(data.sep)) {
            is.na(data.sep[[i]]) <- data.sep[[i]] == "" 
        }

        # Remove 'dummy' columns by subsetting columns I want to keep (2:4,6:9).
        for (i in 1:length(data.sep)) {
            data.sep[[i]] <- data.sep[[i]][c(2:9)]
        }

        # Collapse all of the values of 'data.sep' into a vector.
        data.sep_L <- lapply(data.sep, 
                            function(r_L) {
                                sapply(r_L, function(x) x[1])
                            })
                        
        # Pack into a temporary data frame, temp_wow_data
        temp_wow_data <- ldply(data.sep_L, function(ls) ls)

        # Assign appropriate column names to data for data frame, dat.
        colnames(temp_wow_data) <- c("Query_Time", "Query_Seq_#", "Avatar_ID", "Guild", "Level", "Race", "Class", "Zone")
        temp_wow_data
    
        # Merge data frames
        wow_data <- rbind(wow_data, temp_wow_data)
        
    }   ## -- End else clause --

}   ## -- End outer for loop; data processing --
     
# Display process time.
proc.time() - ptm

# Close progress bar.
close(pb)

# Display final processed data frame, wow_data.
head(wow_data) ; tail(wow_data)

# Split "Query_Time" variable into "Query_Date" and "Query_Time" variables.
require(tidyr)
wow_data %>% separate(Query_Time, c("Query_Date", "Query_Time"), sep = " ", convert = TRUE) -> wow_data
glimpse(wow_data)

# Check the "Avatar_ID" variable for any anomalies. Treated as numeric sorts list.
table(as.numeric(wow_data$Avatar_ID))

# Check the "Guild" variable for any anomalies. Treated as numeric sorts list.
table(as.numeric(wow_data$Guild))

# Check the "Level" variable for any anomalies. Sort these by level (treat as numeric since it corresponds to a number).
table(as.numeric(wow_data$Level))

# Check the "Race" variable for any anomalies.
table(wow_data$Race)

# Remove any occurrences of classes that are not {Orc, Tauren, Troll, Undead}.
# Create a list of "Race" items that I want to keep {Orc, Tauren, Troll, Undead}.
race_list <- unique(wow_data$Race)[1:4] ; race_list
race_data <- wow_data[wow_data$Race %in% race_list, ] ; race_data ; nrow(race_data) ; nrow(wow_data)
# Overwrite 'wow_data' values with 'new_race' values.
wow_data <- race_data
nrow(wow_data) ; table(wow_data$Race)

# Check the "Class" variable for any anomalies.
table(wow_data$Class)
# Isolate the "Class" values that are only in {Druid, Hunter, Mage, Priest, Rogue, Shaman, Warlock, Warrior}.
#class_list <- c("Rogue", "Hunter", "Warrior", "Shaman", "Druid", "Priest", "Mage", "Warlock") ; class_list
class_list <- unique(wow_data$Class)[1:8] ; class_list
class_data <- wow_data[wow_data$Class %in% class_list, ] ; class_data;  nrow(class_data) ; nrow(wow_data)
# Assign new subset to 'wow_data'.
wow_data <- class_data
table(wow_data$Class)

# Check the "Zone" variable for any anomalies.
table(wow_data$Zone)
# Remove observations where "Zone" is not legible.
# Create a list of values want to remove.
zone_list <- sort(unique(wow_data$Zone)) ; zone_list
zone_list_len <- length(zone_list) ; zone_list_len
# Do not include first two entries in zone_list.
zone_list <- sort(unique(wow_data$Zone))[3:zone_list_len] ; zone_list
# Do not include the last four entries in zone_list.
zone_list <- sort(unique(wow_data$Zone))[3:77] ; zone_list
zone_data <- wow_data[wow_data$Zone %in% zone_list, ] ; zone_data ; nrow(zone_data) ; nrow(wow_data)
# Assign new subset to 'wow_data'.
wow_data <- zone_data ; table(wow_data$Zone)

### ***** End data checking and cleaning. *****

#### *** Start data analysis. ***** ####
## Load data from the wow_data.RData file.
load("/Users/christopher/Documents/Fall 2 2016/MET CS 544/Final Project/CS544Final_valdez/wow_data.RData")
dplyr::glimpse(wow_data)
View(dplyr::glimpse(wow_data))

### *** Numerical Data ***

# --- Level -----
# Mean of all Level values.
mean(as.numeric(wow_data$Level))
# Display the five number summary for all "Level" values.
summary(as.numeric(wow_data$Level))
# Display corresponding boxplot of dataset "Level" data.
boxplot(as.numeric(wow_data$Level), xlab = "Avatar Levels", col = "dodgerblue3", horizontal = TRUE, xaxt = "n")
axis(side = 1, at = fivenum(as.numeric(wow_data$Level)), labels = TRUE)
# Determine if there are any outliers.
level_iqr <- IQR(as.numeric(wow_data$Level)) ; level_iqr
summary <- summary(as.numeric(wow_data$Level)) ; summary
summary[2] - 1.5 * level_iqr   # Check for lower bound outlier
summary[5] + 1.5 * level_iqr   # Check for upper bound outlier
# Display the common measures of spread.
x <- as.numeric(wow_data$Level)
range(x) ; diff(range(x))
var(x) ; sd(x)
# Display barplot of avatar levels.
barplot(table(as.numeric(wow_data$Level)), names.arg=1:length(as.numeric(unique(wow_data$Level))), xlab = "Level",
        ylab = "Number of Avatars", col = "royalblue")
# Display the frequencies of avatar levels.
sort(table(as.numeric(wow_data$Level)))
# Display PDF curve of avatar levels.
x <- seq(1:60)
pdf <- dnorm(x, mean = mean(as.numeric(wow_data$Level)), sd = sd(as.numeric(wow_data$Level)))
plot(x, pdf, type = "l", col = "red", lwd = 4, main = "Avatar Level", ylab = "PDF", xlab = "Level")
abline(v = mean(as.numeric(wow_data$Level)), lty = 2, lwd = 2, col = "blue")
# Display CDF curve of avatar levels.
y <- seq(1:60)
cdf <- pnorm(y, mean = mean(as.numeric(wow_data$Level)), sd = sd(as.numeric(wow_data$Level)))
plot(y, cdf, type = "l", col = "red", lwd = 4, main = "Avatar Level", ylab = "CDF", xlab = "Level")
abline(v = mean(as.numeric(wow_data$Level)), lty = 2, lwd = 2, col = "blue")

z <- as.numeric(wow_data$Level)
plot(table(z), type = "h")

# --- Avatar_ID -----
# Create a list of Avatar_IDs and Level increase per day (including zeroes). ####
set.seed(100)
table(as.numeric(wow_data$Avatar_ID))
avg_per_day <- NULL ; avatar_id <- NULL
for (i in rownames(table(as.numeric(wow_data$Avatar_ID)))) {
  min_avid <- rownames(table(as.numeric(wow_data$Level[wow_data$Avatar_ID == i])))[1]
  max_avid <- rownames(table(as.numeric(wow_data$Level[wow_data$Avatar_ID == i])))[length(table(wow_data$Level[wow_data$Avatar_ID == i]))]
  avg <- (as.numeric(max_avid) - as.numeric(min_avid)) / length(unique(table(wow_data$Query_Date[wow_data$Avatar_ID == i])))

  avg_per_day[i] <- c(avg)
}
save(avg_per_day, file = "/Users/christopher/Documents/Fall 2 2016/MET CS 544/Final Project/CS544Final_valdez/avg_per_day.RData")

load("/Users/christopher/Documents/Fall 2 2016/MET CS 544/Final Project/CS544Final_valdez/avg_per_day.RData")
# Create a list of Avatar_IDs and Level increase per day (excluding zeroes).
avg_per_day_nz <- avg_per_day[avg_per_day > 0] ; length(avg_per_day_nz)

# Display summary of level increases including zeroes.
summary(avg_per_day, digits = 3)
# Display summary of level increases exluding zeroes.
summary(avg_per_day_nz, digits = 3)

# Display boxplot of level increases including zeroes.
boxplot(avg_per_day, horizontal = TRUE, col = "royalblue", main = "Average Level Increases Per Day", xlab = "Levels")
axis(side = 1, labels = TRUE)
text(summary(avg_per_day, digits = 3), rep(1.23,5), srt = 90, adj = 0, labels = c(summary(avg_per_day, digits = 3)))
# Display boxplot of level increases without zeroes.
boxplot(avg_per_day_nz, horizontal = TRUE, col = "purple", main = "Average Level Increases Per Day Without Zeroes", xlab = "Levels")
axis(side = 1, labels = TRUE)
text(summary(avg_per_day_nz, digits = 3), rep(1.23,5), srt = 90, adj = 0, labels = c(summary(avg_per_day_nz, digits = 3)))
# Display histogram of avg. level per day.
alpd_hist <- hist(avg_per_day, col = "royalblue", xlab = "Levels", main = "Average Level Increases Per Day", ylim = c(0, 7000))
# Display histogram of avg. level per day without zeroes.
alpd_hist_nz <- hist(avg_per_day_nz, col = "purple", xlab = "Levels", main = "Average Level Increases Per Day Without Zeroes", ylim = c(0, 7000))
# Display histogram of avg. level per day with probabilities.
hist(avg_per_day, col = "royalblue", xlab = "Levels", main = "Average Level Increases Per Day", prob = TRUE, ylim = c(0, 0.7))
mean(avg_per_day) ; sd(avg_per_day)
# Display histogram of avg. level per day with probabilities - without zeroes.
hist(avg_per_day_nz, col = "purple", xlab = "Levels", main = "Average Level Increases Per Day Without Zeroes", prob = TRUE, ylim = c(0, 0.7))
mean(avg_per_day_nz) ; sd(avg_per_day_nz)

# Display PDF curve of ALPD with zeroes.
par(mfrow = c(1, 2))
x <- seq(0:13)
pdf <- dnorm(x, mean = mean(avg_per_day), sd = sd(avg_per_day))
plot(x, pdf, type = "l", col = "royalblue", lwd = 3, main = "Average Avatar Level Increases", ylab = "PDF", xlab = "Average Level")
abline(v = mean(avg_per_day), lty = 2, lwd = 2, col = "limegreen")
# Display CDF curve of ALPD with zeroes.
y <- seq(0:13)
cdf <- pnorm(y_nz, mean = mean(avg_per_day), sd = sd(avg_per_day))
plot(y, cdf, type = "l", col = "royalblue", lwd = 3, main = "Average Avatar Level Increases", ylab = "CDF", xlab = "Average Level")
abline(v = mean(avg_per_day), lty = 2, lwd = 2, col = "limegreen")
# Display PDF curve of ALPD without zeroes.
par(mfrow = c(1, 2))
x_nz <- seq(0:13)
pdf_nz <- dnorm(x_nz, mean = mean(avg_per_day_nz), sd = sd(avg_per_day_nz))
plot(x_nz, pdf_nz, type = "l", col = "purple", lwd = 3, main = "Average Avatar Level Increases Without Zeroes", ylab = "PDF", xlab = "Average Level")
abline(v = mean(avg_per_day_nz), lty = 2, lwd = 2, col = "limegreen")
# Display CDF curve of ALPD without zeroes.
y_nz <- seq(0:13)
cdf_nz <- pnorm(y_nz, mean = mean(avg_per_day_nz), sd = sd(avg_per_day_nz))
plot(y_nz, cdf_nz, type = "l", col = "purple", lwd = 3, main = "Average Avatar Level Increases Without Zeroes", ylab = "CDF", xlab = "Average Level")
abline(v = mean(avg_per_day_nz), lty = 2, lwd = 2, col = "limegreen")

# ----- Sampling Average Level Increases Per Day Using avg_per_day -----
set.seed(30)
# Draw 500 samples of sample size 10
samples <- 500
sample.size <- 10
# Create a vector of 500 samples of size 10 each.
xbar.1 <- numeric(samples)
for(i in 1:samples) {
  xbar.1[i] <- mean(sample(avg_per_day, size = sample.size, replace = TRUE))
}
hist(xbar.1, prob = TRUE, main = "Sample Size = 10", xlab = "Average Level Increases")
# Calculate mean and std. dev. of sample means.
mean.1 <- mean(xbar.1) ; sd.1 <- sd(xbar.1)

# Draw 500 samples of sample size 20
samples <- 500
sample.size <- 20
# Create a vector of 500 samples of size 20 each.
xbar.2 <- numeric(samples)
for(i in 1:samples) {
  xbar.2[i] <- mean(sample(avg_per_day, size = sample.size, replace = TRUE))
}
hist(xbar.2, prob = TRUE, main = "Sample Size = 20", xlab = "Average Level Increases")
# Calculate mean and std. dev. of sample means.
mean.2 <- mean(xbar.2) ; sd.2 <- sd(xbar.2)

# Draw 500 samples of sample size 30
samples <- 500
sample.size <- 30
# Create a vector of 500 samples of size 30 each.
xbar.3 <- numeric(samples)
for(i in 1:samples) {
  xbar.3[i] <- mean(sample(avg_per_day, size = sample.size, replace = TRUE))
}
hist(xbar.3, prob = TRUE, main = "Sample Size = 30", xlab = "Average Level Increases")
# Calculate mean and std. dev. of sample means.
mean.3 <- mean(xbar.3) ; sd.3 <- sd(xbar.3)

# Draw 500 samples of sample size 40
samples <- 500
sample.size <- 40
# Create a vector of 500 samples of size 40 each.
xbar.4 <- numeric(samples)
for(i in 1:samples) {
  xbar.4[i] <- mean(sample(avg_per_day, size = sample.size, replace = TRUE))
}
hist(xbar.4, prob = TRUE, main = "Sample Size = 40", xlab = "Average Level Increases")
# Calculate mean and std. dev. of sample means.
mean.4 <- mean(xbar.4) ; sd.4 <- sd(xbar.4)

# Compare means and std. dev. of samples.
data <- c("Distribution", "Mean", "Difference from Mean", "Standard Deviation",
          "Avg_Per_Day", mean(avg_per_day), (mean(avg_per_day) - mean(avg_per_day)), sd(avg_per_day),
          "Sample 1", mean.1, abs(mean(avg_per_day) - mean.1), sd.1,
          "Sample 2", mean.2, abs(mean(avg_per_day) - mean.2), sd.2,
          "Sample 3", mean.3, abs(mean(avg_per_day) - mean.3), sd.3,
          "Sample 4", mean.4, abs(mean(avg_per_day) - mean.4), sd.4)
results <- matrix(data, nrow = 6, ncol = 4, byrow = TRUE)
View(results)
# Display sample histograms.
par(mfrow = c(2, 2))
hist(xbar.1, prob = TRUE, main = "Sample Size = 10",
     col = "royalblue", xlab = "Average Level Increases", xlim = c(0,3), ylim = c(0, 2))
lines(density(xbar.1, adjust = 3), lty = "dotted", col = "limegreen", lwd=3)
hist(xbar.2, prob = TRUE, main = "Sample Size = 20",
     col = "royalblue", xlab = "Average Level Increases", xlim = c(0,3), ylim = c(0, 2))
lines(density(xbar.2, adjust = 3), lty = "dotted", col = "limegreen", lwd=3)
hist(xbar.3, prob = TRUE, main = "Sample Size = 30",
     col = "royalblue", xlab = "Average Level Increases", xlim = c(0,3), ylim = c(0, 2))
lines(density(xbar.3, adjust = 3),lty = "dotted", col = "limegreen", lwd=3)
hist(xbar.4, prob = TRUE, main = "Sample Size = 40",
     col = "royalblue", xlab = "Average Level Increases", xlim = c(0,3), ylim = c(0, 2))
lines(density(xbar.4, adjust = 3), lty = "dotted", col = "limegreen", lwd=3)

# ----- Sampling Average Level Increases Per Day Using avg_per_day_nz -----
set.seed(99)
# Draw 500 samples of sample size 10
samples_nz <- 500
sample.size_nz <- 10
# Create a vector of 500 samples of size 10 each.
xbar.1_nz <- numeric(samples_nz)
for(i in 1:samples_nz) {
  xbar.1_nz[i] <- mean(sample(avg_per_day_nz, size = sample.size_nz, replace = TRUE))
}
hist(xbar.1_nz, prob = TRUE, main = "Sample Size = 10", xlab = "Average Level Increases Without Zeroes")
# Calculate mean and std. dev. of sample means.
mean.1_nz <- mean(xbar.1_nz) ; sd.1_nz <- sd(xbar.1_nz)

# Draw 500 samples of sample size 20
samples_nz <- 500
sample.size_nz <- 20
# Create a vector of 500 samples of size 20 each.
xbar.2_nz <- numeric(samples_nz)
for(i in 1:samples_nz) {
  xbar.2_nz[i] <- mean(sample(avg_per_day_nz, size = sample.size_nz, replace = TRUE))
}
hist(xbar.2_nz, prob = TRUE, main = "Sample Size = 20", xlab = "Average Level Increases Without Zeroes")
# Calculate mean and std. dev. of sample means.
mean.2_nz <- mean(xbar.2_nz) ; sd.2_nz <- sd(xbar.2_nz)

# Draw 500 samples of sample size 30
samples_nz <- 500
sample.size_nz <- 30
# Create a vector of 500 samples of size 30 each.
xbar.3_nz <- numeric(samples_nz)
for(i in 1:samples_nz) {
  xbar.3_nz[i] <- mean(sample(avg_per_day_nz, size = sample.size_nz, replace = TRUE))
}
hist(xbar.3_nz, prob = TRUE, main = "Sample Size = 30", xlab = "Average Level Increases Without Zeroes")
# Calculate mean and std. dev. of sample means.
mean.3_nz <- mean(xbar.3_nz) ; sd.3_nz <- sd(xbar.3_nz)

# Draw 500 samples of sample size 40
samples_nz <- 500
sample.size_nz <- 40
# Create a vector of 500 samples of size 40 each.
xbar.4_nz <- numeric(samples_nz)
for(i in 1:samples_nz) {
  xbar.4_nz[i] <- mean(sample(avg_per_day_nz, size = sample.size_nz, replace = TRUE))
}
hist(xbar.4_nz, prob = TRUE, main = "Sample Size = 40", xlab = "Average Level Increases Without Zeroes")
# Calculate mean and std. dev. of sample means.
mean.4_nz <- mean(xbar.4_nz) ; sd.4_nz <- sd(xbar.4_nz)

# Compare means and std. dev. of samples.
data_nz <- c("Distribution", "Mean", "Difference from Mean", "Standard Deviation",
          "Avg_Per_Day_NZ", mean(avg_per_day_nz), (mean(avg_per_day_nz) - mean(avg_per_day_nz)), sd(avg_per_day_nz),
          "Sample 1", mean.1_nz, abs(mean(avg_per_day_nz) - mean.1_nz), sd.1_nz,
          "Sample 2", mean.2_nz, abs(mean(avg_per_day_nz) - mean.2_nz), sd.2_nz,
          "Sample 3", mean.3_nz, abs(mean(avg_per_day_nz) - mean.3_nz), sd.3_nz,
          "Sample 4", mean.4_nz, abs(mean(avg_per_day_nz) - mean.4_nz), sd.4_nz)
results_nz <- matrix(data_nz, nrow = 6, ncol = 4, byrow = TRUE)
View(results_nz)
# Display sample histograms.
par(mfrow = c(2, 2))
hist(xbar.1_nz, prob = TRUE, main = "Sample Size = 10",
     col = "purple", xlab = "Average Level Increases Without Zeroes", xlim = c(0,3), ylim = c(0, 2))
lines(density(xbar.1_nz, adjust = 3), lty = "dotted", col = "limegreen", lwd=3)
hist(xbar.2_nz, prob = TRUE, main = "Sample Size = 20",
     col = "purple", xlab = "Average Level Increases Without Zeroes", xlim = c(0,3), ylim = c(0, 2))
lines(density(xbar.2_nz, adjust = 3), lty = "dotted", col = "limegreen", lwd=3)
hist(xbar.3_nz, prob = TRUE, main = "Sample Size = 30",
     col = "purple", xlab = "Average Level Increases Without Zeroes", xlim = c(0,3), ylim = c(0, 2))
lines(density(xbar.3_nz, adjust = 3),lty = "dotted", col = "limegreen", lwd=3)
hist(xbar.4_nz, prob = TRUE, main = "Sample Size = 40",
     col = "purple", xlab = "Average Level Increases Without Zeroes", xlim = c(0,3), ylim = c(0, 2))
lines(density(xbar.4_nz, adjust = 3), lty = "dotted", col = "limegreen", lwd=3)

# -- Bootstrap Distribution (Curve of Avg_Per_Day != Normal) Including Zeroes - Means ####
set.seed(520)
mean_apd_r1 <- mean(avg_per_day)
sd_apd_r1 <- sd(avg_per_day)
sample.size_r1 <- 20

resamples_apd_1 <- replicate(500, sample(avg_per_day, size = sample.size_r1, replace = TRUE), simplify = FALSE)
means.star1 <- sapply(resamples_apd_1, mean, simplify = TRUE)

hist(means.star1, prob = TRUE)
hist(means.star1, prob = TRUE, main = "Histogram of means.star",
     col = "royalblue", xlab = "Average Level Increases", ylim = c(0, 1.2))
lines(density(means.star1, adjust = 3),lty = "dotted", col = "limegreen", lwd=3)

mean.star1 <- mean(means.star1) ; mean.star1
bias2 <- mean(means.star1) - mean(avg_per_day) ; bias2  # Bootstrap estimate of bias
sd.star2 <- sd(means.star1) ; sd.star2   # Standard error of resamples means

samplemean <- function(x, indices) {
  return(mean(x[indices]))
}
boot(data = avg_per_day, statistic = samplemean, R = 500)

# Bootstrap confidence intervals - confidence = 80% (means) including zeroes
quantile(means.star1, c(0.10, 0.90))
hist(means.star1, prob = TRUE, main = "Histogram of means.star",
     col = "royalblue", xlab = "Average Level Increases")
lines(density(means.star1, adjust = 3),lty = "dotted", col = "limegreen", lwd=3)
abline(v = quantile(means.star1, c(0.10, 0.90))[1], col = "red", lty = "dotted", lwd=3)
abline(v = quantile(means.star1, c(0.10, 0.90))[2], col = "red", lty = "dotted", lwd = 3)
# *With 80% confidence, the median of the resamples will be between 0.70 and 0.78.*
boot.data1 <- boot(data = avg_per_day, statistic = samplemean, R = 500)
boot.ci(boot.data1, conf = 0.80, type = "perc")

# Bootstrap confidence intervals - confidence = 90% (means)
quantile(medians.star1, (c(0.05, 0.95)))
hist(medians.star1, prob = TRUE, main = "Histogram of medians.star",
     col = "royalblue", xlab = "Average Level Increases")
lines(density(medians.star1, adjust = 3),lty = "dotted", col = "limegreen", lwd=3)
abline(v = quantile(medians.star1, c(0.05, 0.95))[1], col = "red", lty = "dotted", lwd=3)
abline(v = quantile(medians.star1, c(0.05, 0.95))[2], col = "red", lty = "dotted", lwd = 3)
# *With 90% confidence, the median of the resamples will be between 0.67 and 0.78.*
boot.data2 <- boot(data = avg_per_day, statistic = samplemedian, R = 20)
boot.ci(boot.data2, conf = 0.90, type = "perc")

# -- Bootstrap Distribution (Curve of Avg_Per_Day != Normal) Including Zeroes - Medians ####
set.seed(500)
mean_apd_r2 <- mean(avg_per_day)
median_apd_r2 <- median(avg_per_day)   # Right skew > median is better.
sample.size_r2 <- 20

resamples_apd_2 <- replicate(500, sample(avg_per_day, size = sample.size_r2, replace = TRUE), simplify = FALSE)
medians.star1 <- sapply(resamples_apd_2, median, simplify = TRUE)

hist(medians.star1, prob = TRUE)
hist(medians.star1, prob = TRUE, main = "Histogram of medians.star",
     col = "royalblue", xlab = "Average Level Increases")
lines(density(medians.star1, adjust = 3),lty = "dotted", col = "limegreen", lwd=3)

## Increase sample size
set.seed(500)
mean_apd_r2 <- mean(avg_per_day)
median_apd_r2 <- median(avg_per_day)   # Right skew > median is better.
sample.size_r2 <- 100

resamples_apd_2 <- replicate(500, sample(avg_per_day, size = sample.size_r2, replace = TRUE), simplify = FALSE)
medians.star1 <- sapply(resamples_apd_2, median, simplify = TRUE)

hist(medians.star1, prob = TRUE)
hist(medians.star1, prob = TRUE, main = "Histogram of medians.star",
     col = "royalblue", xlab = "Average Level Increases")
lines(density(medians.star1, adjust = 3),lty = "dotted", col = "limegreen", lwd=3)

median(avg_per_day)
mean.star1 <- mean(medians.star1)
bias1 <- mean(medians.star1) - median(avg_per_day) ; bias1  # Bootstrap estimate of bias
sd.star1 <- sd(medians.star1)   # Standard error of resamples medians

samplemedian <- function(x, indices) {
  return(median(x[indices]))
}
boot(data = avg_per_day, statistic = samplemedian, R = 500)

# Bootstrap confidence intervals - confidence = 80% (medians) including zeroes
quantile(medians.star1, c(0.10, 0.90))
hist(medians.star1, prob = TRUE, main = "Histogram of medians.star",
     col = "royalblue", xlab = "Average Level Increases")
lines(density(medians.star1, adjust = 3),lty = "dotted", col = "limegreen", lwd=3)
abline(v = quantile(medians.star1, c(0.10, 0.90))[1], col = "red", lty = "dotted", lwd=3)
abline(v = quantile(medians.star1, c(0.10, 0.90))[2], col = "red", lty = "dotted", lwd = 3)
# *With 80% confidence, the median of the resamples will be between 0.70 and 0.78.*
boot.data1 <- boot(data = avg_per_day, statistic = samplemedian, R = 500)
boot.ci(boot.data1, conf = 0.80, type = "perc")

# Bootstrap confidence intervals - confidence = 90% (medians)
quantile(medians.star1, (c(0.05, 0.95)))
hist(medians.star1, prob = TRUE, main = "Histogram of medians.star",
     col = "royalblue", xlab = "Average Level Increases")
lines(density(medians.star1, adjust = 3),lty = "dotted", col = "limegreen", lwd=3)
abline(v = quantile(medians.star1, c(0.05, 0.95))[1], col = "red", lty = "dotted", lwd=3)
abline(v = quantile(medians.star1, c(0.05, 0.95))[2], col = "red", lty = "dotted", lwd = 3)
# *With 90% confidence, the median of the resamples will be between 0.67 and 0.78.*
boot.data2 <- boot(data = avg_per_day, statistic = samplemedian, R = 20)
boot.ci(boot.data2, conf = 0.90, type = "perc")

## Sampling Methods of average levels per day (apd) ####
### Create a list of average leveling times; use for sampling.
s_apd <- NULL ; s_avatar_id <- NULL
for (i in rownames(table(as.numeric(wow_data$Avatar_ID)))) {
  s_min_avid <- rownames(table(as.numeric(wow_data$Level[wow_data$Avatar_ID == i])))[1]
  s_max_avid <- rownames(table(as.numeric(wow_data$Level[wow_data$Avatar_ID == i])))[length(table(wow_data$Level[wow_data$Avatar_ID == i]))]
  s_avg <- (as.numeric(s_max_avid) - as.numeric(s_min_avid)) / length(unique(table(wow_data$Query_Date[wow_data$Avatar_ID == i])))
  
  s_apd[i] <- c(s_avg)
}
save(s_apd, file = "/Users/christopher/Documents/Fall 2 2016/MET CS 544/Final Project/CS544Final_valdez/s_apd.RData")

load("/Users/christopher/Documents/Fall 2 2016/MET CS 544/Final Project/CS544Final_valdez/s_apd.RData")
## Simple Random Sampling Without Replacement for ALPD Including Zeroes
set.seed(1)
nrow(as.table(s_apd))
sample.size <- 20
s_avg <- srswor(sample.size, length(wow_data$Avatar_ID))
alpd_sample1 <- wow_data[s_avg != 0, ] ; head(alpd_sample1)
# Display sample Avatar_ID and corresponding average level per day increase.
s_apd[alpd_sample1$Avatar_ID[alpd_sample1$Avatar_ID %in% rownames(as.table(s_apd))]]
length(s_apd[alpd_sample1$Avatar_ID[alpd_sample1$Avatar_ID %in% rownames(as.table(s_apd))]]) # Length = 20
sampling1 <- s_apd[alpd_sample1$Avatar_ID[alpd_sample1$Avatar_ID %in% rownames(as.table(s_apd))]]
mean(sampling1) ; sd(sampling1)
mean(s_apd) ; sd(s_apd)
# Find confidence intervals for sample with conf. levels {80%, 90%}.
sd.sampling1.means <- sd(avg_per_day)/sqrt(sample.size)
xbar.sampling1.means <- mean(sampling1)
z.sampling1 <- (xbar.sampling1.means - mean(avg_per_day))/(sd(avg_per_day)/sqrt(sample.size))
cat("Critical Value: z = ", z.sampling1, ", Conf. Level 80%: Lower value = ",
            qnorm(0.2/2), ", Upper value = ", qnorm(1-0.2/2))  # Contains the mean.
cat("Critical Value: z = ", z.sampling1, ", Conf. Level 90%: Lower value = ",
    qnorm(0.1/2), ", Upper value = ", qnorm(1-0.1/2))  # Contains the mean.
# Display sample mean conf. level values.
cat(
c("\n", "Sample Mean = ", xbar.sampling1.means, ", Conf. Level 80%: Lower value = ",
         xbar.sampling1.means - qnorm(1 - 0.2/2) * sd.sampling1.means, "Upper value = ",
         xbar.sampling1.means + qnorm(1 - 0.2/2) * sd.sampling1.means), "\n",
c("Sample Mean = ", xbar.sampling1.means, ", Conf. Level 90%: Lower value = ",
         xbar.sampling1.means - qnorm(1 - 0.1/2) * sd.sampling1.means, "Upper value = ",
         xbar.sampling1.means + qnorm(1 - 0.1/2) * sd.sampling1.means))

## Systematic Sampling for ALPD
set.seed(200)
N_apd <- nrow(as.table(s_apd)) ; N_apd
n_apd <- 20
k_apd <- ceiling(N_apd / n_apd) ; k_apd
r_apd <- sample(k_apd, 1) ; r_apd
s_apd.2 <- seq(r_apd, by = k_apd, length = n_apd)
s_apd.2_sample <- wow_data[s_apd.2, ] ; s_apd.2_sample
s_apd[s_apd.2_sample$Avatar_ID[s_apd.2_sample$Avatar_ID %in% rownames(as.table(s_apd))]]
length(s_apd[s_apd.2_sample$Avatar_ID[s_apd.2_sample$Avatar_ID %in% rownames(as.table(s_apd))]]) # Length = 20
sampling2 <- s_apd[s_apd.2_sample$Avatar_ID[s_apd.2_sample$Avatar_ID %in% rownames(as.table(s_apd))]]
mean(sampling2) ; sd(sampling2)
mean(s_apd) ; sd(s_apd)
# Find confidence interval for sample with conf. levels {80%, 90%}.
sample.size <- 20
sd.sampling2.means <- sd(avg_per_day)/sqrt(sample.size)
xbar.sampling2.means <- mean(sampling2)
z.sampling2 <- (xbar.sampling2.means - mean(avg_per_day))/(sd(avg_per_day)/sqrt(sample.size))
cat("Critical Value: z = ", z.sampling2, ", Conf. Level 80%: Lower value = ",
    qnorm(0.2/2), ", Upper value = ", qnorm(1-0.2/2))  # Contains the mean.
cat("Critical Value: z = ", z.sampling2, ", Conf. Level 90%: Lower value = ",
    qnorm(0.1/2), ", Upper value = ", qnorm(1-0.1/2))  # Contains the mean.
# Display sample mean conf. level values.
cat(
  c("\n", "Sample Mean = ", xbar.sampling2.means, ", Conf. Level 80%: Lower value = ",
    xbar.sampling2.means - qnorm(1 - 0.2/2) * sd.sampling2.means, "Upper value = ",
    xbar.sampling2.means + qnorm(1 - 0.2/2) * sd.sampling2.means), "\n",
  c("Sample Mean = ", xbar.sampling2.means, ", Conf. Level 90%: Lower value = ",
    xbar.sampling2.means - qnorm(1 - 0.1/2) * sd.sampling2.means, "Upper value = ",
    xbar.sampling2.means + qnorm(1 - 0.1/2) * sd.sampling2.means))

## Systematic Sampling for ALPD with Inclusion Probabilities
set.seed(150)
incl.p <- inclusionprobabilities(s_apd, 20)   # Calculate incl. prob. based on avatar level. and samp. size 20.
s_apd.3 <- UPsystematic(incl.p) ; s_apd.3
table(s_apd.3)
s_apd.3_sample <- wow_data[which(s_apd.3 != 0), ]
s_apd[s_apd.3_sample$Avatar_ID[s_apd.3_sample$Avatar_ID %in% rownames(as.table(s_apd))]]
length(s_apd[s_apd.3_sample$Avatar_ID[s_apd.3_sample$Avatar_ID %in% rownames(as.table(s_apd))]]) # Length = 20
sampling3 <- s_apd[s_apd.3_sample$Avatar_ID[s_apd.3_sample$Avatar_ID %in% rownames(as.table(s_apd))]]
mean(sampling3) ; sd(sampling3)
# Find confidence interval for sample with conf. levels {80%, 90%}.
sample.size <- 20
sd.sampling3.means <- sd(avg_per_day)/sqrt(sample.size)
xbar.sampling3.means <- mean(sampling3)
z.sampling3 <- (xbar.sampling3.means - mean(avg_per_day))/(sd(avg_per_day)/sqrt(sample.size))
cat("Critical Value: z = ", z.sampling3, ", Conf. Level 80%: Lower value = ",
    qnorm(0.2/2), ", Upper value = ", qnorm(1-0.2/2))  # Contains the mean.
cat("Critical Value: z = ", z.sampling3, ", Conf. Level 90%: Lower value = ",
    qnorm(0.1/2), ", Upper value = ", qnorm(1-0.1/2))  # Contains the mean.
# Display sample mean conf. level values.
cat(
  c("\n", "Sample Mean = ", xbar.sampling3.means, ", Conf. Level 80%: Lower value = ",
    xbar.sampling3.means - qnorm(1 - 0.2/2) * sd.sampling3.means, "Upper value = ",
    xbar.sampling3.means + qnorm(1 - 0.2/2) * sd.sampling3.means), "\n",
  c("Sample Mean = ", xbar.sampling3.means, ", Conf. Level 90%: Lower value = ",
    xbar.sampling3.means - qnorm(1 - 0.1/2) * sd.sampling3.means, "Upper value = ",
    xbar.sampling3.means + qnorm(1 - 0.1/2) * sd.sampling3.means))

## Display differences in sampling methods.
sample_data <- c("Dist.", "Mean", "Diff. from Pop. Mean", "Std. Dev.",
                 "s_apd", mean(s_apd), mean(s_apd) - mean(s_apd), sd(s_apd),
                 "Sampling1", mean(sampling1), mean(s_apd) - mean(sampling1), sd(sampling1),
                 "Sampling2", mean(sampling2), mean(s_apd) - mean(sampling2), sd(sampling2),
                 "Sampling3", mean(sampling3), mean(s_apd) - mean(sampling3), sd(sampling3))
sample_results <- matrix(sample_data, nrow = 5, ncol = 4, byrow = TRUE) ; sample_results
View(sample_results)

# Display sample mean, pop. mean, and conf. level values for all samples.
cat(
  c("\nSimple Random Sampling:\n", "Population Mean = ", round(mean(s_apd), digits = 3),
    "\tSample Mean = ", round(xbar.sampling1.means, digits = 3), 
    "\tConf. Level 80%: Lower value = ", round(xbar.sampling1.means - qnorm(1 - 0.2/2) * sd.sampling1.means, digits = 3),
    "\tUpper value = ", round(xbar.sampling1.means + qnorm(1 - 0.2/2) * sd.sampling1.means, digits = 3)),
  c("\n Population Mean = ", round(mean(s_apd), digits = 3),
    "\tSample Mean = ", round(xbar.sampling1.means, digits = 3),
    "\tConf. Level 90%: Lower value = ", round(xbar.sampling1.means - qnorm(1 - 0.1/2) * sd.sampling1.means, digits = 3),
    "\tUpper value = ", round(xbar.sampling1.means + qnorm(1 - 0.1/2) * sd.sampling1.means, digits = 3)),
  c("\n\nSystematic Sampling:\n", "Population Mean = ", round(mean(s_apd), digits = 3),
    "\tSample Mean = ", round(xbar.sampling2.means, digits = 3),
    "\tConf. Level 80%: Lower value = ", round(xbar.sampling2.means - qnorm(1 - 0.2/2) * sd.sampling2.means, digits = 3),
    "\tUpper value = ", round(xbar.sampling2.means + qnorm(1 - 0.2/2) * sd.sampling2.means, digits = 3)),
  c("\n Population Mean = ", round(mean(s_apd), digits = 3),
    "\tSample Mean = ", round(xbar.sampling2.means, digits = 3),
    "\tConf. Level 90%: Lower value = ", round(xbar.sampling2.means - qnorm(1 - 0.1/2) * sd.sampling2.means, digits = 3),
    "\tUpper value = ", round(xbar.sampling2.means + qnorm(1 - 0.1/2) * sd.sampling2.means, digits = 3)),
  c("\n\nSystematic Sampling (Incl. Prob):\n", "Population Mean = ", round(mean(s_apd), digits = 3),
    "\tSample Mean = ", round(xbar.sampling3.means, digits = 3),
    "\tConf. Level 80%: Lower value = ", round(xbar.sampling3.means - qnorm(1 - 0.2/2) * sd.sampling3.means, digits = 3),
    "\tUpper value = ", round(xbar.sampling3.means + qnorm(1 - 0.2/2) * sd.sampling3.means, digits = 3)),
  c("\n Population Mean = ", round(mean(s_apd), digits = 3),
    "\tSample Mean = ", round(xbar.sampling1.means, digits = 3),
    "\tConf. Level 90%: Lower value = ", round(xbar.sampling3.means - qnorm(1 - 0.1/2) * sd.sampling3.means, digits = 3),
    "\tUpper value = ", round(xbar.sampling3.means + qnorm(1 - 0.1/2) * sd.sampling3.means, digits = 3), "\n\n"))

## Sampling Methods of average levels per day (apd_nz) without zeroes ####
### Create a list of average leveling times without zeroes using s_apd; use for sampling.
s_apd_nz <- s_apd[s_apd >0]

## Simple Random Sampling Without Replacement for ALPD without zeroes.
nrow(as.table(s_apd_nz))
sample.size_nz <- 20
s_avg_nz <- srswor(sample.size_nz, length(wow_data$Avatar_ID))
alpd_sample1_nz <- wow_data[s_avg_nz != 0, ] ; head(alpd_sample1_nz)
# Display sample Avatar_ID and corresponding average level per day increase without zeroes.
s_apd[alpd_sample1_nz$Avatar_ID[alpd_sample1_nz$Avatar_ID %in% rownames(as.table(s_apd_nz))]]
length(s_apd[alpd_sample1_nz$Avatar_ID[alpd_sample1_nz$Avatar_ID %in% rownames(as.table(s_apd_nz))]]) # Length = 17
sampling4 <- s_apd[alpd_sample1_nz$Avatar_ID[alpd_sample1_nz$Avatar_ID %in% rownames(as.table(s_apd_nz))]]

## Systematic Sampling for ALPD Without Zeroes
N_apd_nz <- nrow(as.table(s_apd_nz)) ; N_apd_nz
n_apd_nz <- 20
k_apd_nz <- ceiling(N_apd_nz / n_apd_nz) ; k_apd_nz
r_apd_nz <- sample(k_apd_nz, 1) ; r_apd_nz
s_apd.2_nz <- seq(r_apd_nz, by = k_apd_nz, length = n_apd_nz) ; length(s_apd.2_nz)
s_apd.2_sample_nz <- wow_data[s_apd.2_nz, ] ; s_apd.2_sample_nz ; nrow(s_apd.2_sample_nz)
# Display sample Avatar_ID and corresponding average level per day increase.
s_apd_nz[s_apd.2_sample_nz$Avatar_ID[s_apd.2_sample_nz$Avatar_ID %in% rownames(as.table(s_apd_nz))]]
length(s_apd_nz[s_apd.2_sample_nz$Avatar_ID[s_apd.2_sample_nz$Avatar_ID %in% rownames(as.table(s_apd_nz))]]) # Length = 14
sampling5 <- s_apd_nz[s_apd.2_sample_nz$Avatar_ID[s_apd.2_sample_nz$Avatar_ID %in% rownames(as.table(s_apd_nz))]]

## Systematic Sampling for ALPD with Inclusion Probabilities
incl.p_nz <- inclusionprobabilities(s_apd_nz, 20)   # Calculate incl. prob. based on avatar level. and samp. size 20.
length(incl.p_nz) ; length(s_apd_nz)
s_apd.3_nz <- UPsystematic(incl.p_nz) ; length(s_apd.3_nz)
table(s_apd.3_nz) ; sum(s_apd.3_nz)
s_apd.3_sample_nz <- wow_data[which(s_apd.3_nz != 0), ] ; nrow(s_alt)
head(s_apd.3_sample_nz) ; nrow(s_apd.3_sample_nz)
# Display sample Avatar_ID and corresponding average level per day increase.
s_apd_nz[s_apd.3_sample_nz$Avatar_ID[s_apd.3_sample_nz$Avatar_ID %in% rownames(as.table(s_apd_nz))]]
length(s_apd_nz[s_alt$Avatar_ID[s_alt$Avatar_ID %in% rownames(as.table(s_apd_nz))]]) # Length = 19
sampling6 <- s_apd_nz[s_apd.3_sample_nz$Avatar_ID[s_apd.3_sample_nz$Avatar_ID %in% rownames(as.table(s_apd_nz))]]

## Display differences in sampling methods without zeroes.
sample_data2 <- c("Dist.", "Mean", "Diff. from Pop. Mean", "Std. Dev.",
                 "s_apd", mean(s_apd), mean(s_apd) - mean(s_apd), sd(s_apd),
                 "Sampling4", mean(sampling4), mean(s_apd) - mean(sampling4), sd(sampling4),
                 "Sampling5", mean(sampling5), mean(s_apd) - mean(sampling5), sd(sampling5),
                 "Sampling6", mean(sampling6), mean(s_apd) - mean(sampling6), sd(sampling6))
sample_results2 <- matrix(sample_data2, nrow = 5, ncol = 4, byrow = TRUE) ; sample_results
View(sample_results2)

# ----- Sampling Avatar Levels -----
set.seed(60)
par(mfrow = c(2, 2))
# Draw 500 samples of sample size 10
samples <- 500
sample.size <- 10
# Create a vector of 500 samples of size 10 each.
xbar.1 <- numeric(samples)
for(i in 1:samples) {
  xbar.1[i] <- mean(sample(as.numeric(wow_data$Level), size = sample.size, replace = TRUE))
}

# Calculate mean and std. dev. of sample means.
mean.1 <- mean(xbar.1) ; sd.1 <- sd(xbar.1)

# Draw 500 samples of sample size 20
samples <- 500
sample.size <- 20
# Create a vector of 500 samples of size 20 each.
xbar.2 <- numeric(samples)
for(i in 1:samples) {
  xbar.2[i] <- mean(sample(as.numeric(wow_data$Level), size = sample.size, replace = TRUE))
}

# Calculate mean and std. dev. of sample means.
mean.2 <- mean(xbar.2) ; sd.2 <- sd(xbar.2)

# Draw 500 samples of sample size 30
samples <- 500
sample.size <- 30
# Create a vector of 500 samples of size 30 each.
xbar.3 <- numeric(samples)
for(i in 1:samples) {
  xbar.3[i] <- mean(sample(as.numeric(wow_data$Level), size = sample.size, replace = TRUE))
}

# Calculate mean and std. dev. of sample means.
mean.3 <- mean(xbar.3) ; sd.3 <- sd(xbar.3)

# Draw 50 samples of sample size 40
samples <- 500
sample.size <- 40
# Create a vector of 500 samples of size 40 each.
xbar.4 <- numeric(samples)
for(i in 1:samples) {
  xbar.4[i] <- mean(sample(as.numeric(wow_data$Level), size = sample.size, replace = TRUE))
}

# Calculate mean and std. dev. of sample means.
mean.4 <- mean(xbar.4) ; sd.4 <- sd(xbar.4)

# Compare means and std. dev. of samples.
data <- c("Distribution", "Mean", "Difference from Mean", "Standard Deviation",
          "Avg_Per_Day", mean(as.numeric(wow_data$Level)), (mean(as.numeric(wow_data$Level)) - mean(as.numeric(wow_data$Level))), sd(as.numeric(wow_data$Level)),
          "Sample 1", mean.1, abs(mean(as.numeric(wow_data$Level)) - mean.1), sd.1,
          "Sample 2", mean.2, abs(mean(as.numeric(wow_data$Level)) - mean.2), sd.2,
          "Sample 3", mean.3, abs(mean(as.numeric(wow_data$Level)) - mean.3), sd.3,
          "Sample 4", mean.4, abs(mean(as.numeric(wow_data$Level)) - mean.4), sd.4)
results <- matrix(data, nrow = 6, ncol = 4, byrow = TRUE) ; results
# Display sample histograms.
par(mfrow = c(2, 2))
hist(xbar.1, prob = TRUE, main = "Sample Size = 10",
     col = "lightslateblue", xlab = "Average Level Increases", xlim = c(0,60))
lines(density(xbar.1, adjust = 3), lty = "dotted", col = "limegreen", lwd=3)
hist(xbar.2, prob = TRUE, main = "Sample Size = 20",
     col = "lightslateblue", xlab = "Average Level Increases", xlim = c(0,60))
lines(density(xbar.2, adjust = 3), lty = "dotted", col = "limegreen", lwd=3)
hist(xbar.3, prob = TRUE, main = "Sample Size = 30",
     col = "lightslateblue", xlab = "Average Level Increases", xlim = c(0,60))
lines(density(xbar.3, adjust = 3),lty = "dotted", col = "limegreen", lwd=3)
hist(xbar.4, prob = TRUE, main = "Sample Size = 40",
     col = "lightslateblue", xlab = "Average Level Increases", xlim = c(0,60))
lines(density(xbar.4, adjust = 3), lty = "dotted", col = "limegreen", lwd=3)

### ----- End Sampling ------

# --- Query_Date -----
# Display the number of queries by query date.
sort(table(wow_data$Query_Date))
# Display the average number of queries daily.
mean(table(wow_data$Query_Date))

# --- Query_Time -----
#Display the highest number of queries by query times.
tail(sort(table(wow_data$Query_Time)))
length(table(wow_data$Query_Time))

#### *** Categorical Data *** ####

# --- Race & Class -----
# Display table of each race.
sort(table(wow_data$Race))
# Display table of each class.
sort(table(wow_data$Class))
# Calculate proportions of each "Race" over the dataset in increasing order.
sort(table(wow_data$Race)/length(wow_data$Race)*100)
# Calculate proportions of each "Class" over the dataset in increasing order.
sort(table(wow_data$Class)/length(wow_data$Class)*100)
# View most chosen combination of race and class.
race_class <- dplyr::mutate(wow_data, race_class = paste(Race, Class), sep = " ")
sort(table(race_class$race_class))
race_class_list <- sort(unique(race_class$race_class))   # Create a list of race-class comb.
race_class_bar <- barplot(table(race_class$race_class), xaxt = "n",
                          ylab = "Frequency", col = "royalblue", main = "Race-Class Frequencies", ylim = c(0,400000))
axis(1, at = race_class_bar, labels = race_class_list, tick = FALSE,
     las = 2, line = -0.25, cex.axis = 0.5)
text(x = race_class_bar, y = table(race_class$race_class), label = table(race_class$race_class), pos = 3, cex = 0.8, col = "darkslateblue")
mtext(side = 1, text = "Race Class Combination", line = 4)

# Display pie chart of Race.
data <- table(wow_data$Race)
slice.labels <- names(data)
slice.percents <- round(data/sum(data)*100)
slice.labels <- paste(slice.labels, slice.percents)
slice.labels <- paste(slice.labels, "%", sep="")
pie(data, labels = slice.labels, col=rainbow(length(unique(wow_data$Race))),
    main = "Proportions by Race")
# Display pie chart of Class.
data <- table(wow_data$Class)
slice.labels <- names(data)
slice.percents <- round(data/sum(data)*100)
slice.labels <- paste(slice.labels, slice.percents)
slice.labels <- paste(slice.labels, "%", sep="")
pie(data, labels = slice.labels, col=rainbow(length(unique(wow_data$Class))), 
    main = "Proportions by Class")

# Display Two-Way Table of Class and Race
table(wow_data$Class, wow_data$Race)
# Display mosaic plot for class and race.
mosaicplot(table(wow_data$Class, wow_data$Race), color = c("royalblue", "firebrick3"), main = "Mosaic Plot for Race & Class", off = 30)
help(mosaicplot)
# Display proportions two-way table of class and race.
options(digits = 3)
prop.table(table(wow_data$Class, wow_data$Race))

# --- Sampling Methods for Race-Class --- ####
## Simple Random Sampling Without Replacement for Race-Class
nrow(race_class)
sample.size <- 20
s.1 <- srswor(sample.size, nrow(race_class))
race_class_sample <- wow_data[s.1 != 0, ] ; race_class_sample
table(race_class_sample$Race) ; table(race_class_sample$Class)     # See frequency tables of Race and Class
options(digits = 3)
(table(race_class_sample$Race) / table(wow_data$Race)) * 100     # See proportions of sample compared to population for Race as percent.
(table(race_class_sample$Class) / table(wow_data$Class[wow_data$Class %in% race_class_sample$Class])) * 100     # See proportions of sample compared to pop. for Class as percent.

## Systematic Sampling for Race-Class
N_raceclass2 <- nrow(race_class)
n_raceclass2 <- 20
k_raceclass2 <- ceiling(N_raceclass2 / n_raceclass2)
r_raceclass2 <- sample(k_raceclass2, 1)
s_raceclass2 <- seq(r_raceclass2, by = k_raceclass2, length = n_raceclass2)
race_class_sample2 <- wow_data[s_raceclass2, ] ; race_class_sample2
table(race_class_sample2$Race) ; table(race_class_sample2$Class)   # View frequency tables of Race and Class
options(digits = 3)
(table(race_class_sample2$Race) / table(wow_data$Race)) * 100   # See prop. of sample compared to Race pop. as percent.
(table(race_class_sample2$Class) / table(wow_data$Class[wow_data$Class %in% race_class_sample2$Class])) * 100   # See prop. of sample compared to Class pop. as percent.

## Systematic Sampling for Race-Class with Inclusion Probabilities
incl.p <- inclusionprobabilities(as.numeric(wow_data$Level), 20)   # Calculate incl. prob. based on avatar level. and samp. size 20.
s_race_class3 <- UPsystematic(incl.p) ; s_race_class3
table(s_race_class3)
race_class_sample3 <- wow_data[s_race_class3 != 0, ] ; nrow(wow_data) ; nrow(race_class_sample3)
options(digits = 3)
(table(race_class_sample3$Race) / table(wow_data$Race)) * 100   # See prop. of sample compared to Race pop. as percent.
(table(race_class_sample3$Class) / table(wow_data$Class[wow_data$Class %in% race_class_sample3$Class])) * 100   # See prop. of sample compared to Class pop. as percent.

## Stratified Sampling for Race
order.index <- order(wow_data$Race) ; race_class4 <- wow_data[order.index, ]
freq <- table(race_class4$Race) ; freq
st.sizes <- 20 * freq / sum(freq) ; st.sizes <- round(st.sizes)
st <- strata(race_class4, stratanames = c("Race"), size = st.sizes, method = "srswor", description = TRUE) ; st
race_class_sample4 <- getdata(race_class4, st)
race_class_sample4

## Stratified Sampling for Class
order.index <- order(wow_data$Class) ; race_class5 <- wow_data[order.index, ]
freq <- table(race_class5$Class) ; freq
st.sizes <- 20 * freq / sum(freq) ; st.sizes <- round(st.sizes)
st <- strata(race_class5, stratanames = c("Class"), size = st.sizes, method = "srswor", description = TRUE) ; st
race_class_sample5 <- getdata(race_class5, st)
race_class_sample5

# --- Zone -----
# View zone most frequently sampled.
sort(table(wow_data$Zone)) ; length(unique(wow_data$Zone))
zone_list <- sort(unique(wow_data$Zone))
zone_bar <- barplot(table(wow_data$Zone), xaxt = "n", ylab = "Frequency",
                    col = "royalblue", main = "Zones")
axis(1, at = zone_bar, labels = zone_list, tick = FALSE, las = 2, line = -1, cex.axis = 0.5)
mtext(side = 1, text = "Zone", line = 4)
# View zone most frequently sampled by level.
# Create rownames.
m <- NULL
for (i in 1:length(table(wow_data$Zone))) {
  m[i] <- rownames(table(wow_data$Zone))[i]
}
# Create matrix of frequencies for each zone by level.
zone_level <- NULL
zone_level <- matrix(c(NA), nrow = length(m), ncol = length(1:60))
rownames(zone_level) <- c(m) ; colnames(zone_level) <- c(rownames(table(as.numeric(wow_data$Level))))
zone_level
# Fill matrix with frequency values.
for (j in 1:60) {
  for (i in rownames(zone_level)) {
    if (i %in% rownames(table(wow_data$Zone[wow_data$Level == j]))) {
      zone_level[i,j] <- table(wow_data$Zone[wow_data$Level == j])[[which(rownames(table(wow_data$Zone[wow_data$Level == j])) == i)]]
    }
    else {
      zone_level[i,j] <- 0
    }
  }
}
zone_level
# Draw heatmap.
zone_level_map <- heatmap(zone_level, Rowv = NA, Colv = NA, col = brewer.pal(9, "Blues"), scale = "column", margins = c(5,10),
                        main = "Heatmap of Zones by Level", xlab = "Levels", ylab = "Zones")

### ***** End data analysis. *****