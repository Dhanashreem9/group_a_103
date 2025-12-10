install.packages("tidyverse")

#---------------------------------------
#load the necessary libraries
#---------------------------------------
library(tidyverse)
library(readr)

#---------------------------------------
#view and load the dataset
#---------------------------------------
view(all_grains_data)
df <- all_grains_data
str(df)

#---------------------------------------
#convert the date column to date 
#---------------------------------------
df$date <- as.Date(df$date, format = "%d-%m-%Y")

#---------------------------------------
#extract year from date
#---------------------------------------
df$year <- format(df$date, "%Y")

df$year <- as.numeric(format(df$date, "%Y"))

df

#---------------------------------------
#display first 5 rows
#---------------------------------------
head(df, 5)

#---------------------------------------
#display last 5 rows
#---------------------------------------
tail(df,5)

#to rename the coloumn in the dataset
df<- rename(df,crop_type = commodity,
           opening_price = open,
           max_price=high,
           low_price=low,
           closing_price=close )

df

summary(df)

# Identify the commodity with the highest mean closing price
mean_close <- aggregate(closing_price ~ crop_type, df, mean)
highest <- mean_close$crop_type[which.max(mean_close$closing_price)]

#---------------------------------------
# 1. Filter Rough Rice and Oat
#---------------------------------------

df2 <- df[df$crop_type %in% c("Rough Rice", "Oat"), ]

#------------------------------------------------
# 3a. Histogram for Rough Rice WITH normal curve
#------------------------------------------------

rice <- df2$closing_price[df2$crop_type == "Rough Rice"]

hist(rice,
     probability = TRUE,
     main = "Histogram of Rough Rice Closing Prices\nwith Normal Curve",
     xlab = "Closing Price",
     col = "lightgray",
     breaks = 20)

# Normal curve overlay
curve(dnorm(x, mean = mean(rice, na.rm = TRUE),
            sd = sd(rice, na.rm = TRUE)),
      add = TRUE,
      lwd = 2)

df

#------------------------------------------------
# 3b. Histogram for oat WITH normal curve 
#------------------------------------------------

oat <- df2$closing_price[df2$crop_type == "Oat"]

hist(oat,
     probability = TRUE,
     main = "Histogram of Oat Closing Prices\nwith Normal Curve",
     xlab = "Closing Price",
     col = "lightgray",
     breaks = 20)

# Normal curve overlay
curve(dnorm(x, mean = mean(oat, na.rm = TRUE),
            sd = sd(oat, na.rm = TRUE)),
      add = TRUE,
      lwd = 2)

# ---------------------------
# 1. Boxplot (closing prices)
# ---------------------------
boxplot(closing_price ~ crop_type,
        data = df,
        cex.axis = 0.6,
        main = "Boxplot of Closing price Distribution by the type of crop",
        xlab = "Type of crop",
        ylab = "Closing Price",
        col = c("lightyellow", "lightpink", "lightblue", "lightgreen", "plum", "lightgray"),
        outline = TRUE)

# --- Filter Rough Rice and Oat ----
rice <- df2$closing_price[df2$crop_type == "Rough Rice"]
oat  <- df2$closing_price[df2$crop_type == "Oat"]

# Determine limits
xmin <- min(c(rice, oat), na.rm = TRUE)
xmax <- max(c(rice, oat), na.rm = TRUE)

# Calculate densities to determine y-axis limit
rice_hist <- hist(rice, plot = FALSE, breaks = 20)
oat_hist  <- hist(oat, plot = FALSE, breaks = 20)

# Normalize counts to get densities
rice_density <- rice_hist$counts / sum(rice_hist$counts) / diff(rice_hist$breaks[1:2])
oat_density  <- oat_hist$counts  / sum(oat_hist$counts)  / diff(oat_hist$breaks[1:2])

#ymax <- max(c(rice_hist$density, oat_hist$density))
ymax <- max(c(rice_density, oat_density))

# Combined histogram
hist(rice,
     probability = TRUE,
     xlim = c(xmin, xmax),
     ylim = c(0, ymax * 1.1),  # Add a little padding
     col = rgb(0, 0, 1, 0.4),
     breaks = 20,
     main = "Combined Histogram: Rough Rice vs Oat \nwith Normal Curves",
     xlab = "Closing Price")

# Add Oat histogram
hist(oat,
     probability = TRUE,
     col = rgb(1, 0, 0, 0.4),
     breaks = 20,
     add = TRUE)

legend("topright",
       legend = c("Rough Rice", "Oat"),
       fill  = c(rgb(0,0,1,0.4), rgb(1,0,0,0.4)),
       border = NA)

# Add normal curves
curve(dnorm(x, mean = mean(rice), sd = sd(rice)),
      add = TRUE, col = "blue", lwd = 2)

curve(dnorm(x, mean = mean(oat), sd = sd(oat)),
      add = TRUE, col = "red", lwd = 2)

t_test_result <- t.test(rice, oat,
                        alternative = "two.sided",
                        var.equal = FALSE)   # Welch t-test

t_test_result

#---------------------------------------
# Line plot : Rough rice
#---------------------------------------

rough_rice_data <- filter(df, crop_type == "Rough Rice")
rough_rice_data <- mutate(rough_rice_data, date = as.Date(date, format="%d-%m-%Y"))

plot(rough_rice_data$date, rough_rice_data$closing_price,
     type = "l", col = "blue",
     xlab = "Date", ylab = "Closing Price",
     main = "Rough rice Closing Prices")

#---------------------------------------
# Line plot : Oat 
#---------------------------------------

oat_data <- filter(df, crop_type == "Oat")
oat_data <- mutate(oat_data, date = as.Date(date, format="%d-%m-%Y"))

plot(oat_data$date, oat_data$closing_price,
     type = "l", col = "blue",
     xlab = "Date", ylab = "Closing Price",
     main = "Oat Closing Prices")

#--------------------------------------------------------
# Average closing prices : Rough Rice and Oat - Pie chart
#--------------------------------------------------------

# Filter only Rough Rice and Oat
df_subset <- df[df$crop_type %in% c("Rough Rice", "Oat"), ]

# Calculate mean closing price for each
avg_values <- aggregate(closing_price ~ crop_type, df_subset, mean)

# Create pie chart
pie(avg_values$closing_price,
    labels = paste(avg_values$crop_type,
                   "\nAvg:", round(avg_values$closing_price, 1)),
    col = c("skyblue", "lightgreen"),
    main = "Average Closing Price: Rough Rice vs Oat")

#--------------------------------------------------------
# Average closing prices : Rough Rice and Oat - Bar plot
#--------------------------------------------------------

# Create bar plot
bp <- barplot(avg_values$closing_price,
              names.arg = avg_values$crop_type,
              col = c("skyblue", "lightgreen"),
              ylim = c(0, max(avg_values$closing_price) * 1.2),
              main = "Average Closing Price: Rough Rice vs Oat",
              ylab = "Average Closing Price",
              xlab = "Crop Type",
              las = 1)

# Add value labels on top of bars
text(x = bp,
     y = avg_values$closing_price,
     labels = round(avg_values$closing_price, 2),
     pos = 3,      # position above the bars
     cex = 1)      # text size

#------------------------------------------------------
# Histogram : Rough rice volume distribution
#------------------------------------------------------

rough_rice_data <- filter(df, crop_type == "Rough Rice")

hist(rough_rice_data$volume,
     col = "lightgreen",
     main = "Rough Rice Trading Volume Distribution",
     xlab = "Volume")

#------------------------------------------------------
# Histogram : Oat volume distribution
#------------------------------------------------------



#------------------------------------------------------
# Average closing price by the type of crops - Bar plot
#------------------------------------------------------

avg_close<-summarise(group_by(df, crop_type),
                     avg_close=mean(closing_price, na.rm = TRUE))

par(mar = c(9,4,4,2))

barplot(avg_close$avg_close,
        names.arg = avg_close$crop_type,
        col = rainbow(nrow(avg_close)),
        main = "Average Closing Price by Crop type",
        las = 2,
        cex.axis = 0.9,
        cex.names = 0.9,
        ylab = "Price",
        ylim = c(0,1900))
        
#--------------------------------------------------------
# Overall average closing prices by commodity - Pie chart
#--------------------------------------------------------
avg_close <- summarise(group_by(df, crop_type),
                       avg_close = mean(closing_price, na.rm = TRUE))

# Convert averages to percentages
total_avg <- sum(avg_close$avg_close, na.rm = TRUE)
avg_close <- mutate(avg_close,
                    perc = (avg_close / total_avg) * 100)

# Build labels with commodity names + percentages
labels <- paste(avg_close$crop_type,
                round(avg_close$perc, 0.1), "%")

# Set graphical parameters
par(mar = c(5, 5, 4, 4))  # Adjust margins
par(cex = 0.9)            # Reduce overall font size

# Pie chart
pie(avg_close$perc,
    labels = labels,
    col = rainbow(6),
    main = "Average Closing Price Share by Crop Type")
