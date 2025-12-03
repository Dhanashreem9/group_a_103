###   ####
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
# summarize the dataset
#---------------------------------------
summary(df)

#---------------------------------------
#display first 5 rows
#---------------------------------------
head(df, 5)


### ###
# to rename the columns in the dataset
df <- rename(df, crop_type  = commodity,
             opening_price = open,
             max_price = high,
             low_price = low,
             closing_price = close)

df

summary(df)

# Identify the commodity with the highest mean closing price
mean_close <- aggregate(closing_price ~ crop_type, df, mean)
highest <- mean_close$crop_type[which.max(mean_close$closing_price)]

#------------------------
# --- 1. Filter Rough Rice and Corn ----------------------------------
df2 <- df[df$crop_type %in% c("Rough Rice", "Oat"), ]

# --- 3a. Histogram for Rough Rice WITH normal curve ------------------
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

# --- 3b. Histogram for Corn WITH normal curve ------------------------
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


#---------------------------------------
#
#---------------------------------------
# --- Filter Rough Rice and Oat ----
df2 <- df[df$crop_type %in% c("Rough Rice", "Oat"), ]

rice <- df2$closing_price[df2$crop_type == "Rough Rice"]
oat  <- df2$closing_price[df2$crop_type == "Oat"]

# --- Determine common x-axis range ---
xmin <- min(c(rice, oat), na.rm = TRUE)
xmax <- max(c(rice, oat), na.rm = TRUE)

# --- Combined Histogram ---
hist(rice,
     probability = TRUE,
     xlim = c(xmin, xmax),
     col = rgb(0, 0, 1, 0.4),        # Blue with transparency
     breaks = 20,
     main = "Combined Histogram of Closing Prices\nRough Rice vs Oat",
     xlab = "Closing Price ($)")

hist(oat,
     probability = TRUE,
     col = rgb(1, 0, 0, 0.4),        # Red with transparency
     breaks = 20,
     add = TRUE)

legend("topright",
       legend = c("Rough Rice", "Oat"),
       fill = c(rgb(0, 0, 1, 0.4), rgb(1, 0, 0, 0.4)))
####

# Determine limits
xmin <- min(c(rice, oat), na.rm = TRUE)
xmax <- max(c(rice, oat), na.rm = TRUE)

# Estimate peak densities
rice_peak <- dnorm(mean(rice), mean = mean(rice), sd = sd(rice))
oat_peak  <- dnorm(mean(oat), mean = mean(oat), sd = sd(oat))

# Set ylim slightly above the highest peak
ymax <- max(rice_peak, oat_peak) * 1.2  # Add 20% buffer

# Combined histogram
hist(rice,
     probability = TRUE,
     xlim = c(xmin, xmax),
     ylim = c(0,ymax),
     col = rgb(0, 0, 1, 0.4),
     breaks = 20,
     main = "Combined Histogram: Rough Rice vs Oat\nwith Normal Curves",
     xlab = "Closing Price")

# Add Oat histogram
hist(oat,
     probability = TRUE,
     col = rgb(1, 0, 0, 0.4),
     breaks = 20,
     add = TRUE)

# Add normal curves
curve(dnorm(x, mean = mean(rice), sd = sd(rice)),
      add = TRUE, col = "blue", lwd = 2)

curve(dnorm(x, mean = mean(oat), sd = sd(oat)),
      add = TRUE, col = "red", lwd = 1)

legend("topright",
       legend = c("Rough Rice", "Oat"),
       fill  = c(rgb(0,0,1,0.4), rgb(1,0,0,0.4)),
       border = NA)




# QQ plots for normality checks
par(mfrow = c(1,2))

qqnorm(rice, main = "QQ Plot - Rough Rice")
qqline(rice)

qqnorm(oat, main = "QQ Plot - Oat")
qqline(oat)

par(mfrow = c(1,1))



# Filter Rough Rice and Oat
df2 <- df[df$crop_type %in% c("Rough Rice", "Oat"), ]

rice <- df2$closing_price[df2$crop_type == "Rough Rice"]
oat  <- df2$closing_price[df2$crop_type == "Oat"]

# --- Boxplot ---
boxplot(closing_price ~ crop_type,
        data = df2,
        main = "Boxplot of Closing Prices: Rough Rice vs Oat",
        xlab = "Commodity",
        ylab = "Closing Price ($)",
        col = c("lightblue", "lightgreen"))



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

# Average closing price by commodity
avg_close <- summarise(group_by(df, crop_type),
                       avg_close = mean(closing_price, na.rm = TRUE))

# Convert averages to percentages
total_avg <- sum(avg_close$avg_close, na.rm = TRUE)
avg_close <- mutate(avg_close,
                    perc = (avg_close / total_avg) * 100)

# Build labels with commodity names + percentages
labels <- paste(avg_close$crop_type,
                round(avg_close$perc, 1), "%")