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

#to rename the coloumn in the dataset
df<- rename(df,crop_type = commodity,
           opening_price = open,
           max_price=high,
           low_price=low,
           closing_price=close )



df2 <- df[df$crop_type %in% c("Rough Rice", "Oat"), ]


# --- 3b. Histogram for oat WITH normal curve ------------------------
oat <- df2$closing_price[df2$crop_type == "Oat"]

hist(oat,
     probability = TRUE,
     main = "Histogram of Oat Closing Prices\nwith Normal Curve",
     xlab = "Closing Price",
     col = "lightgray",
     breaks = 20)
