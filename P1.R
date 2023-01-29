library(ggplot2)
library(lubridate)

getwd()
setwd("C:/Documents/School/6 Spring 2023/CMPT 318/G1/CMPT318A1")
df <- read.table("Group_Assignment_1_Dataset.txt", header = TRUE, sep = ",")

# Group 16 -> Filter week 16 data
firstDate = as.Date('1/1/2007',format='%d/%m/%Y')
startDate = firstDate + 7*16
endDate = startDate + 7

df$Date <- as.Date(df$Date, format= "%d/%m/%Y")
df <- subset(df, Date >= startDate & Date < endDate)

# Part 1: Compute the arithmetic and the geometric mean, the median, the mode and the standard
# deviation for features A, B and C respectively. For features A and B compute the and
# values on weekdays and weekend days during day hours and night hours respectively
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# A geometric mean, median, mode, and standard deviation
A <- df$Global_active_power
A_geoMean <- exp(mean(log(A)))
cat("A Geometric Mean: ", A_geoMean)
A_median <- median(A)
cat("A Median: ", A_median)
A_mode <- Mode(A)
cat("A Mode: ", A_mode)
A_sd <- sd(A)
cat("A Standard Deviation: ", A_sd)

# B geometric mean, median, mode, and standard deviation
B <- na.omit(df$Global_reactive_power)
B_geoMean <- exp(mean(log(B)))
cat("B Geometric Mean: ", B_geoMean)
B_median <- median(B)
cat("B Median: ", B_median)
B_mode <- Mode(B)
cat("B Mode: ", B_mode)
B_sd <- sd(B)
cat("B Standard Deviation: ", B_sd)

# C geometric mean, median, mode, and standard deviation
C <- na.omit(df$Voltage)
C_geoMean <- exp(mean(log(C)))
cat("C Geometric Mean: ", C_geoMean)
C_median <- median(C)
cat("C Median: ", C_median)
C_mode <- Mode(C)
cat("B Mode: ", B_mode)
C_sd <- sd(C)
cat("C Standard Deviation", C_sd)

# Format the time column to date time
# Day is defined as 06:00:00 to 17:59:59
# night is defined as 00:00:00 to 05:59:59 and 18:00:00 to 23:59:59
df$Time <- strptime(df$Time, format="%H:%M:%S")
T_6 <- strptime("06:00:00", format="%H:%M:%S")
T_18 <- strptime("18:00:00", format="%H:%M:%S")
dayData <- subset(df, difftime(df$Time, T_6) >= 0 & difftime(df$Time, T_18) < 0)
nightData <- subset(df, (difftime(df$Time, T_0) >= 0 & difftime(df$Time, T_6) < 0) 
                    | (difftime(df$Time, T_18) >= 0 & difftime(df$Time, T_0) < 0))

# A Weekday Daytime
A_weekday_daytime <- subset(df, wday(dayData$Date) != 1 | wday(dayData$Date) != 7)
cat("A Weekday Daytime Max: ", max(A_weekday_daytime$Global_active_power))
cat("A Weekday Daytime Min: ", min(A_weekday_daytime$Global_active_power))

# A Weekday Nighttime
A_weekday_nighttime <- subset(df, wday(nightData$Date) != 1 | wday(nightData$Date) != 7)
cat("A Weekday Nighttime Max: ", max(A_weekday_nighttime$Global_active_power))
cat("A Weekday Nighttime Min: ", min(A_weekday_nighttime$Global_active_power))

# A Weekend Daytime
A_weekend_daytime <- subset(df, wday(dayData$Date) == 1 | wday(dayData$Date) == 7)
cat("A Weekend Daytime Max: ", max(A_weekend_daytime$Global_active_power))
cat("A Weekend Daytime Min: ", min(A_weekend_daytime$Global_active_power))

# A Weekend Nightttime
A_weekend_nighttime <- subset(df, wday(nightData$Date) == 1 | wday(nightData$Date) == 7)
cat("A Weekend Nighttime Max: ", max(A_weekend_nighttime$Global_active_power))
cat("A Weekend Nighttime Min: ", min(A_weekend_nighttime$Global_active_power))


# B Weekday Daytime
B_weekday_daytime <- na.omit(subset(df, wday(dayData$Date) != 1 & wday(dayData$Date) != 7))
cat("B Weekday Daytime Max: ", max(B_weekday_daytime$Global_reactive_power))
cat("B Weekday Daytime Min: ", min(B_weekday_daytime$Global_reactive_power))

# B Weekday Nighttime
B_weekday_nighttime <- na.omit(subset(df, wday(nightData$Date) != 1 | wday(nightData$Date) != 7))
cat("B Weekday Nighttime Max: ", max(B_weekday_nighttime$Global_reactive_power))
cat("B Weekday Nighttime Min: ", min(B_weekday_nighttime$Global_reactive_power))

# B Weekend Daytime
B_weekend_daytime <- na.omit(subset(df, wday(dayData$Date) == 1 | wday(dayData$Date) == 7))
cat("B Weekend Daytime Max: ", max(B_weekend_daytime$Global_reactive_power))
cat("B Weekend Daytime Min: ", min(B_weekend_daytime$Global_reactive_power))

# B Weekend Nightttime
B_weekend_nighttime <- na.omit(subset(df, wday(nightData$Date) == 1 | wday(nightData$Date) == 7))
cat("B Weekend Nightttime Max: ", max(B_weekend_nighttime$Global_reactive_power))
cat("B Weekend Nightttime Min: ", min(B_weekend_nighttime$Global_reactive_power))





