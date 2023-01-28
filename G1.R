library(ggplot2)
library(lubridate)

getwd()
setwd("C:/Users/Filip/CMPT318A1")

df <- read.table("Group_Assignment_1_Dataset.txt", header = TRUE, sep = ",")
head(df)

# Group 16 -> Filter week 16 data
firstDate = as.Date('1/1/2007',format='%d/%m/%Y')
startDate = firstDate + 7*16
print(startDate)
endDate = startDate + 7
print(endDate)

df$Date <- as.Date(df$Date, format= "%d/%m/%Y")
df <- subset(df, Date >= startDate & Date < endDate)
head(df)

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
print(A_geoMean)
A_median <- median(A)
print(A_median)
A_mode <- Mode(A)
print(A_mode)
A_sd <- sd(A)
print(A_sd)

# B geometric mean, median, mode, and standard deviation
B <- na.omit(df$Global_reactive_power)
B_geoMean <- exp(mean(log(B)))
print(B_geoMean)
B_median <- median(B)
print(B_median)
B_mode <- Mode(B)
print(B_mode)
B_sd <- sd(B)
print(B_sd)

# C geometric mean, median, mode, and standard deviation
C <- na.omit(df$Voltage)
C_geoMean <- exp(mean(log(C)))
print(C_geoMean)
C_median <- median(C)
print(C_median)
C_mode <- Mode(C)
print(B_mode)
C_sd <- sd(C)
print(C_sd)

# Format the time column to date time
# Day is defined as 06:00:00 to 17:59:59
# night is defined as 00:00:00 to 05:59:59 and 18:00:00 to 23:59:59
df$Time <- strptime(df$Time, format="%H:%M:%S")
T_6 <- strptime("06:00:00", format="%H:%M:%S")
T_18 <- strptime("18:00:00", format="%H:%M:%S")
dayData <- subset(df, difftime(df$Time, T_6) >= 0 & difftime(df$Time, T_18) < 0)
nightData <- subset(df, difftime(df$Time, T_6) < 0 | difftime(df$Time, T_18) >= 0)

# A Weekday Daytime
A_weekday_daytime <- subset(df, wday(dayData$Date) != 1 | wday(dayData$Date) != 7)
# A Weekday Daytime max
print(max(A_weekday_daytime$Global_active_power))
# A Weekday Daytime min
print(min(A_weekday_daytime$Global_active_power))

# A Weekday Nighttime
A_weekday_nighttime <- subset(df, wday(nightData$Date) != 1 | wday(nightData$Date) != 7)
# A Weekday Nighttime max
print(max(A_weekday_nighttime$Global_active_power))
# A Weekday Nighttime min
print(min(A_weekday_nighttime$Global_active_power))

# A Weekend Daytime
A_weekend_daytime <- subset(df, wday(dayData$Date) == 1 | wday(dayData$Date) == 7)
# A Weekend Daytime max
print(max(A_weekend_daytime$Global_active_power))
# A Weekend Daytime min
print(min(A_weekend_daytime$Global_active_power))

# A Weekend Nightttime
A_weekend_nightttime <- subset(df, wday(nightData$Date) == 1 | wday(nightData$Date) == 7)
# A Weekend Nightttime max
print(max(A_weekend_nighttime$Global_active_power))
# A Weekend Nightttime min
print(min(A_weekend_nighttime$Global_active_power))




# B Weekday Daytime
B_weekday_daytime <- subset(df, wday(dayData$Date) != 1 | wday(dayData$Date) != 7)
# B Weekday Daytime max
print(max(B_weekday_daytime$Global_reactive_power))
# B Weekday Daytime min
print(min(B_weekday_daytime$Global_reactive_power))

# B Weekday Nighttime
B_weekday_nighttime <- subset(df, wday(nightData$Date) != 1 | wday(nightData$Date) != 7)
# B Weekday Nighttime max
print(max(B_weekday_nighttime$Global_reactive_power))
# B Weekday Nighttime min
print(min(B_weekday_nighttime$Global_reactive_power))

# B Weekend Daytime
B_weekend_daytime <- subset(df, wday(dayData$Date) == 1 | wday(dayData$Date) == 7)
# B Weekend Daytime max
print(max(B_weekend_daytime$Global_reactive_power))
# B Weekend Daytime min
print(min(B_weekend_daytime$Global_reactive_power))

# B Weekend Nightttime
B_weekend_nightttime <- subset(df, wday(nightData$Date) == 1 | wday(nightData$Date) == 7)
# B Weekend Nightttime max
print(max(B_weekend_nighttime$Global_reactive_power))
# B Weekend Nightttime min
print(min(B_weekend_nighttime$Global_reactive_power))





