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
cat("A Arithmetic Mean: ", mean(A, na.rm = TRUE))
cat("A Geometric Mean: ", exp(mean(log(A))))
cat("A Median: ", median(A))
cat("A Mode: ", Mode(A))
cat("A Standard Deviation: ", sd(A))

# B geometric mean, median, mode, and standard deviation
B <- na.omit(df$Global_reactive_power)
cat("B Arithmetic Mean: ", mean(B, na.rm = TRUE))
cat("B Geometric Mean: ", exp(mean(log(B))))
cat("B Median: ", median(B))
cat("B Mode: ", Mode(B))
cat("B Standard Deviation: ", sd(B))

# C geometric mean, median, mode, and standard deviation
C <- na.omit(df$Voltage)
cat("C Arithmetic Mean: ", mean(C, na.rm = TRUE))
cat("C Geometric Mean: ", exp(mean(log(C))))
cat("C Median: ", median(C))
cat("B Mode: ", Mode(C))
cat("C Standard Deviation", sd(C))

# Format the time column to date time
# Day is defined as 06:00:00 to 17:59:59
# night is defined as 00:00:00 to 05:59:59 and 18:00:00 to 23:59:59
df$Time <- strptime(df$Time, format="%H:%M:%S")
T_0 <- strptime("00:00:00", format="%H:%M:%S")
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



# Part 3: Calculating average Global_intensity values, Linear and Polynomial Regression, graphing results
Weekday_day_averages <- subset(A_weekday_daytime, select = c("Time", "Global_intensity"))
Weekday_day_averages <- aggregate(Weekday_day_averages$Global_intensity ~ format(Weekday_day_averages$Time, "%H:%M"), Weekday_day_averages, mean)
colnames(Weekday_day_averages)[1] <- "Time"
colnames(Weekday_day_averages)[2] <- "Global_intensity_avg"
head(Weekday_day_averages)

Weekday_night_averages <- subset(A_weekday_nighttime, select = c("Time", "Global_intensity"))
Weekday_night_averages <- aggregate(Weekday_night_averages$Global_intensity ~ format(Weekday_night_averages$Time, "%H:%M"), Weekday_night_averages, mean)
colnames(Weekday_night_averages)[1] <- "Time"
colnames(Weekday_night_averages)[2] <- "Global_intensity_avg"
head(Weekday_night_averages)

Weekend_day_averages <- subset(A_weekend_daytime, select = c("Time", "Global_intensity"))
Weekend_day_averages <- aggregate(Weekend_day_averages$Global_intensity ~ format(Weekend_day_averages$Time, "%H:%M"), Weekend_day_averages, mean)
colnames(Weekend_day_averages)[1] <- "Time"
colnames(Weekend_day_averages)[2] <- "Global_intensity_avg"

Weekend_night_averages <- subset(A_weekend_nighttime, select = c("Time", "Global_intensity"))
Weekend_night_averages <- aggregate(Weekend_night_averages$Global_intensity ~ format(Weekend_night_averages$Time, "%H:%M"), Weekend_night_averages, mean)
colnames(Weekend_night_averages)[1] <- "Time"
colnames(Weekend_night_averages)[2] <- "Global_intensity_avg"

# Weekday daytime
Weekday_day_fit_linear <- lm(Global_intensity_avg ~ as.numeric(hm(Time)), data = Weekday_day_averages)
Weekday_day_fit_polynomial <- lm(Global_intensity_avg~poly(as.numeric(hm(Time)),4,raw=TRUE), data=Weekday_day_averages)
summary(Weekday_day_fit_linear)
summary(Weekday_day_fit_polynomial)

# Weekday nigttime
Weekday_night_fit_linear <- lm(Global_intensity_avg ~ as.numeric(hm(Time)), data = Weekday_night_averages)
Weekday_night_fit_polynomial <- lm(Global_intensity_avg~poly(as.numeric(hm(Time)),4,raw=TRUE), data=Weekday_night_averages)
summary(Weekday_night_fit_linear)
summary(Weekday_night_fit_polynomial)

# Weekend daytime
Weekend_day_fit_linear <- lm(Global_intensity_avg ~ as.numeric(hm(Time)), data = Weekend_day_averages)
Weekend_day_fit_polynomial <- lm(Global_intensity_avg~poly(as.numeric(hm(Time)),4,raw=TRUE), data=Weekend_day_averages)
summary(Weekend_day_fit_linear)
summary(Weekend_day_fit_polynomial)


# Weekend nighttime
Weekend_night_fit_linear <- lm(Global_intensity_avg ~ as.numeric(hm(Time)), data = Weekend_night_averages)
Weekend_night_fit_polynomial<- lm(Global_intensity_avg~poly(as.numeric(hm(Time)),4,raw=TRUE), data=Weekend_night_averages)
summary(Weekend_night_fit_linear)
summary(Weekend_night_fit_polynomial)

# Graph linear fits
ggplot(data=Weekday_day_averages, mapping=aes(x=Time, y=Global_intensity_avg)) + 
  geom_line(aes(y=predict(Weekday_day_fit_linear)), color="red", group=1) + 
  geom_line(aes(y=predict(Weekday_night_fit_linear)), color="blue", group=2) +
  geom_line(aes(y=predict(Weekend_day_fit_linear)), color="green", group=3) +
  geom_line(aes(y=predict(Weekend_night_fit_linear)), color="pink", group=4)+
  ggtitle("Linear Fits") +
  ylab("Average Global Intensity") + 
  xlab("Time")
  
# Graph poly fits
ggplot(data=Weekday_day_averages, mapping=aes(x=Time, y=Global_intensity_avg)) + 
  geom_line(aes(y=predict(Weekday_day_fit_polynomial)), color="red", group=1) + 
  geom_line(aes(y=predict(Weekday_night_fit_polynomial)), color="blue", group=2) +
  geom_line(aes(y=predict(Weekend_day_fit_polynomial)), color="green", group=3) +
  geom_line(aes(y=predict(Weekend_night_fit_polynomial)), color="pink", group=4) +
  ggtitle("Polynomial Fits") +
  ylab("Average Global Intensity") + 
  xlab("Time")

