## =========================================
## Q3 – FUEL EFFICIENCY & TROUBLE ANALYSIS
## =========================================

## --- 1. Read datasets ---
engine  <- read.csv("C:/Users/Admin/Downloads/Engine.csv", na.strings = "?")
auto    <- read.csv("C:/Users/Admin/Downloads/Automobile.csv", na.strings = "?")
maint   <- read.csv("C:/Users/Admin/Downloads/Maintenance.csv", na.strings = "?")

## --- 2. Convert categorical variables ---
engine$FuelType <- as.factor(engine$FuelType)
auto$DriveWheels <- as.factor(auto$DriveWheels)
maint$ErrorCodes <- as.factor(maint$ErrorCodes)

## --- 3. Merge datasets for analysis ---
## Join automobile with engine using EngineModel
df <- merge(auto, engine, by = "EngineModel", all.x = TRUE)

## Join maintenance using PlateNumber
df <- merge(df, maint, by = "PlateNumber", all.x = TRUE)

## --- 4. Research question 1:
## Do diesel cars have higher average CityMpg than gasoline cars?

mean_citympg <- aggregate(CityMpg ~ FuelType, df, mean)

## Perform a t-test comparing diesel vs gas
t_city <- t.test(CityMpg ~ FuelType, data = df)

## --- 5. Research question 2:
## Does DriveWheels affect fuel efficiency?

mean_drive_city  <- aggregate(CityMpg ~ DriveWheels, df, mean)
mean_drive_high  <- aggregate(HwyMpg   ~ DriveWheels, df, mean)

## ANOVA for CityMpg
anova_city <- aov(CityMpg ~ DriveWheels, data = df)

## ANOVA for HighwayMpg
anova_high <- aov(HwyMpg ~ DriveWheels, data = df)

## --- 6. Research question 3:
## Filter engines that have trouble or suspected trouble
df_trouble <- df[df$ErrorCodes != 0 & !is.na(df$ErrorCodes), ]

## Count most common troubles
top_troubles <- sort(table(df_trouble$Troubles), decreasing = TRUE)[1:5]

## Compare trouble distribution across engine types
trouble_by_engine_type <- table(df_trouble$EngineTypes, df_trouble$Troubles)
