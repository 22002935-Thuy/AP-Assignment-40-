###############################################
## Q4 – ERROR TYPES & FACTORS AFFECTING MAINTENANCE
###############################################

## 1. READ DATASETS
engine <- read.csv("C:/Users/Admin/Downloads/Engine.csv", na.strings = "?")
auto   <- read.csv("C:/Users/Admin/Downloads/Automobile.csv", na.strings = "?")
maint  <- read.csv("C:/Users/Admin/Downloads/Maintenance.csv", na.strings = "?")

## 2. DATA PREPROCESSING
auto$BodyStyles  <- as.factor(auto$BodyStyles)
engine$FuelType  <- as.factor(engine$FuelType)
maint$ErrorCodes <- as.factor(maint$ErrorCodes)
maint$Methods    <- as.factor(maint$Methods)

engine$Horsepower <- as.numeric(engine$Horsepower)
median_hp <- median(engine$Horsepower, na.rm = TRUE)
engine$Horsepower[is.na(engine$Horsepower)] <- median_hp

## 3. MOST FREQUENT ERROR TYPE
error_counts <- table(maint$ErrorCodes)
most_frequent_error <- names(which.max(error_counts))
cat("Most frequent ErrorCode:", most_frequent_error, "\n")
print(error_counts)

## 4. MERGE DATASETS CORRECTLY
# First merge maint + auto using PlateNumber
merged <- merge(maint, auto, by = "PlateNumber", all.x = TRUE)

# Then merge with engine using EngineModel
merged <- merge(merged, engine, by = "EngineModel", all.x = TRUE)

## 5. CROSSTAB ANALYSIS
body_vs_maint <- prop.table(table(merged$BodyStyles, merged$Methods), 1)
fuel_vs_maint <- prop.table(table(merged$FuelType, merged$Methods), 1)

cat("\nBodyStyles vs Maintenance Methods:\n")
print(body_vs_maint)

cat("\nFuelTypes vs Maintenance Methods:\n")
print(fuel_vs_maint)

## 6. VISUALIZATION
library(ggplot2)

# Error codes frequency chart
ggplot(as.data.frame(error_counts), aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Frequency of ErrorCodes", x = "ErrorCodes", y = "Count") +
  theme_minimal()

# BodyStyles vs Methods chart
ggplot(as.data.frame(body_vs_maint), aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity") +
  labs(title = "BodyStyles vs Maintenance Methods", x = "BodyStyles", y = "Proportion") +
  theme_minimal()

# FuelTypes vs Methods chart
ggplot(as.data.frame(fuel_vs_maint), aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity") +
  labs(title = "FuelTypes vs Maintenance Methods", x = "FuelTypes", y = "Proportion") +
  theme_minimal()



                                                    