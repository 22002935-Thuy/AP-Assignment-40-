## =========================================
## Q2 – HORSEPOWER DISTRIBUTION ANALYSIS
## =========================================

# READ 3 DATASETS
engine  <- read.csv("C:/Users/Admin/Downloads/Engine.csv", na.strings = "?")
auto    <- read.csv("C:/Users/Admin/Downloads/Automobile.csv", na.strings = "?")
maint   <- read.csv("C:/Users/Admin/Downloads/Maintenance.csv", na.strings = "?")
# convert categorical variables
auto$BodyStyles <- as.factor(auto$BodyStyles)
engine$FuelType <- as.factor(engine$FuelType)
maint$ErrorCodes <- as.factor(maint$ErrorCodes)

# replace missing horsepower with median
engine$Horsepower <- as.numeric(engine$Horsepower)
median_hp <- median(engine$Horsepower, na.rm = TRUE)
engine$Horsepower[is.na(engine$Horsepower)] <- median_hp
library(ggplot2)

## ===============================
## HORSEPOWER BY ENGINE TYPE
## ===============================

# histogram of horsepower across engine types
ggplot(engine, aes(x = Horsepower, fill = EngineTypes)) +
  geom_histogram(binwidth = 10, alpha = 0.6, position = "identity") +
  labs(title = "Horsepower Distribution Across Engine Types",
       x = "Horsepower", y = "Frequency") +
  theme_minimal()


## ===============================
## HORSEPOWER BY ENGINE SIZE GROUP
## ===============================

# ensure EngineSize numeric
engine$EngineSize <- as.numeric(engine$EngineSize)

# create engine size groups
engine$SizeGroup <- cut(
  engine$EngineSize,
  breaks = c(-Inf, 90, 190, 299, Inf),
  labels = c("60-90", "91-190", "191-299", "300+")
)

# histogram by size group
ggplot(engine, aes(x = Horsepower, fill = SizeGroup)) +
  geom_histogram(binwidth = 10, alpha = 0.6, position = "identity") +
  labs(title = "Horsepower Distribution Across Engine Size Groups",
       x = "Horsepower", y = "Frequency") +
  theme_minimal()
