#############################################
# Q1: Data inspection and preprocessing
# ----- 1. Read 3 datasets -----
engine_path <- "C:/Users/Admin/Downloads/Engine.csv"
automobile_path <- "C:/Users/Admin/Downloads/Automobile.csv"
maintenance_path <- "C:/Users/Admin/Downloads/Maintenance.csv"
engine <- read.csv(engine_path, stringsAsFactors = FALSE)
automobile <- read.csv(automobile_path, stringsAsFactors = FALSE)
maintenance <- read.csv(maintenance_path, stringsAsFactors = FALSE)


# ----- 2. Check the initial data structure -----
# Print the number of rows / columns and the first few lines to display in the report
cat("Engine: dim = ", dim(engine), "\n")
print(str(engine))
cat("Head Engine:\n"); print(head(engine, 5))


cat("\nAutomobile: dim = ", dim(automobile), "\n")
print(str(automobile))
cat("Head Automobile:\n"); print(head(automobile, 5))


cat("\nMaintenance: dim = ", dim(maintenance), "\n")
print(str(maintenance))
cat("Head Maintenance:\n"); print(head(maintenance, 5))


# ----- 3. Count the number of rows 'affected' (containing at least one '?' value) -----
# Function to check how many rows in a dataframe contain "?"
count_rows_with_qmark <- function(df) { 
  # direct comparison: convert everything to character first to avoid type errors 
  m <- apply(df, 1, function(r) any(as.character(r) == "?", na.rm = TRUE)) 
  sum(m)
}


affected_engine <- count_rows_with_qmark(engine)
affected_automobile <- count_rows_with_qmark(automobile)
affected_maintenance <- count_rows_with_qmark(maintenance)
total_affected_rows <- affected_engine + affected_automobile + affected_maintenance


cat("\nRows containing '?' before replacement:\n")
cat(" Engine: ", affected_engine, "\n")
cat(" Automobile: ", affected_automobile, "\n")
cat(" Maintenance: ", affected_maintenance, "\n")
cat(" TOTAL affected rows: ", total_affected_rows, "\n")


# ----- 4. Replace all "?" with NA for all 3 datasets -----
replace_qmark_with_na <- function(df) {
  df[df == "?"] <- NA
  return(df)
}
engine <- replace_qmark_with_na(engine)
automobile <- replace_qmark_with_na(automobile)
maintenance <- replace_qmark_with_na(maintenance)


# ----- 5. Check if the change changes the Horsepower distribution -----
# Create a horsepower_before variable from engine (if any)
if ("Horsepower" %in% names(engine)) {
  # Before replacement, they were replaced with "?" -> NA; to compare, we create numeric value
  hp_before <- as.numeric( engine$Horsepower ) # as.numeric will convert character -> numeric, NA if not converted
  # Summary before impute
  cat("\nHorsepower summary (before impute / after converting to numeric):\n")
  print(summary(hp_before))
  cat("Count NA in horsepower before impute: ", sum(is.na(hp_before)), "\n")
} else {
  warning("Column 'Horsepower' does not exist in Engine dataset.")
  hp_before <- NULL
}


# ----- 6. Convert categorical variables to factor (if exist) -----
# Requirements: BodyStyles (Automobile), FuelTypes / FuelType (Engine), ErrorCodes (Maintenance)
# Check specific column name and use existing name
if ("BodyStyles" %in% names(automobile)) { 
  automobile$BodyStyles <- factor(automobile$BodyStyles) 
  cat("\nConverted Automobile$BodyStyles to factor. Levels:", levels(automobile$BodyStyles), "\n")
} else if ("BodyStyle" %in% names(automobile)) { 
  # sometimes the filename is BodyStyle (singular) 
  automobile$BodyStyle <- factor(automobile$BodyStyle) 
  cat("\nConverted Automobile$BodyStyle to factor. Levels:", levels(automobile$BodyStyle), "\n")
} else { 
  cat("\nWarning: BodyStyles (or BodyStyle) column not found in Automobile.\n")
}


# FuelType(s) - engines
if ("FuelTypes" %in% names(engine)) { 
  engine$FuelTypes <- factor(engine$FuelTypes) 
  cat("\nConverted Engine$FuelTypes to factor. Levels:", levels(engine$FuelTypes), "\n")
} else if ("FuelType" %in% names(engine)) { 
  engine$FuelType <- factor(engine$FuelType) 
  cat("\nConverted Engine$FuelType to factor. Levels:", levels(engine$FuelType), "\n")
} else { 
  cat("\nWarning: FuelType(s) column not found in Engine dataset.\n")
}


# ErrorCodes - maintenance
if ("ErrorCodes" %in% names(maintenance)) { 
  maintenance$ErrorCodes <- factor(maintenance$ErrorCodes) 
  cat("\nConverted Maintenance$ErrorCodes to factor. Levels:", levels(maintenance$ErrorCodes), "\n")
} else { 
  cat("\nWarning: ErrorCodes column not found in Maintenance dataset.\n")
}


# ----- 7. Replace missing values ​​in Horsepower with median (if column exists) -----
if (!is.null(hp_before)) { 
  # Create numeric version of horsepower (after replacement "?"->NA) 
  engine$Horsepower <- as.numeric(engine$Horsepower) 
  # Calculate median removing NA 
  hp_median <- median(engine$Horsepower, na.rm = TRUE) 
  cat("\nMedian(Horsepower, na.rm=TRUE) = ", hp_median, "\n") 
  # Number of NAs before impute 
  na_before <- sum(is.na(engine$Horsepower)) 
  cat("Number of NA in Horsepower before impute: ", na_before, "\n") 
  # Assign median to NA 
  engine$Horsepower[is.na(engine$Horsepower)] <- hp_median 
  na_after <- sum(is.na(engine$Horsepower)) 
  cat("Number of NA in Horsepower after impute: ", na_after, "\n")
  # Compare summary before and after impute (to check for distribution changes)
  cat("\nSummary of Horsepower after imputing median:\n")
  print(summary(engine$Horsepower))
} else {
  cat("\nSkipping Horsepower imputation because column is missing.\n")
}


# ----- 8. Plotting Horsepower Distribution -----
# Load ggplot2
library(ggplot2)


# Plot histogram + density overlay for horsepower distribution
if ("Horsepower" %in% names(engine)) {
  g <- ggplot(engine, aes(x = Horsepower)) +
    geom_histogram(aes(y = ..density..), bins = 30) + # histogram
    geom_density(alpha = 0.3) + # density line to see distribution
    ggtitle("Horsepower distribution (after imputing median)") +
    xlab("Horsepower") + ylab("Density")
  
  
  # Save the plot to a PNG file (if needed) and also display it in RStudio
  ggsave(filename = "horsepower_distribution.png", plot = g, width = 7, height = 5, dpi = 150)
  print(g) # print it in RStudio Viewer / Plots pane
} else {
  cat("Cannot plot: 'Horsepower' column not found.\n")
}


# ----- 9. Test / report main results to paste into report -----
cat("\n--- Summary results to paste into report ---\n")
cat("Total rows affected (contained '?') before replacement: ", total_affected_rows, "\n")
cat("Engine dim:", dim(engine), "\n")
if ("Horsepower" %in% names(engine)) { 
  cat("Horsepower median used for imputation: ", hp_median, "\n") 
  cat("Horsepower summary (after impute):\n") 
  print(summary(engine$Horsepower))
}
cat("Factors converted (if present): BodyStyles/BodyStyle, FuelType(s), ErrorCodes\n")
cat("Saved horsepower plot to 'horsepower_distribution.png' in working directory.\n")
