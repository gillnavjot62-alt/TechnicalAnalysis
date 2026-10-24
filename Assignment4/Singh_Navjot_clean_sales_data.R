# AI Assistance Declaration:
# I used ChatGPT (GPT-5.6 Sol) for data-cleaning ideas, R code drafting,
# debugging suggestions, comments, and document structure on October 24, 2026.
# Prompts used are listed in Appendix_AI_Use.txt.
# I verified outputs by running the code in RStudio, reviewing summary(), glimpse(),
# table(), nrow(), sum(is.na()), duplicated(), and before/after checks.
# All final calculations are performed directly in R on my computer.
# I am responsible for the accuracy and originality of this work.

# BDA400 Assignment 4 - Ask AI Clean This Dataset with R and dplyr
# Student: Navjot Singh

library(dplyr)

# 1. Load original data
sales_raw <- read.csv("sales_data_dirty.csv", stringsAsFactors = FALSE)

cat("===== BEFORE CLEANING =====\n")
cat("Rows:", nrow(sales_raw), "\n")
cat("Total missing values:", sum(is.na(sales_raw)), "\n")
cat("Exact duplicate rows:", sum(duplicated(sales_raw)), "\n")
cat("Duplicate OrderID values:", sum(duplicated(sales_raw$OrderID)), "\n\n")
print(summary(sales_raw))
cat("\nProduct labels:\n")
print(table(sales_raw$Product, useNA = "ifany"))
cat("\nCustomer labels:\n")
print(table(sales_raw$Customer, useNA = "ifany"))

# 2. Calculate Quantity outlier limits with the IQR method in R
q1_qty <- quantile(sales_raw$Quantity, 0.25, na.rm = TRUE)
q3_qty <- quantile(sales_raw$Quantity, 0.75, na.rm = TRUE)
iqr_qty <- IQR(sales_raw$Quantity, na.rm = TRUE)
lower_qty <- q1_qty - 1.5 * iqr_qty
upper_qty <- q3_qty + 1.5 * iqr_qty

cat("\nQuantity IQR limits:", lower_qty, "to", upper_qty, "\n")

# 3. Clean the data
sales_clean <- sales_raw %>%
  distinct() %>%                                      # remove exact duplicate rows
  mutate(
    Product = trimws(Product),
    Customer = trimws(Customer),
    Product = case_when(
      tolower(Product) == "widget" ~ "Widget",
      tolower(Product) == "gadget" ~ "Gadget",
      tolower(Product) == "thingamajig" ~ "Thingamajig",
      TRUE ~ Product
    ),
    Customer = tools::toTitleCase(tolower(Customer))  # standardize customer text
  ) %>%
  filter(!is.na(Product),                            # remove missing product values
         !is.na(Quantity),                           # remove missing quantity values
         Quantity > 0,                               # remove zero/negative quantities
         Quantity >= lower_qty,
         Quantity <= upper_qty) %>%                  # remove IQR quantity outliers
  arrange(OrderID) %>%
  distinct(OrderID, .keep_all = TRUE)                # ensure one record per order

# 4. Verification checks
cat("\n\n===== AFTER CLEANING =====\n")
cat("Rows:", nrow(sales_clean), "\n")
cat("Total missing values:", sum(is.na(sales_clean)), "\n")
cat("Exact duplicate rows:", sum(duplicated(sales_clean)), "\n")
cat("Duplicate OrderID values:", sum(duplicated(sales_clean$OrderID)), "\n\n")
print(summary(sales_clean))
cat("\nProduct labels:\n")
print(table(sales_clean$Product, useNA = "ifany"))
cat("\nCustomer labels:\n")
print(table(sales_clean$Customer, useNA = "ifany"))

# 5. Sanity checks - these should all be TRUE after cleaning
checks <- c(
  no_missing_product = sum(is.na(sales_clean$Product)) == 0,
  no_missing_quantity = sum(is.na(sales_clean$Quantity)) == 0,
  no_exact_duplicates = sum(duplicated(sales_clean)) == 0,
  unique_order_ids = sum(duplicated(sales_clean$OrderID)) == 0,
  positive_quantity = all(sales_clean$Quantity > 0),
  quantity_within_iqr_limits = all(sales_clean$Quantity >= lower_qty & sales_clean$Quantity <= upper_qty)
)
cat("\nVerification checks:\n")
print(checks)

# 6. Save cleaned data
write.csv(sales_clean, "sales_data_clean.csv", row.names = FALSE)

# 7. Save before/after text output for submission evidence
sink("before_after_summary.txt")
cat("BDA400 Assignment 4 - Before/After Verification\n")
cat("Student: Navjot Singh\n\n")
cat("BEFORE CLEANING\n")
cat("Rows:", nrow(sales_raw), "\n")
cat("Total missing values:", sum(is.na(sales_raw)), "\n")
cat("Exact duplicate rows:", sum(duplicated(sales_raw)), "\n")
cat("Duplicate OrderID values:", sum(duplicated(sales_raw$OrderID)), "\n")
print(summary(sales_raw))
cat("\nAFTER CLEANING\n")
cat("Rows:", nrow(sales_clean), "\n")
cat("Total missing values:", sum(is.na(sales_clean)), "\n")
cat("Exact duplicate rows:", sum(duplicated(sales_clean)), "\n")
cat("Duplicate OrderID values:", sum(duplicated(sales_clean$OrderID)), "\n")
print(summary(sales_clean))
cat("\nVerification checks:\n")
print(checks)
sink()

cat("\nCleaning complete. Files created: sales_data_clean.csv and before_after_summary.txt\n")
