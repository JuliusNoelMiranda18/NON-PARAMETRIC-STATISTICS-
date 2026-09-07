# ==============================================================================
# SECTION 1: 40 OBSERVATIONS (Systolic Blood Pressure Data)
# ==============================================================================

sbp <- c(112, 118, 121, 124, 125, 126, 127, 128, 129, 130, 
         131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 
         141, 142, 143, 144, 145, 146, 147, 148, 149, 150, 
         151, 152, 153, 154, 155, 156, 158, 160, 163, 168) 


# ==============================================================================
# SECTION 2: NUMERICAL STATISTICS & SUMMARY METRICS
# ==============================================================================

# Sample Size
length(sbp)

# Central Tendency
mean(sbp)
median(sbp)

# Dispersion / Variation
sd(sbp)
min(sbp)
max(sbp)

# Shape Parameters (Normality Assessment)
# Shape Parameters (Normality Assessment)
# Note: 'e1071' or 'moments' package is required for skewness/kurtosis 
# install.packages("e1071")
if (requireNamespace("e1071", quietly = TRUE)) {
  library(e1071)
  skewness(sbp)
  kurtosis(sbp, type = 1)
} else if (requireNamespace("moments", quietly = TRUE)) {
  library(moments)
  skewness(sbp)
  kurtosis(sbp)
}


# ==============================================================================
# SECTION 3: HISTOGRAM & FREQUENCY DISTRIBUTION
# ==============================================================================

# Calculations for Class Intervals:
# Range: R = max(sbp) - min(sbp) = 168 - 112 = 56
# Number of classes: k = sqrt(40) ≈ 6.32 → 6 classes
# Class width: w = ceiling(R / k) = ceiling(56 / 6) = ceiling(9.33) = 10
# Lower bound starts at min(sbp) = 112
# Class Breaks: 112, 122, 132, 142, 152, 162, 172 (6 classes total)

breaks_sbp <- seq(from = min(sbp), to = min(sbp) + 6 * 10, by = 10)

hist(sbp,
     breaks = breaks_sbp,
     right = FALSE,
     main = "Histogram of Systolic Blood Pressure (SBP)",
     xlab = "Systolic Blood Pressure (mmHg)",
     ylab = "Frequency",
     col = "#4E79A7",
     border = "white",
     xaxt = "n")

# Custom x-axis ticks matching class boundaries
axis(1, at = breaks_sbp)


# ==============================================================================
# SECTION 4: BOX PLOT & FIVE-NUMBER SUMMARY (OUTLIER DETECTION)
# ==============================================================================

# Five-Number Summary Calculations (using type = 6 for (N + 1) positioning):
# 
# 1. Q1 Calculation:
#    Q1 Position = (40 + 1) / 4 = 10.25
#    Q1 = X10 + 0.25 * (X11 - X10) = 130 + 0.25 * (131 - 130) = 130.25
q1 <- quantile(sbp, 0.25, type = 6)

# 2. Q2 (Median) Calculation:
#    Q2 Position = 2 * (40 + 1) / 4 = 20.5
#    Q2 = X20 + 0.50 * (X21 - X20) = 140 + 0.50 * (141 - 140) = 140.50
q2 <- quantile(sbp, 0.50, type = 6)

# 3. Q3 & Interquartile Range (IQR) Calculation:
#    Q3 Position = 3 * (40 + 1) / 4 = 30.75
#    Q3 = X30 + 0.75 * (X31 - X30) = 150 + 0.75 * (151 - 150) = 150.75
#    IQR = Q3 - Q1 = 150.75 - 130.25 = 20.50
q3 <- quantile(sbp, 0.75, type = 6)
iqr <- q3 - q1

# 4. Outlier Fences:
#    LEFT FENCE  = Q1 - 1.5 * (IQR) = 130.25 - 1.5 * (20.50) = 99.50
#    RIGHT FENCE = Q3 + 1.5 * (IQR) = 150.75 + 1.5 * (20.50) = 181.50
left_fence  <- q1 - 1.5 * iqr
right_fence <- q3 + 1.5 * iqr

# Display calculated values
cat("Q1 (25th Percentile):", q1, "\n")
cat("Q2 (Median):         ", q2, "\n")
cat("Q3 (75th Percentile):", q3, "\n")
cat("IQR:                 ", iqr, "\n")
cat("Left Fence:          ", left_fence, "\n")
cat("Right Fence:         ", right_fence, "\n")

# Box Plot Creation
boxplot(sbp,
        horizontal = TRUE,
        main = "Boxplot of Systolic Blood Pressure (SBP)",
        xlab = "Systolic Blood Pressure (mmHg)",
        col = "#59A14F",
        border = "#2B5C25",
        pch = 19)

