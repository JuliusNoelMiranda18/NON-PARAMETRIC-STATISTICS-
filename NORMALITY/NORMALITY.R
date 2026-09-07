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
# Note: 'e1071' or 'moments' package is required for skewness/kurtosis 
#install.packages("e1071")
skewness(sbp)
kurtosis(sbp, type = 1)


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
