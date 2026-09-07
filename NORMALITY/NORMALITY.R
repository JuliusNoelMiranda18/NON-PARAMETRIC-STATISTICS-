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


# ==============================================================================
# SECTION 5: CHI-SQUARE GOODNESS-OF-FIT TEST FOR NORMALITY (DYNAMIC COMPUTATION)
# ==============================================================================

# 1. Parameter Estimates from Dataset
mean_sbp <- mean(sbp)
sd_sbp   <- sd(sbp)
n_obs    <- length(sbp)

# 2. Intervals and Class Upper Boundaries
sbp_intervals <- c("< 120", "121 - 130", "131 - 140", "141 - 150", "151 - 160", "> 160")

# 3. Observed Frequencies (Oi) computed from dataset
oi <- c(
  sum(sbp < 120),
  sum(sbp >= 120.5 & sbp <= 130.5),
  sum(sbp >= 130.5 & sbp <= 140.5),
  sum(sbp >= 140.5 & sbp <= 150.5),
  sum(sbp >= 150.5 & sbp <= 160.5),
  sum(sbp > 160.5)
)

# 4. Probabilities (Pi) computed dynamically using normal cumulative distribution function (pnorm)
# Class upper limits (with continuity correction): 120.5, 130.5, 140.5, 150.5, 160.5
p1 <- pnorm(120.5, mean = mean_sbp, sd = sd_sbp)
p2 <- pnorm(130.5, mean = mean_sbp, sd = sd_sbp) - pnorm(120.5, mean = mean_sbp, sd = sd_sbp)
p3 <- pnorm(140.5, mean = mean_sbp, sd = sd_sbp) - pnorm(130.5, mean = mean_sbp, sd = sd_sbp)
p4 <- pnorm(150.5, mean = mean_sbp, sd = sd_sbp) - pnorm(140.5, mean = mean_sbp, sd = sd_sbp)
p5 <- pnorm(160.5, mean = mean_sbp, sd = sd_sbp) - pnorm(150.5, mean = mean_sbp, sd = sd_sbp)
p6 <- 1 - pnorm(160.5, mean = mean_sbp, sd = sd_sbp)

pi <- round(c(p1, p2, p3, p4, p5, p6), 4)

# 5. Expected Frequencies (Ei = N * Pi) computed from scratch
ei <- round(n_obs * pi, 4)

# 6. Chi-Square Components: (Oi - Ei)^2 / Ei computed from scratch
chi_components <- round(((oi - ei)^2) / ei, 5)

# Constructing the Chi-Square Table dynamically:
chi_table <- data.frame(
  SBP_INTERVAL   = sbp_intervals,
  OBSERVED_Oi    = oi,
  PROBABILITY_Pi = pi,
  EXPECTED_Ei    = ei,
  CHI_COMPONENT  = chi_components
)

# Total Chi-Square Test Statistic:
total_oi   <- sum(oi)
total_pi   <- sum(pi)
total_ei   <- sum(ei)
total_chi2 <- sum(chi_components)

cat("\n--- Chi-Square Goodness-of-Fit Test Table (Computed from Scratch) ---\n")
print(chi_table, row.names = FALSE)

cat("\nTotal Observed (N):           ", total_oi, "\n")
cat("Total Probability (Sum Pi):   ", total_pi, "\n")
cat("Total Expected (Sum Ei):      ", total_ei, "\n")
cat("Chi-Square Test Statistic (X2):", round(total_chi2, 5), "\n")


# ==============================================================================
# SECTION 6: CHI-SQUARE GOODNESS-OF-FIT TEST (COMBINED CLASSES)
# ==============================================================================

# Combining classes to satisfy minimum expected frequency requirements:
# Bin 1: < 120 and 121 - 130 combined (Oi = 2 + 8 = 10)
# Bin 2: 131 - 140                    (Oi = 10)
# Bin 3: 141 - 150                    (Oi = 10)
# Bin 4: 151 - 160 and > 160 combined (Oi = 8 + 2 = 10)

sbp_intervals_comb <- c("< 120 / 121 - 130", "131 - 140", "141 - 150", "151 - 160 / > 160")

# 1. Observed Frequencies (Oi) combined dynamically from dataset
oi_comb <- c(
  sum(sbp <= 130.5),
  sum(sbp >= 130.5 & sbp <= 140.5),
  sum(sbp >= 140.5 & sbp <= 150.5),
  sum(sbp >= 150.5)
)

# 2. Probabilities (Pi) computed dynamically for combined bins
p_comb1 <- p1 + p2  # 0.0582 + 0.1537 = 0.2119
p_comb2 <- p3       # 0.2721
p_comb3 <- p4       # 0.2833
p_comb4 <- p5 + p6  # 0.1646 + 0.0681 = 0.2327

pi_comb <- round(c(p_comb1, p_comb2, p_comb3, p_comb4), 4)

# 3. Expected Frequencies (Ei = N * Pi) computed from scratch
ei_comb <- round(n_obs * pi_comb, 3)

# 4. Chi-Square Components: (Oi - Ei)^2 / Ei computed from scratch
chi_components_comb <- round(((oi_comb - ei_comb)^2) / ei_comb, 5)

# Constructing the Combined Chi-Square Table:
chi_table_comb <- data.frame(
  SBP_INTERVAL   = sbp_intervals_comb,
  OBSERVED_Oi    = oi_comb,
  PROBABILITY_Pi = pi_comb,
  EXPECTED_Ei    = ei_comb,
  CHI_COMPONENT  = chi_components_comb
)

# Totals for Combined Table:
total_oi_comb   <- sum(oi_comb)
total_pi_comb   <- sum(pi_comb)
total_ei_comb   <- sum(ei_comb)
total_chi2_comb <- sum(chi_components_comb)

cat("\n--- Chi-Square Goodness-of-Fit Test Table (Combined Classes) ---\n")
print(chi_table_comb, row.names = FALSE)

cat("\nTotal Observed (N):           ", total_oi_comb, "\n")
cat("Total Probability (Sum Pi):   ", total_pi_comb, "\n")
cat("Total Expected (Sum Ei):      ", total_ei_comb, "\n")
cat("Chi-Square Test Statistic (X2):", round(total_chi2_comb, 5), "\n")
