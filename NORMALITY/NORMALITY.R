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

