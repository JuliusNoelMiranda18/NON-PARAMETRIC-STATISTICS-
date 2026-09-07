# NON-PARAMETRIC-STATISTICS-

A collection of R scripts exploring key topics and activities in nonparametric statistics.

---

## TOPIC 1: NORMALITY

This covers numerical and graphical methods to assess whether a dataset follows a normal distribution, using 40 observations of Systolic Blood Pressure (SBP) as a sample dataset.

### Components
1. **Numerical Statistics**: Calculates sample size (\(N\)), central tendency (mean, median), measures of dispersion (SD, min, max), and shape parameters (skewness and kurtosis).
2. **Frequency Distribution & Histogram**: Computes range (\(R\)), number of classes (\(k\)), class width (\(w\)), and renders a histogram with custom class boundaries starting at the minimum value.
3. **Five-Number Summary & Boxplot**: Computes quartiles (\(Q_1\), \(Q_2\), \(Q_3\)), Interquartile Range (\(IQR\)), and outlier fences (\(Q_1 - 1.5 \cdot IQR\) and \(Q_3 + 1.5 \cdot IQR\)) to generate a boxplot.
4. **Chi-Square Goodness-of-Fit Test**: Evaluates normality using both standard (uncombined) and combined class interval tables computed dynamically from scratch.

