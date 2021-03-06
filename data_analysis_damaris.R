library(data.table)
library(psych)

# ------------------Data Loading and Preparation ------------------------
#load data
data <- fread("survey_results_manualclean_nocheaters.csv")
#remove empty rows
data <- data[-(74:1030),]

# replace NANs with NA
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

data[is.nan(data)] <- NA


# -------------NA------------------------------------
#Prüfung ob Kompetenzvariabeln NA normalverteilt

# Shapiro Wilk normality test
shapiro.test(data$KompetenzNA)
# W = 0.9744, p-value = 0.1746
# da p > 0.05 sind Daten normalverteilt
hist(data$KompetenzNA)
# Histogram ist etwa normalverteilt - Bestätigt Shapiro-Wilk Test

# -------------WA------------------------------------
#Prüfung ob Kompetenzvariabeln WA normalverteilt
# Shapiro Wilk normality test
shapiro.test(data$KompetenzWA)
# W = 0.98547, p-value = 0.6054
# da p > 0.05 sind Daten normalverteilt
hist(data$KompetenzWA)
# Histogram ist etwa normalverteilt - Bestätigt Shapiro-Wilk Test

# -------------SA------------------------------------
#Prüfung ob Kompetenzvariabeln SA normalverteilt
# Shapiro Wilk normality test
shapiro.test(data$KompetenzSA)
# W = 0.96794, p-value = 0.07708
# da p > 0.05 sind Daten normalverteilt
hist(data$KompetenzSA)
# Histogram ist etwa normalverteilt - Bestätigt Shapiro-Wilk Test




# --------------------------- Welch Two Sample t-Test NA-SA ----------------------------
# Vorbedingung: Ist Varianz etwa gleich?
describeBy(data$KompetenzSA, na.rm = TRUE)
describeBy(data$KompetenzNA, na.rm = TRUE)
# KompetenzSA sd = 0.61; KompetenzNA = 0.5

# Nullhypothese: No difference between averages of the two groups

# ---Alternativhypothese: SA ist weniger kompetent als NA
t.test(data$KompetenzNA, data$KompetenzSA, alternative = "less", na.rm = TRUE)
#t = -2.76, df = 128.39, p-value = 0.003313
#alternative hypothesis: true difference in means is less than 0


# ---Alternativhypothese: SA ist kompetenter als NA
t.test(data$KompetenzNA, data$KompetenzSA, alternative = "greater", na.rm = TRUE)
#t = -2.76, df = 128.39, p-value = 0.9967
#alternative hypothesis: true difference in means is greater than 0
# ---> there is not enough evidence of a difference between the (true) averages of the two groups

# ---Two-sided
t.test(data$KompetenzNA, data$KompetenzSA, alternative = "two.sided", na.rm = TRUE)
# t = -2.76, df = 128.39, p-value = 0.006627
# alternative hypothesis: true difference in means is not equal to 0





# --------------------------- Wilcoxon NA-SA ----------------------------
describe(data$KompetenzNA)
#    vars  n mean  sd median trimmed  mad  min  max range  skew kurtosis   se
# X1    1 68  3.2 0.5   3.25    3.23 0.37 1.92 4.25  2.33 -0.46        0 0.06
describe(data$KompetenzSA)
#   vars  n mean   sd median trimmed  mad  min  max range  skew kurtosis   se
#X1    1 68 3.47 0.61    3.5     3.5 0.49 1.67 4.67     3 -0.57     0.48 0.07

# Ist die wahrgenommene Kompetenz des NA geringer als diejenige des SA?
wilcox.test(data$KompetenzNA, data$KompetenzSA, paired = TRUE, exact = FALSE, correct = TRUE, alternative = "less")
# V = 540, p-value = 0.001804
# alternative hypothesis: true location shift is less than 0
# --> Ja

# --------------------------- Wilcoxon NA-WA ----------------------------
describe(data$KompetenzNA)
#    vars  n mean  sd median trimmed  mad  min  max range  skew kurtosis   se
# X1    1 68  3.2 0.5   3.25    3.23 0.37 1.92 4.25  2.33 -0.46        0 0.06
describe(data$KompetenzWA)
#    vars  n mean  sd median trimmed  mad  min  max range skew kurtosis   se
# X1    1 69 3.06 0.6   2.92    3.06 0.62 1.75 4.67  2.92 0.22    -0.43 0.07

# Ist die wahrgenommene Kompetenz des NA geringer als diejenige des WA?
wilcox.test(data$KompetenzNA, data$KompetenzWA, paired = TRUE, exact = FALSE, correct = TRUE, alternative = "less")
# V = 1211, p-value = 0.9855
# --> Nein

# --------------------------- Wilcoxon WA-SA ----------------------------
describe(data$KompetenzWA)
#    vars  n mean  sd median trimmed  mad  min  max range skew kurtosis   se
# X1    1 69 3.06 0.6   2.92    3.06 0.62 1.75 4.67  2.92 0.22    -0.43 0.07
describe(data$KompetenzSA)
#   vars  n mean   sd median trimmed  mad  min  max range  skew kurtosis   se
#X1    1 68 3.47 0.61    3.5     3.5 0.49 1.67 4.67     3 -0.57     0.48 0.07

# Ist die wahrgenommene Kompetenz des NA grösser als diejenige des WA?
wilcox.test(data$KompetenzWA, data$KompetenzSA, paired = TRUE, exact = FALSE, correct = TRUE, alternative = "greater")
# V = 318.5, p-value = 1
# --> Nein




# --------------------------- Multiple Regression KompetenzSA~KompetenzBeraterSA----------------------------
plot(data$KompetenzSA, data$KompetenzBeraterSA)

#Beraterkompetenz abhängig von HM Kompetenz?
testmodel <- lm(KompetenzSA~KompetenzBeraterSA, data = data)

# add line to plot which shows the estimated values
abline(testmodel, col="red")

summary(testmodel)
