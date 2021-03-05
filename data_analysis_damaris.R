library(data.table)

# ------------------Data Loading and Preparation ------------------------
#load data
data <- fread("survey_results_manualclean_nocheaters.csv")
#remove empty rows
data <- data[-(74:1030),]


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
diff <- data$KompetenzWA

# Shapiro Wilk normality test
shapiro.test(diff)
# W = 0.98547, p-value = 0.6054
# da p > 0.05 sind Daten normalverteilt
hist(diff)
# Histogram ist etwa normalverteilt - Bestätigt Shapiro-Wilk Test

# -------------SA------------------------------------
#Prüfung ob Kompetenzvariabeln SA normalverteilt
diff <- data$KompetenzSA

# Shapiro Wilk normality test
shapiro.test(diff)
# W = 0.96794, p-value = 0.07708
# da p > 0.05 sind Daten normalverteilt
hist(diff)
# Histogram ist etwa normalverteilt - Bestätigt Shapiro-Wilk Test




# --------------------------- Welch Two Sample t-Test NA-SA ----------------------------
library(psych)
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

wilcox.test(data$KompetenzNA, data$KompetenzSA)
# W = 1662, p-value = 0.004637
# alternative hypothesis: true location shift is not equal to 0

# Ist die wahrgenommene Kompetenz des NA geringer als diejenige des SA?
wilcox.test(data$KompetenzNA, data$KompetenzSA, paired = TRUE, exact = FALSE, correct = TRUE, alternative = "less")
# V = 540, p-value = 0.001804
# alternative hypothesis: true location shift is less than 0
# --> Ja
