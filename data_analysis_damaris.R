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
