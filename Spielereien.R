# Datenspielereien
#extract column name
abb <- sample(colnames(data_extract_MANOVA), 3)

#extract the last two characters of string, e.g. 'NA' or 'WA'
abb <- substr(abb, nchar(abb)-1, nchar(abb))

# add column which states anthro version
add_column(data_extract_MANOVA, version = abb)

# replace NANs with NA
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

data[is.nan(data)] <- NA


# -------------NA------------------------------------
#Prüfung ob Kompetenzvariabeln NA normalverteilt

# Shapiro Wilk normality test
shapiro.test(data$KompetenzNA)
# W = 0.97636, p-value = 0.1612
# da p > 0.05 sind Daten normalverteilt
hist(data$KompetenzNA)
# Histogram ist etwa normalverteilt - Bestätigt Shapiro-Wilk Test

# -------------WA------------------------------------
#Prüfung ob Kompetenzvariabeln WA normalverteilt
# Shapiro Wilk normality test
shapiro.test(data$KompetenzWA)
# W = 0.98437, p-value = 0.4662
# da p > 0.05 sind Daten normalverteilt
hist(data$KompetenzWA)
# Histogram ist etwa normalverteilt - Bestätigt Shapiro-Wilk Test

# -------------SA------------------------------------
#Prüfung ob Kompetenzvariabeln SA normalverteilt
# Shapiro Wilk normality test
shapiro.test(data$KompetenzSA)
# W = 0.97451, p-value = 0.1241
# da p > 0.05 sind Daten normalverteilt
hist(data$KompetenzSA)
# Histogram ist etwa normalverteilt - Bestätigt Shapiro-Wilk Test

# --------------------------- Welch Two Sample t-Test NA-SA ----------------------------
# Vorbedingung: Ist Varianz etwa gleich?
describeBy(data$KompetenzSA, na.rm = TRUE)
describeBy(data$KompetenzNA, na.rm = TRUE)
# KompetenzSA sd = 0.61; KompetenzNA sd = 0.48

# Nullhypothese: No difference between averages of the two groups

# ---Alternativhypothese: SA ist weniger kompetent als NA
t.test(data$KompetenzNA, data$KompetenzSA, alternative = "less", na.rm = TRUE)
#t = -2.7794, df = 143.55, p-value = 0.003088
#alternative hypothesis: true difference in means is less than 0


# ---Alternativhypothese: SA ist kompetenter als NA
t.test(data$KompetenzNA, data$KompetenzSA, alternative = "greater", na.rm = TRUE)
#t = -2.7794, df = 143.55, p-value = 0.9969
#alternative hypothesis: true difference in means is greater than 0
# ---> there is not enough evidence of a difference between the (true) averages of the two groups

# ---Two-sided
t.test(data$KompetenzNA, data$KompetenzSA, alternative = "two.sided", na.rm = TRUE)
# t = -2.7794, df = 143.55, p-value = 0.006175
# alternative hypothesis: true difference in means is not equal to 0


# --------------------------- One-way MANOVA ----------------------------

# Visualization
ggboxplot(
  competenceAll, x = "version", y = c("Agent", "Berater") , 
  merge = TRUE, palette = "jco"
)


# Compute summary statistics
competenceAll %>%
  group_by(version) %>%
  get_summary_stats(Berater, Agent, type = "mean_sd")
# version variable     n  mean    sd
# 1 NA      Agent       77  3.20 0.477
# 2 NA      Berater     77  3.45 0.522
# 3 SA      Agent       77  3.45 0.611
# 4 SA      Berater     77  3.42 0.557
# 5 WA      Agent       77  3.04 0.578
# 6 WA      Berater     77  3.44 0.564

# Check outliers
competenceAll %>%
  group_by(version) %>%
  identify_outliers(Berater)
# --> 2 outliers in WA; none is extreme

competenceAll %>%
  group_by(version) %>%
  identify_outliers(Agent)
# --> 7 outliers in NA, SA, WA; none is extreme


# Compute distance by groups and filter outliers
# Use -id to omit the id column in the computation
competenceAll %>%
  group_by(version) %>%
  mahalanobis_distance(-id) %>%
  filter(is.outlier == TRUE) %>%
  as.data.frame()


# Check univariate normality assumption
competenceAll %>%
  group_by(version) %>%
  shapiro_test(Agent, Berater) %>%
  arrange(variable)
# version variable statistic     p
# <chr>   <chr>        <dbl> <dbl>
# 1 NA      Agent        0.976 0.161
# 2 SA      Agent        0.975 0.124
# 3 WA      Agent        0.984 0.466
# 4 NA      Berater      0.985 0.498
# 5 SA      Berater      0.991 0.870
# 6 WA      Berater      0.974 0.123
# --> all are normally distributed


# Plot normal distribution
ggqqplot(competenceAll, "Agent", facet.by = "version",
         ylab = "Competence Measure", ggtheme = theme_bw())
ggqqplot(competenceAll, "Berater", facet.by = "version",
         ylab = "Competence Measure", ggtheme = theme_bw())

# Multivariate normality
competenceAll %>%
  select(Agent, Berater) %>%
  mshapiro_test()
#     statistic p.value
#  1     0.977 0.000791
# --> We cannot assume multivariate normality.

# Identify multicolinearity
competenceAll %>% cor_test(Agent, Berater)
# no multicolinearity, as assessed by Pearson correlation (p < 0.001)

# Check the homogeneity of covariances assumption
box_m(competence_all["measure"], competence_all$anthro_version)
# p.value 0.360   --> :(
#Note that, if you have balanced design (i.e., groups with similar sizes), you 
#don’t need to worry too much about violation of the homogeneity of variances-covariance 
#matrices and you can continue your analysis.
#However, having an unbalanced design is problematic. Possible solutions include: 
#1) transforming the dependent variables; 
#2) running the test anyway, but using Pillai’s multivariate statistic instead of Wilks’ statistic.


#Check the homogneity of variance assumption
competence_all %>% 
  levene_test(measure ~ anthro_version)
# p.value 0.464 --> :(
# Note that, if you do not have homogeneity of variances, you can try to transform 
# the outcome (dependent) variable to correct for the unequal variances.
# Alternatively, you can continue, but accept a lower level of statistical 
#significance (alpha level) for your MANOVA result. Additionally, any follow-up 
# univariate ANOVAs will need to be corrected for this violation 
#(i.e., you will need to use different post-hoc tests).

# Computation MANOVA
model <- lm(measure ~ anthro_version, competence_all)
Manova(cbind(KompetenzNA, KompetenzBeraterNA)~id)

# --------------------------- Wilcoxon NA-SA ----------------------------
describe(data$KompetenzNA)
#    vars  n mean   sd median trimmed  mad  min  max range  skew kurtosis   se
# X1    1 77  3.2 0.48   3.25    3.23 0.37 1.92 4.25  2.33 -0.45     0.16 0.05
describe(data$KompetenzSA)
#   vars  n mean   sd median trimmed  mad  min  max range  skew kurtosis   se
#X1    1 77 3.45 0.61   3.42    3.47 0.62 1.67 4.67     3 -0.41     0.24 0.07

# Ist die wahrgenommene Kompetenz des NA geringer als diejenige des SA?
wilcox.test(data$KompetenzNA, data$KompetenzSA, paired = TRUE, exact = FALSE, correct = TRUE, alternative = "less")
# V = 763, p-value = 0.001588
# alternative hypothesis: true location shift is less than 0
# --> Ja

# --------------------------- Wilcoxon NA-WA ----------------------------
describe(data$KompetenzNA)
#    vars  n mean   sd median trimmed  mad  min  max range  skew kurtosis   se
# X1    1 77  3.2 0.48   3.25    3.23 0.37 1.92 4.25  2.33 -0.45     0.16 0.05
describe(data$KompetenzWA)
#    vars  n mean  sd median trimmed  mad  min  max range skew kurtosis   se
# X1    1 77 3.04 0.58   2.92    3.03 0.49 1.75 4.67  2.92 0.26    -0.23 0.07

# Ist die wahrgenommene Kompetenz des NA geringer als diejenige des WA?
wilcox.test(data$KompetenzNA, data$KompetenzWA, paired = TRUE, exact = FALSE, correct = TRUE, alternative = "less")
# V = 1714.5, p-value = 0.9939
# --> Nein

# --------------------------- Wilcoxon WA-SA ----------------------------
describe(data$KompetenzWA)
#    vars  n mean  sd median trimmed  mad  min  max range skew kurtosis   se
# X1    1 77 3.04 0.58   2.92    3.03 0.49 1.75 4.67  2.92 0.26    -0.23 0.07
describe(data$KompetenzSA)
#   vars  n mean   sd median trimmed  mad  min  max range  skew kurtosis   se
#X1    1 77 3.45 0.61   3.42    3.47 0.62 1.67 4.67     3 -0.41     0.24 0.07

# Ist die wahrgenommene Kompetenz des NA grösser als diejenige des WA?
wilcox.test(data$KompetenzWA, data$KompetenzSA, paired = TRUE, exact = FALSE, correct = TRUE, alternative = "greater")
# V  = 496, p-value = 1
# --> Nein




# --------------------------- Multiple Regression KompetenzSA~KompetenzBeraterSA----------------------------
plot(data$KompetenzSA, data$KompetenzBeraterSA)

#Beraterkompetenz abhängig von HM Kompetenz?
testmodel <- lm(KompetenzSA~KompetenzBeraterSA, data = data)

# add line to plot which shows the estimated values
abline(testmodel, col="red")

summary(testmodel)


# --------------------------- Multiple Regression KompetenzWA~KompetenzBeraterWA----------------------------
plot(data$KompetenzWA, data$KompetenzBeraterWA, col = data$KompetenzWA)

#Beraterkompetenz abhängig von HM Kompetenz?
testmodel <- lm(KompetenzWA~KompetenzBeraterWA, data = data)
# da p-value: 6.633e-05, leistet das Modell einen Erklärungsbeitrag zur Fragestellung
# --> Multiple R-squared:  0.1923 -> ich kann ledglich 17.42% der Varianz von KompetenzWA erklären (sehr wenig)
# --> Ändert sich die KompetenzBeraterWA um 1 Einheit, steigt die KompetenzWA um 0.4371

# add line to plot which shows the estimated values
abline(testmodel, col="blue")

summary(testmodel)




# --------------------------- Multiple Regression KompetenzAgent~KompetenzBerater----------------------------
plot(competenceAll$Agent, competenceAll$Berater, col = competenceAll$Agent)

#Beraterkompetenz abhängig von HM Kompetenz?
testmodel <- lm(Agent~Berater, data = competenceAll)
# da p-value: 6.633e-05, leistet das Modell einen Erklärungsbeitrag zur Fragestellung
# --> Multiple R-squared:  0.1656 -> ich kann ledglich 16.56% der Varianz von Agent erklären (sehr wenig)
# --> Ändert sich die Kompetenz des Berater um 1 Einheit, steigt die Kompetenz des Agenten um 0.43

# add line to plot which shows the estimated values
abline(testmodel, col="blue")

summary(testmodel)


