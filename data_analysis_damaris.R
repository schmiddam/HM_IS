library(data.table)
library(psych)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(rstatix)

# ------------------Data Loading and Preparation ------------------------
#load data
data <- fread("survey_results_manualclean.csv")
#remove empty rows
data <- data[-(64),]

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





# ---------------------------  Pearson Korrelation KompetenzXA~KompetenzBeraterXA----------------------------
# Voraussetzungen erfüllt gemäss https://www.methodenberatung.uzh.ch/de/datenanalyse_spss/zusammenhaenge/korrelation.html#3.6._Eine_typische_Aussage

#detect outliers using a boxplot
ggplot(data) +
  aes(x = "", y = KompetenzBeraterWA) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

# Pearson Korrelation Test
cor.test(data$KompetenzBeraterWA, data$KompetenzWA)
# t = 4.2257, df = 75, p-value = 6.633e-05
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.2379086 0.6032485
# sample estimates:
#   cor 
# 0.4385202 

cor.test(data$KompetenzBeraterNA, data$KompetenzNA)
# t = 4.7501, df = 75, p-value = 9.558e-06
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   0.2879387 0.6363403
# sample estimates:
#   cor 
# 0.4809031 

cor.test(data$KompetenzBeraterSA, data$KompetenzSA)
# t = 3.6911, df = 75, p-value = 0.000421
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   0.1842852 0.5663259
# sample estimates:
#   cor 
# 0.3920805 




# ---------------------------  Pearson Korrelation KompetenzXA~KompetenzXA - Honesty----------------------------
# Voraussetzungen erfüllt gemäss https://www.methodenberatung.uzh.ch/de/datenanalyse_spss/zusammenhaenge/korrelation.html#3.6._Eine_typische_Aussage





# ---------------------------   Repeated One-way ANOVA HM ----------------------------
# data preparation
# extract the 3 competence measure-columns NA, WA, SA
data_extract <- data %>% select(id, KompetenzNA, KompetenzWA, KompetenzSA)
# rearrange values
competence <- data_extract %>%  
  gather(key = "anthro_version", value = "measure", KompetenzNA, KompetenzWA, KompetenzSA) %>% 
  convert_as_factor(id, anthro_version)

# compute some summary statistics
competence %>%
  group_by(anthro_version) %>%
  get_summary_stats(measure, type = "mean_sd")
# A tibble: 3 x 5
#anthro_version variable     n  mean    sd
# <fct>            <chr>    <dbl> <dbl> <dbl>
# 1 KompetenzNA    measure     77  3.20 0.477
# 2 KompetenzSA    measure     77  3.45 0.611
# 3 KompetenzWA    measure     77  3.04 0.578

# Visualize
bxp <- ggboxplot(competence, x = "anthro_version", y = "measure", add = "point")
bxp

#Check for extreme outliers
competence %>%
  group_by(anthro_version) %>%
  identify_outliers(measure)
# --> No, extreme outliers

# Check normality assumption
competence %>%
  group_by(anthro_version) %>%
  shapiro_test(measure)
# The competence scores are all normally distributed at each anthro_version, as assessed by Shapiro-Wilk’s test (p > 0.05).

# Additional normality assumptions check since Shapiro Wilks test can get sensitive to minor deviation from normality with sample size > 50
ggqqplot(competence, "measure", facet.by = "anthro_version")

# Computation of repeated ANOVA
res.aov <- anova_test(data = competence, dv = measure, wid = id, within = anthro_version)
get_anova_table(res.aov)
## ANOVA Table (type III tests)
## 
##   Effect         DFn DFd F      p        p<.05   ges
## 1 anthro_version   2 152 16.327 3.77e-07     * 0.084
## --> The competence score is statistically significantly different for the different anthro_versions NA, WA, and SA

# Posthoc test
# pairwise comparisons
pwc <- competence %>%
  pairwise_t_test(
    measure ~ anthro_version, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc
# A tibble: 3 x 10
#.y.         group1      group2         n1    n2 statistic    df           p     p.adj p.adj.signif
#*   <chr>   <chr>       <chr>       <int> <int>     <dbl> <dbl>       <dbl>     <dbl> <chr>       
#  1 measure KompetenzNA KompetenzSA    77    77     -3.23    76 0.002       0.005      **            
#  2 measure KompetenzNA KompetenzWA    77    77      2.45    76 0.016       0.049      *           
#  3 measure KompetenzSA KompetenzWA    77    77      5.58    76 0.000000357 0.00000107 ****       
# --> First and last of the pairwise differences are statistically significant, since all p-values<(0.05/3=0.017) 
# (Slide 7: https://www.ebpi.uzh.ch/dam/jcr:ffffffff-c1f2-5119-0000-000078458c66/slides-anova.pdf)

# Visualization: box plots with p-values
pwc <- pwc %>% add_xy_position(x = "anthro_version")
bxp + 
  stat_pvalue_manual(pwc) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )


# ---------------------------   Repeated One-way ANOVA Berater----------------------------
# data preparation
# extract the 3 competence measure-columns NA, WA, SA
data_extract_Berater <- data %>% select(id, KompetenzBeraterKL, KompetenzBeraterNA, KompetenzBeraterWA, KompetenzBeraterSA)

# rearrange values
competence_Berater <- data_extract_Berater %>%  
  gather(key = "anthro_version", value = "measure", KompetenzBeraterKL, KompetenzBeraterNA, KompetenzBeraterWA, KompetenzBeraterSA) %>% 
  convert_as_factor(id, anthro_version)

# replace all two NA in KL with mean of KL values 3.36 to enable Posthoc test
competence_Berater[is.na(competence_Berater)] <- 3.36


# compute some summary statistics
competence_Berater %>%
  group_by(anthro_version) %>%
  get_summary_stats(measure, type = "mean_sd")
# A tibble: 3 x 5
#   anthro_version     variable     n  mean    sd
#   <fct>              <chr>    <dbl> <dbl> <dbl>
# 1 KompetenzBeraterKL measure     75  3.36 0.669
# 2 KompetenzBeraterNA measure     77  3.45 0.522
# 3 KompetenzBeraterSA measure     77  3.42 0.557
# 4 KompetenzBeraterWA measure     77  3.44 0.564

# Visualize
bxp <- ggboxplot(competence_Berater, x = "anthro_version", y = "measure", add = "point")
bxp

#Check for extreme outliers
competence_Berater %>%
  group_by(anthro_version) %>%
  identify_outliers(measure)
# --> 3 outliers; No extreme outliers

# Check normality assumption
competence_Berater %>%
  group_by(anthro_version) %>%
  shapiro_test(measure)
# The competence measures are all normally distributed at each anthro_version, as assessed by Shapiro-Wilk’s test (p > 0.05).

# Additional normality assumptions check since Shapiro Wilks test can get sensitive to minor deviation from normality with sample size > 50
ggqqplot(competence_Berater, "measure", facet.by = "anthro_version")

# Computation of repeated ANOVA
res.aov <- anova_test(data = competence_Berater, dv = measure, wid = id, within = anthro_version)
get_anova_table(res.aov)
## ANOVA Table (type III tests)
## 
##        Effect  DFn    DFd     F     p p<.05   ges
## anthro_version 2.48 183.55 1.091   0.347       0.004
## --> The competence score of Berater is NOT statistically significantly different for the different anthro_versions KL, NA, WA, and SA


# Posthoc test
# pairwise comparisons
pwc <- competence_Berater %>%
  pairwise_t_test(
    measure ~ anthro_version, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc
# A tibble: 3 x 10
# .y.     group1             group2                n1    n2 statistic    df     p p.adj p.adj.signif
# * <chr>   <chr>              <chr>              <int> <int>     <dbl> <dbl> <dbl> <dbl> <chr>       
# 1 measure KompetenzBeraterKL KompetenzBeraterNA    77    77    -1.45     76 0.151 0.906 ns          
# 2 measure KompetenzBeraterKL KompetenzBeraterSA    77    77    -0.801    76 0.426 1     ns          
# 3 measure KompetenzBeraterKL KompetenzBeraterWA    77    77    -1.29     76 0.202 1     ns          
# 4 measure KompetenzBeraterNA KompetenzBeraterSA    77    77     0.713    76 0.478 1     ns          
# 5 measure KompetenzBeraterNA KompetenzBeraterWA    77    77     0.231    76 0.818 1     ns          
# 6 measure KompetenzBeraterSA KompetenzBeraterWA    77    77    -0.605    76 0.547 1     ns       
# --> None of the pairwise differences are statistically significant, since all p-values<(0.05/3=0.017) 
# (Slide 7: https://www.ebpi.uzh.ch/dam/jcr:ffffffff-c1f2-5119-0000-000078458c66/slides-anova.pdf)

# Visualization: box plots with p-values
pwc <- pwc %>% add_xy_position(x = "anthro_version")
bxp + 
  stat_pvalue_manual(pwc) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )
