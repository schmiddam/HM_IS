#Github token instead password:  4f957a59042f0e723c5bd81e694a57b12b3cd291

library(data.table)
library(psych)
library(purrr)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(dplyr)
library(carData) # MANOVA
library(car) # MANOVA
library(broom) # MANOVA
library(lavaan) # SEM

# ------------------Data Loading and Preparation ------------------------
#load data
data <- fread("survey_results_manualclean.csv")
#remove empty rows
data <- data[-(64),]

# change "weiblich"/"männlich" zu 1/0
data$Dem02 <- str_replace_all(data$Dem02, "weiblich", "1")
data$Dem02 <- str_replace_all(data$Dem02, "mÃ¤nnlich", "0")
data$Dem02 <- sapply(data[, Dem02], as.integer)

# change "Ja"/"Nein" zu 1/0
data$ErfahrungBank <- str_replace_all(data$ErfahrungBank, "Ja", "1")
data$ErfahrungBank <- str_replace_all(data$ErfahrungBank, "Nein", "0")
data$ErfahrungBank <- sapply(data[, ErfahrungBank], as.integer)

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





# ---------------------------  Pearson Korrelation KompetenzXA~KompetenzBeraterXA----------------------------
# Voraussetzungen erfüllt gemäss https://www.methodenberatung.uzh.ch/de/datenanalyse_spss/zusammenhaenge/korrelation.html#3.6._Eine_typische_Aussage

#detect outliers using a boxplot
ggplot(competenceAll) +
  aes(x = "", y = Agent) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

# Pearson Korrelation Test
cor.test(competenceAll$Berater, competenceAll$Agent)
# t = 6.7415, df = 229, p-value = 1.26e-10
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   0.2932643 0.5092655
# sample estimates:
#   cor 
# 0.4069378 

cor.test(data$KompetenzBeraterNA, data$KompetenzNA)
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


# --------------------------- Repeated One-way ANOVA Berater----------------------------
# data preparation
# extract the 3 competence measure-columns NA, WA, SA
data_extract_Berater <- data %>% select(id, KompetenzBeraterKL, KompetenzBeraterNA, KompetenzBeraterWA, KompetenzBeraterSA)

# rearrange values
competence_Berater <- data_extract_Berater %>%  
  gather(key = "anthro_version", value = "measure", KompetenzBeraterKL, KompetenzBeraterNA, KompetenzBeraterWA, KompetenzBeraterSA) %>% 
  convert_as_factor(id, anthro_version)

# replace two NA in KL with mean of KL values 3.36 to enable Posthoc test
competence_Berater[is.na(competence_Berater)] <- 3.36


# compute some summary statistics
competence_Berater %>%
  group_by(anthro_version) %>%
  get_summary_stats(measure, type = "mean_sd")
# A tibble: 3 x 5
#   anthro_version     variable     n  mean    sd
#   <fct>              <chr>    <dbl> <dbl> <dbl>
# 1 KompetenzBeraterKL measure     77  3.36 0.66 
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

# --------------------------- Two-way ANOVA ----------------------------
# independent variables: human (0,1) & version (NA, WA, SA)
# dependent variable: Kompetenz

# data preparation; select columns
data_extract_MANOVA <- data %>% 
  select(id, KompetenzNA, KompetenzBeraterNA, KompetenzWA, KompetenzBeraterWA, KompetenzSA, KompetenzBeraterSA)

dNA <- data %>%
  select(id, KompetenzNA, KompetenzBeraterNA) %>%
  rename(Agent = KompetenzNA, Berater = KompetenzBeraterNA) %>%
  add_column(version="NA")

dWA <- data %>%
  select(id, KompetenzWA, KompetenzBeraterWA) %>%
  rename(Agent = KompetenzWA, Berater = KompetenzBeraterWA) %>%
  add_column(version="WA")
dSA <- data %>%
  select(id, KompetenzSA, KompetenzBeraterSA) %>%
  rename(Agent = KompetenzSA, Berater = KompetenzBeraterSA) %>%
  add_column(version="SA")

competenceAll <- rbind(dNA, dWA, dSA)


# split Berater&Agent in einzige Spalte Kompetenz, dafür neue value human(0,1)
new_table_agent <- competenceAll %>%
  # drop Berater
  select(id, Agent, version) %>%
  # new column with binary independent humanism value (0 or 1)
  add_column(human=0) %>%
  # Agent is renamed to generic competence
  rename(Kompetenz = Agent) 

new_table_berater <- competenceAll %>%
  # drop Agent
  select(id, Berater, version) %>%
  # new column with binary independent humanism value (0 or 1)
  add_column(human=1) %>%
  # Agent is renamed to generic competence
  rename(Kompetenz = Berater) 

dt <- rbind(new_table_agent, new_table_berater)

# change "NA"/"WA"/"SA" zu 1/2/3
dt$version <- str_replace_all(dt$version, "NA", "1")
dt$version <- str_replace_all(dt$version, "WA", "2")
dt$version <- str_replace_all(dt$version, "SA", "3")
dt$version <- sapply(dt[, version], as.integer)

# remove unnecessary tables from environment
rm(dNA, dSA, dWA, new_table, new_table_agent, new_table_berater)

# now we are ready for ANOVA \o.O/


