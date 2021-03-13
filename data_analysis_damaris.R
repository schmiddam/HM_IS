#Github token instead password:  f89287d9f124ebba9f9118f2762c8d7f578ed12b

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
library(emmeans) # two-way ANOVA
library(corrplot) # visualize Pearson correlation

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


# --------------------------- Welch Two Sample t-Test Anthropomophizität ----------------------------

# Combine multiple Anthro-values (Anthro1NA,Anthro3NA, Anthro5NA) by mean
anthro_table <- data %>% 
  select(id, Anthro1NA, Anthro3NA, Anthro5NA, 
         Anthro1WA, Anthro3WA, Anthro5WA, 
         Anthro1SA, Anthro3SA, Anthro5SA) %>%
  # AnthroXA is the mean of 
  mutate(AnthroNA = (Anthro1NA+Anthro3NA+Anthro5NA)/3) %>%
  mutate(AnthroWA = (Anthro1WA+Anthro3WA+Anthro5WA)/3) %>%
  mutate(AnthroSA = (Anthro1SA+Anthro3SA+Anthro5SA)/3) 
anthro_table <- anthro_table %>%
  select(id, AnthroNA, AnthroWA, AnthroSA)

# rearrange values
anthro_table_rearranged <- anthro_table %>%  
  gather(key = "version", value = "measure", AnthroNA, AnthroWA, AnthroSA) %>% 
  convert_as_factor(id, version)

anthro_table_rearranged$version <- str_replace_all(anthro_table_rearranged$version, "AnthroNA", "1")
anthro_table_rearranged$version <- str_replace_all(anthro_table_rearranged$version, "AnthroWA", "2")
anthro_table_rearranged$version <- str_replace_all(anthro_table_rearranged$version, "AnthroSA", "3")

# Shapiro-Wilk normality test: keine Normalverteilung -> kein t-Test möglich

describeBy(anthro_table_rearranged$measure, anthro_table_rearranged$version)
group: 1
# vars  n mean   sd median trimmed  mad  min  max range skew kurtosis   se
# X1    1 77 2.69 0.56   2.62    2.65 0.51 1.58 4.47  2.89 0.69     0.44 0.06
# -------------------------------------------------------------------------- 
#   group: 2
# vars  n mean   sd median trimmed  mad  min  max range  skew kurtosis   se
# X1    1 77 2.89 0.62   2.85     2.9 0.65 1.47 4.22  2.75 -0.05    -0.58 0.07
# -------------------------------------------------------------------------- 
#   group: 3
# vars  n mean   sd median trimmed  mad  min  max range  skew kurtosis   se
# X1    1 77 3.48 0.64   3.58    3.53 0.55 1.67 4.59  2.92 -0.73     0.32 0.07

# Kruskal-Wallis-Test: Signifikante Unterschiede zwischen Anthro-Varianten
kruskal.test(anthro_table_rearranged$measure~anthro_table_rearranged$version)
boxplot(measure~version, data = anthro_table_rearranged)

# Wilcoxon Test bestätigt Signifikanz auch zwischen einzelnen Anthrovarianten
pow <- anthro_table_rearranged %>% slice(1:154)
pow1 <- anthro_table_rearranged %>% slice(78:231)
pow2 <- anthro_table_rearranged %>% slice(1:77)
pow3 <- anthro_table_rearranged %>% slice(155:231)
pow4 <- rbind(pow2, pow3)
wilcox.test(measure~version, data = pow, exact=FALSE)


# ---------------------------  Pearson Korrelation KompetenzBeraterXA~KompetenzXA----------------------------
# Voraussetzungen erfüllt gemäss https://www.methodenberatung.uzh.ch/de/datenanalyse_spss/zusammenhaenge/korrelation.html#3.6._Eine_typische_Aussage
# Welchen Einfluss hat wahrgenommene Kompetenz des Beraters auf die wahrgenommene Kompetenz des HM


cor.test(data$KompetenzBeraterNA, data$KompetenzNA)
# t = 4.2257, df = 75, p-value = 6.633e-05
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.2379086 0.6032485
# sample estimates:
#   cor 
# 0.4385202 
# --> mittlerer bis starker positiver Effekt: Immer wenn Kompetenz des Beraters steigt, steigt Kompetenz von HM


cor.test(data$KompetenzBeraterWA, data$KompetenzWA)
# t = 4.2257, df = 75, p-value = 6.633e-05
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   0.2379086 0.6032485
# sample estimates:
#   cor 
# 0.4385202 
# --> mittlerer bis starker positiver Effekt: Immer Kompetenz des Beraters steigt, steigt Kompetenz von HM


cor.test(data$KompetenzBeraterSA, data$KompetenzSA)
# t = 3.6911, df = 75, p-value = 0.000421
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   0.1842852 0.5663259
# sample estimates:
#   cor 
# 0.3920805 
# --> mittlerer bis starker positiver Effekt: Immer Kompetenz des Beraters steigt, steigt Kompetenz von HM



# ---------------------------  Pearson Korrelation KompetenzXA~KompetenzBeraterXA----------------------------
# Voraussetzungen erfüllt gemäss https://www.methodenberatung.uzh.ch/de/datenanalyse_spss/zusammenhaenge/korrelation.html#3.6._Eine_typische_Aussage
# Welchen Einfluss hat wahrgenommene Kompetenz des HM auf die wahrgenommene Kompetenz des Beraters

cor.test(data$KompetenzNA, data$KompetenzBeraterNA)
# t = 4.7501, df = 75, p-value = 9.558e-06
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.2879387 0.6363403
# sample estimates:
#   cor 
# 0.4809031 
# --> mittlerer bis starker positiver Effekt: Immer Kompetenz des HM steigt, steigt Kompetenz von Beraters


cor.test(data$KompetenzWA, data$KompetenzBeraterWA)
# t = 4.2257, df = 75, p-value = 6.633e-05
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   0.2379086 0.6032485
# sample estimates:
#   cor 
# 0.4385202 
# --> mittlerer bis starker positiver Effekt: Immer Kompetenz des HM steigt, steigt Kompetenz von Beraters


cor(data$KompetenzSA, data$KompetenzBeraterSA)
# t = 3.6911, df = 75, p-value = 0.000421
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   0.1842852 0.5663259
# sample estimates:
#   cor 
# 0.3920805 
# --> mittlerer bis starker positiver Effekt: Immer Kompetenz des HM steigt, steigt Kompetenz von Beraters




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


# split "Berater" & "Agent" in einzige Spalte "Kompetenz", dafür neue value human(0,1)
new_table_agent <- competenceAll %>%
  # drop Berater
  select(id, Agent, version) %>%
  # new column with binary independent humanism value (0 or 1)
  add_column(human="no") %>%
  # Agent is renamed to generic competence
  rename(Kompetenz = Agent) 

new_table_berater <- competenceAll %>%
  # drop Agent
  select(id, Berater, version) %>%
  # new column with binary independent humanism value (0 or 1)
  add_column(human="yes") %>%
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

bxp <- ggboxplot(
  dt, x = "human", y = "Kompetenz",
  color = "version", palette =c("#00AFBB", "#E7B800", "#FC4E07")
)
bxp

dt %>%
  group_by(human, version) %>%
  identify_outliers(Kompetenz)
# some Outliers, but no extreme

# Build the linear model
model  <- lm(Kompetenz ~ human*version,
             data = dt)
# Create a QQ plot of residuals
ggqqplot(residuals(model))

# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))
# we can assume normality since p > 0.05

dt %>%
  group_by(human, version) %>%
  shapiro_test(Kompetenz)
# Kompetenz is normally distributed

ggqqplot(dt, "Kompetenz", ggtheme = theme_bw()) +
  facet_grid(human ~ version)

dt %>% levene_test(Kompetenz ~ human*version)
#    df1   df2 statistic     p
#   <int> <int>     <dbl> <dbl>
#     1     5   456 0.926 0.464
# --> we can assume the homogeneity of variances in the different groups


res.aov <- dt %>% anova_test(Kompetenz ~ human * version)
res.aov
# ANOVA Table (type II tests)

#          Effect DFn DFd      F       p p<.05   ges
# 1         human   1 456 15.612 9.01e-05     * 0.033
# 2       version   2 456  4.654 1.00e-02     * 0.020
# 3 human:version   2 456  6.087 2.00e-03     * 0.026
# -->  statistically significant interaction between human and anthropomorphic version 

# Posthoc test

# Procedure for significant two-way interaction
# Group the data by humanness and fit anova
model <- lm(Kompetenz ~ human * version, data = dt)
dt %>%
  group_by(human) %>%
  anova_test(Kompetenz ~ version, error = model)
#    human Effect    DFn   DFd     F         p `p<.05`      ges
# * <chr> <chr>   <dbl> <dbl> <dbl>     <dbl> <chr>      <dbl>
# 1 no    version     2   456 10.7  0.0000299 "*"     0.045   
# 2 yes   version     2   456  0.08 0.923     ""      0.000352
# The main effect of version on the Kompetenz is statistically significant only for the digital agent
# In other words, there is a statistically significant difference in mean Kompetenz 
# between either NA, WA, SA agents , F(1, 458) = 7.40, p < 0.05

model <- lm(Kompetenz ~ human * version, data = dt)
dt %>%
  group_by(version) %>%
  anova_test(Kompetenz ~ human, error = model)
# version Effect   DFn   DFd      F          p `p<.05`      ges
# * <chr>   <chr>  <dbl> <dbl>  <dbl>      <dbl> <chr>      <dbl>
# 1 NA      human      1   456  7.59  0.006      "*"     0.016   
# 2 SA      human      1   456  0.151 0.698      ""      0.000331
# 3 WA      human      1   456 20.0   0.00000955 "*"     0.042 
# The main effect of humanness on the Kompetenz is statistically significant only for NA and WA

# Compare the Kompetenz of the different versions by humanness:
pwc <- dt %>% 
  group_by(human) %>%
  emmeans_test(Kompetenz ~ version, p.adjust.method = "bonferroni") 
pwc
#   human term    .y.       group1 group2    df statistic          p     p.adj p.adj.signif
# * <chr> <chr>   <chr>     <chr>  <chr>  <dbl>     <dbl>      <dbl>     <dbl> <chr>       
# 1 no    version Kompetenz NA     SA       456    -2.75  0.00611    0.0183    *           
# 2 no    version Kompetenz NA     WA       456     1.83  0.0676     0.203     ns          
# 3 no    version Kompetenz SA     WA       456     4.59  0.00000582 0.0000175 ****        
# 4 yes   version Kompetenz NA     SA       456     0.388 0.698      1         ns          
# 5 yes   version Kompetenz NA     WA       456     0.109 0.913      1         ns          
# 6 yes   version Kompetenz SA     WA       456    -0.279 0.780      1         ns 
# --> There is a significant difference of Kompetenz only between NA-SA and SA-WA for the agent


# Report
# Visualization: box plots with p-values
pwc <- pwc %>% add_xy_position(x = "human")
bxp +
  stat_pvalue_manual(pwc) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )
