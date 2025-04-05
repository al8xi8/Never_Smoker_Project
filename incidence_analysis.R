# ---- Load Data and Libraries ----
library(readr)
library(dplyr)
library(tidyr)
nhis <- read_csv("Desktop/nhis_00004.csv")  
US2000_SP84 <- read.table("~/Desktop/US2000_SP84.txt", header = FALSE, col.names = c("Population"))
US2000_SP84 <- data.frame(Age = 0:84, Population = US2000_SP84$Population)

# ---- Recode Race Variables ----
nhis$RACENEW_RECODED <- as.character(nhis$RACENEW)
nhis$RACENEW_RECODED[nhis$RACENEW %in% c(100)] <- "White"
nhis$RACENEW_RECODED[nhis$RACENEW %in% c(200)] <- "Black"
nhis$RACENEW_RECODED[nhis$RACENEW %in% c(300)] <- "AIAN"
nhis$RACENEW_RECODED[nhis$RACENEW %in% c(400)] <- "API"
nhis$RACENEW_RECODED[nhis$RACENEW %in% c(500, 510, 520, 540, 541, 542)] <- "Other"
nhis$RACENEW_RECODED[nhis$RACENEW %in% c(530, 997, 998, 999)] <- NA

nhis$RACEA_RECODED <- as.character(nhis$RACEA)
nhis$RACEA_RECODED[nhis$RACEA %in% c(100)] <- "White"
nhis$RACEA_RECODED[nhis$RACEA %in% c(200)] <- "Black"
nhis$RACEA_RECODED[nhis$RACEA %in% c(300:350)] <- "AIAN"
nhis$RACEA_RECODED[nhis$RACEA %in% c(400:434)] <- "API"
nhis$RACEA_RECODED[nhis$RACEA %in% c(500:617)] <- "Other"
nhis$RACEA_RECODED[nhis$RACEA %in% c(900:990)] <- NA

# Merge race variables
nhis$RACE_COMBINED <- ifelse(!is.na(nhis$RACENEW_RECODED), nhis$RACENEW_RECODED, nhis$RACEA_RECODED)

# ---- Recode Hispanic Ethnicity ----
nhis$HISPYN_RECODED <- ifelse(nhis$HISPYN %in% c(7, 8, 9), NA, nhis$HISPYN)
nhis$HISPYN_RECODED <- ifelse(nhis$HISPYN_RECODED == 2, "Hispanic", ifelse(nhis$HISPYN_RECODED == 1, "Non-Hispanic", NA))

# ---- Create Final Race/Ethnicity Variable ----
nhis$RACE_ETHNICITY <- NA
nhis$RACE_ETHNICITY[nhis$HISPYN_RECODED == "Hispanic"] <- "Hispanic"
nhis$RACE_ETHNICITY[nhis$HISPYN_RECODED == "Non-Hispanic" & nhis$RACE_COMBINED == "White"] <- "Non-Hispanic White"
nhis$RACE_ETHNICITY[nhis$HISPYN_RECODED == "Non-Hispanic" & nhis$RACE_COMBINED == "Black"] <- "Non-Hispanic Black"
nhis$RACE_ETHNICITY[nhis$HISPYN_RECODED == "Non-Hispanic" & nhis$RACE_COMBINED == "AIAN"] <- "Non-Hispanic AIAN"
nhis$RACE_ETHNICITY[nhis$HISPYN_RECODED == "Non-Hispanic" & nhis$RACE_COMBINED == "API"] <- "Non-Hispanic API"
nhis$RACE_ETHNICITY[nhis$HISPYN_RECODED == "Non-Hispanic" & nhis$RACE_COMBINED == "Other"] <- "Non-Hispanic Other"

# ---- Define Smoking Status ----
never <- nhis[nhis$SMOKESTATUS2 == 30, ]
smokers <- nhis[nhis$SMOKESTATUS2 %in% c(10, 11, 12, 13, 20, 40), ]

never_clean <- never[never$CNLUNG == 2 & never$CNLUNGAG < 96, ]
smokers_clean <- smokers[smokers$CNLUNG == 2 & smokers$CNLUNGAG < 96, ]



# =======================================
# EXCLUDE MISSING VALUES FOR AGE & CNLUNGAG
# =======================================
never_clean <- never_clean %>%
  filter(!AGE %in% c(997, 998, 999),  # Remove missing/unknown age at survey
         !CNLUNGAG %in% c(96, 97, 98, 99))  # Remove missing/unknown age at lung cancer diagnosis

smokers_clean <- smokers_clean %>%
  filter(!AGE %in% c(997, 998, 999),  # Remove missing/unknown age at survey
         !CNLUNGAG %in% c(96, 97, 98, 99))  # Remove missing/unknown age at lung cancer diagnosis



# ---- Prevalence ----
# Total prevalence (Weighted) for Never Smokers
never_prev <- (sum(never$SAMPWEIGHT[never$CNLUNG == 2], na.rm = TRUE) / 
                 sum(never$SAMPWEIGHT, na.rm = TRUE)) * 100
cat("Lung Cancer Prevalence (Never Smokers):", never_prev, "%\n")

# Total prevalence (Weighted) for Smokers
smokers_prev <- (sum(smokers$SAMPWEIGHT[smokers$CNLUNG == 2], na.rm = TRUE) / 
                   sum(smokers$SAMPWEIGHT, na.rm = TRUE)) * 100
cat("Lung Cancer Prevalence (Smokers):", smokers_prev, "%\n")

# ---- Prevalence by Race/Ethnicity ----
# Prevalence (Weighted) for Never Smokers by Race/Ethnicity
never_prev_race <- never %>%
  group_by(RACE_ETHNICITY) %>%
  summarise(
    weighted_cases = sum(SAMPWEIGHT * (CNLUNG == 2), na.rm = TRUE),  # Fixed calculation
    total_weighted = sum(SAMPWEIGHT, na.rm = TRUE),
    Prevalence = (weighted_cases / total_weighted) * 100
  ) %>%
  select(RACE_ETHNICITY, Prevalence)

print(never_prev_race)

# Barplot for Never Smokers
barplot(never_prev_race$Prevalence, names.arg = never_prev_race$RACE_ETHNICITY, 
        main = "Lung Cancer Prevalence by Race/Ethnicity (Never Smokers)", 
        col = "lightgreen", las = 2, cex.names = 0.8)

# Prevalence (Weighted) for Smokers by Race/Ethnicity
smokers_prev_race <- smokers %>%
  group_by(RACE_ETHNICITY) %>%
  summarise(
    weighted_cases = sum(SAMPWEIGHT * (CNLUNG == 2), na.rm = TRUE),  # Fixed calculation
    total_weighted = sum(SAMPWEIGHT, na.rm = TRUE),
    Prevalence = (weighted_cases / total_weighted) * 100
  ) %>%
  select(RACE_ETHNICITY, Prevalence)

print(smokers_prev_race)

# Barplot for Smokers
barplot(smokers_prev_race$Prevalence, names.arg = smokers_prev_race$RACE_ETHNICITY, 
        main = "Lung Cancer Prevalence by Race/Ethnicity (Smokers)", 
        col = "pink", las = 2, cex.names = 0.8)




# ---- Year of Diagnosis Calculation ----
never_clean <- never_clean %>%
  mutate(YEAR_DIAGNOSED = YEAR - AGE + CNLUNGAG)

smokers_clean <- smokers_clean %>%
  mutate(YEAR_DIAGNOSED = YEAR - AGE + CNLUNGAG)



# =======================================
# SMOKING-STRATIFIED AGE-STANDARDIZED INCIDENCE RATES
# =======================================

# =======================================
# STEP 1: CLEAN DATA (EXCLUDE MISSING VALUES)
# =======================================
nhis_clean <- nhis %>%
  filter(CNLUNG == 2,  # Only lung cancer cases
         !AGE %in% c(997, 998, 999),  # Remove missing/unknown age at survey
         !CNLUNGAG %in% c(96, 97, 98, 99),  # Remove missing/unknown age at lung cancer diagnosis
         SMOKESTATUS2 %in% c(10, 11, 12, 13, 20, 30, 40),  # Keep only defined smoking status
         !is.na(YEAR)) 

nhis_clean <- nhis_clean %>%
  mutate(YEAR_DIAGNOSED = YEAR - AGE + CNLUNGAG)

# =======================================
# STEP 2: CREATE COMPLETE AGE-YEAR GRID
# =======================================
age <- rep(18:85, (2023 - 1997 + 1))
year <- rep(1997:2023, each = (85 - 18 + 1))
a <- as.data.frame(cbind(age, year))
a$case <- 0  # Initialize cases as 0

# =======================================
# STEP 3: COMPUTE WEIGHTED LUNG CANCER CASES BY SMOKING STATUS & YEAR DIAGNOSED
# =======================================
never_c1 <- nhis_clean %>%
  filter(SMOKESTATUS2 == 30) %>%  # Never smokers
  group_by(CNLUNGAG, YEAR_DIAGNOSED) %>%
  summarise(SAMPWEIGHT_Cases = sum(SAMPWEIGHT, na.rm = TRUE), .groups = "drop") %>%
  filter(YEAR_DIAGNOSED >= 1997 & YEAR_DIAGNOSED <= 2023)

smokers_c1 <- nhis_clean %>%
  filter(SMOKESTATUS2 %in% c(10, 11, 12, 13, 20, 40)) %>%  # Ever smokers
  group_by(CNLUNGAG, YEAR_DIAGNOSED) %>%
  summarise(SAMPWEIGHT_Cases = sum(SAMPWEIGHT, na.rm = TRUE), .groups = "drop") %>%
  filter(YEAR_DIAGNOSED >= 1997 & YEAR_DIAGNOSED <= 2023)

# Merge with full age-year grid
never_c1_t <- left_join(a, never_c1, by = c("age" = "CNLUNGAG", "year" = "YEAR_DIAGNOSED")) %>%
  mutate(SAMPWEIGHT_Cases = replace_na(SAMPWEIGHT_Cases, 0))

smokers_c1_t <- left_join(a, smokers_c1, by = c("age" = "CNLUNGAG", "year" = "YEAR_DIAGNOSED")) %>%
  mutate(SAMPWEIGHT_Cases = replace_na(SAMPWEIGHT_Cases, 0))

# =======================================
# STEP 4: COMPUTE WEIGHTED POPULATION BY AGE & YEAR
# =======================================
never_p1 <- nhis %>%
  filter(SMOKESTATUS2 == 30) %>%
  group_by(AGE, YEAR) %>%
  summarise(SAMPWEIGHT_Pop = sum(SAMPWEIGHT, na.rm = TRUE), .groups = "drop") %>%
  filter(YEAR >= 1997)

smokers_p1 <- nhis %>%
  filter(SMOKESTATUS2 %in% c(10, 11, 12, 13, 20, 40)) %>%
  group_by(AGE, YEAR) %>%
  summarise(SAMPWEIGHT_Pop = sum(SAMPWEIGHT, na.rm = TRUE), .groups = "drop") %>%
  filter(YEAR >= 1997)

# =======================================
# STEP 5: COMPUTE AGE-SPECIFIC INCIDENCE RATES (ASIR)
# =======================================
never_asir <- left_join(never_c1_t, never_p1, by = c("age" = "AGE", "year" = "YEAR")) %>%
  mutate(SAMPWEIGHT_Pop = replace_na(SAMPWEIGHT_Pop, 1),
         Age_Specific_Rate = ifelse(SAMPWEIGHT_Pop > 0,
                                    (SAMPWEIGHT_Cases / SAMPWEIGHT_Pop) * 100000, 0))

smokers_asir <- left_join(smokers_c1_t, smokers_p1, by = c("age" = "AGE", "year" = "YEAR")) %>%
  mutate(SAMPWEIGHT_Pop = replace_na(SAMPWEIGHT_Pop, 1),
         Age_Specific_Rate = ifelse(SAMPWEIGHT_Pop > 0,
                                    (SAMPWEIGHT_Cases / SAMPWEIGHT_Pop) * 100000, 0))

# =======================================
# STEP 6: COMPUTE AGE-ADJUSTMENT WEIGHTS FROM US 2000 STANDARD POPULATION
# =======================================
US2000_SP84 <- US2000_SP84 %>%
  mutate(Adjustment_Weight = Population / sum(Population, na.rm = TRUE))

# =======================================
# STEP 7: COMPUTE AGE-ADJUSTED INCIDENCE RATES (AAIR)
# =======================================
never_asir <- left_join(never_asir, US2000_SP84, by = c("age" = "Age")) %>%
  mutate(Age_Specific_Rate = replace_na(Age_Specific_Rate, 0),
         Adjustment_Weight = replace_na(Adjustment_Weight, 0))

smokers_asir <- left_join(smokers_asir, US2000_SP84, by = c("age" = "Age")) %>%
  mutate(Age_Specific_Rate = replace_na(Age_Specific_Rate, 0),
         Adjustment_Weight = replace_na(Adjustment_Weight, 0))

# =======================================
# STEP 8: COMPUTE FINAL AGE-ADJUSTED INCIDENCE RATES BY YEAR DIAGNOSED
# =======================================
never_inc_adj <- never_asir %>%
  group_by(year) %>%
  summarise(Never_Adj_Rate = sum(Age_Specific_Rate * Adjustment_Weight, na.rm = TRUE), .groups = "drop")

smokers_inc_adj <- smokers_asir %>%
  group_by(year) %>%
  summarise(Smokers_Adj_Rate = sum(Age_Specific_Rate * Adjustment_Weight, na.rm = TRUE), .groups = "drop")

# =======================================
# STEP 9: PRINT FINAL STANDARDIZED INCIDENCE RATES
# =======================================
print(never_inc_adj)
print(smokers_inc_adj)

# =======================================
# STEP 10: PLOT AGE-STANDARDIZED INCIDENCE RATES BY YEAR DIAGNOSED
# =======================================
plot(never_inc_adj$year, never_inc_adj$Never_Adj_Rate, type = "o",
     col = "lightgreen", pch = 16, lty = 1,
     ylim = c(0, max(smokers_inc_adj$Smokers_Adj_Rate, na.rm = TRUE) * 1.1),
     xlab = "Year Diagnosed", ylab = "Age-Adjusted Incidence Rate per 100,000",
     main = "Age-Standardized Lung Cancer Incidence (Smoking Status)")

lines(smokers_inc_adj$year, smokers_inc_adj$Smokers_Adj_Rate, type = "o",
      col = "pink", pch = 17, lty = 2)

legend("topright", legend = c("Never Smokers", "Smokers"),
       col = c("lightgreen", "pink"), pch = c(16, 17), lty = c(1, 2))

# =======================================
# STEP 11: CHECK ASIR BEFORE & AFTER STANDARDIZATION
# =======================================
comparison_table <- never_asir %>%
  select(age, year, Age_Specific_Rate, Adjustment_Weight)

print(comparison_table)
summary(comparison_table)





--------------------------------------------

# =======================================
# SEX AGE-STANDARDIZED INCIDENCE RATES
# =======================================

# =======================================
# STEP 1: CLEAN DATA (EXCLUDE MISSING VALUES)
# =======================================
nhis_clean <- nhis %>%
  filter(CNLUNG == 2,  # Only lung cancer cases
         !AGE %in% c(997, 998, 999),  # Remove missing/unknown age at survey
         !CNLUNGAG %in% c(96, 97, 98, 99),  # Remove missing/unknown age at lung cancer diagnosis
         SEX %in% c(1, 2),  # Only Male (1) and Female (2)
         !is.na(YEAR)) 

# Compute Year of Diagnosis
nhis_clean <- nhis_clean %>%
  mutate(YEAR_DIAGNOSED = YEAR - AGE + CNLUNGAG)

# =======================================
# STEP 2: CREATE DATASET `a` (ALL AGE-YEAR COMBINATIONS)
# =======================================
age <- rep(18:85, times = (2023 - 1997 + 1))  # Repeat 18-85 for each year
year <- rep(1997:2023, each = (85 - 18 + 1))  # Each year repeats for all ages
a <- data.frame(age = age, year = year, case = 0)

# =======================================
# STEP 3: COMPUTE WEIGHTED LUNG CANCER CASES BY SEX & YEAR DIAGNOSED
# =======================================

## --- Males ---
male_c1 <- nhis_clean %>% filter(SEX == 1) %>%
  group_by(CNLUNGAG, YEAR_DIAGNOSED) %>%
  summarise(Male_Weight_Cases = sum(SAMPWEIGHT, na.rm = TRUE), .groups = "drop") %>%
  filter(YEAR_DIAGNOSED >= 1997 & YEAR_DIAGNOSED <= 2023)

## --- Females ---
female_c1 <- nhis_clean %>% filter(SEX == 2) %>%
  group_by(CNLUNGAG, YEAR_DIAGNOSED) %>%
  summarise(Female_Weight_Cases = sum(SAMPWEIGHT, na.rm = TRUE), .groups = "drop") %>%
  filter(YEAR_DIAGNOSED >= 1997 & YEAR_DIAGNOSED <= 2023)

# Merge `a` with `male_c1` & `female_c1` to ensure missing ages have `0` cases
male_c1_t <- left_join(a, male_c1, by = c("age" = "CNLUNGAG", "year" = "YEAR_DIAGNOSED")) %>%
  mutate(Male_Weight_Cases = replace_na(Male_Weight_Cases, 0))

female_c1_t <- left_join(a, female_c1, by = c("age" = "CNLUNGAG", "year" = "YEAR_DIAGNOSED")) %>%
  mutate(Female_Weight_Cases = replace_na(Female_Weight_Cases, 0))

# =======================================
# STEP 4: COMPUTE WEIGHTED TOTAL POPULATION BY SEX & YEAR
# =======================================

## --- Males ---
male_p1 <- nhis %>%
  filter(SEX == 1, !AGE %in% c(997, 998, 999)) %>%
  group_by(AGE, YEAR) %>%
  summarise(Male_Weight_Pop = sum(SAMPWEIGHT, na.rm = TRUE), .groups = "drop") %>%
  filter(YEAR >= 1997)

## --- Females ---
female_p1 <- nhis %>%
  filter(SEX == 2, !AGE %in% c(997, 998, 999)) %>%
  group_by(AGE, YEAR) %>%
  summarise(Female_Weight_Pop = sum(SAMPWEIGHT, na.rm = TRUE), .groups = "drop") %>%
  filter(YEAR >= 1997)

# =======================================
# STEP 5: COMPUTE AGE-SPECIFIC INCIDENCE RATES (ASIR)
# =======================================

## --- Males ---
male_asir <- left_join(male_c1_t, male_p1, 
                       by = c("age" = "AGE", "year" = "YEAR")) %>%
  mutate(Male_Weight_Pop = replace_na(Male_Weight_Pop, 1),  # Prevent division by zero
         Male_Age_Specific_Rate = ifelse(Male_Weight_Pop > 0, 
                                         (Male_Weight_Cases / Male_Weight_Pop) * 100000, 0))

## --- Females ---
female_asir <- left_join(female_c1_t, female_p1, 
                         by = c("age" = "AGE", "year" = "YEAR")) %>%
  mutate(Female_Weight_Pop = replace_na(Female_Weight_Pop, 1),  # Prevent division by zero
         Female_Age_Specific_Rate = ifelse(Female_Weight_Pop > 0, 
                                           (Female_Weight_Cases / Female_Weight_Pop) * 100000, 0))

# =======================================
# STEP 6: COMPUTE AGE-ADJUSTED INCIDENCE RATES (AAIR) BY SEX
# =======================================

## --- Load US 2000 Standard Population ---
US2000_SP84 <- US2000_SP84 %>%
  mutate(Adjustment_Weight = Population / sum(Population, na.rm = TRUE))

## --- Males ---
male_asir <- left_join(male_asir, US2000_SP84, by = c("age" = "Age")) %>%
  mutate(Male_Age_Specific_Rate = replace_na(Male_Age_Specific_Rate, 0),
         Adjustment_Weight = replace_na(Adjustment_Weight, 0))

male_inc_adj <- male_asir %>%
  group_by(year) %>%
  summarise(Male_Age_Adjusted_Rate = sum(Male_Age_Specific_Rate * Adjustment_Weight, na.rm = TRUE), .groups = "drop")

## --- Females ---
female_asir <- left_join(female_asir, US2000_SP84, by = c("age" = "Age")) %>%
  mutate(Female_Age_Specific_Rate = replace_na(Female_Age_Specific_Rate, 0),
         Adjustment_Weight = replace_na(Adjustment_Weight, 0))

female_inc_adj <- female_asir %>%
  group_by(year) %>%
  summarise(Female_Age_Adjusted_Rate = sum(Female_Age_Specific_Rate * Adjustment_Weight, na.rm = TRUE), .groups = "drop")

# =======================================
# STEP 7: PLOT AGE-STANDARDIZED INCIDENCE RATES BY SEX
# =======================================
plot(male_inc_adj$year, male_inc_adj$Male_Age_Adjusted_Rate, 
     type = "o", col = "blue", pch = 16, lty = 1, 
     ylim = range(0, max(c(male_inc_adj$Male_Age_Adjusted_Rate, female_inc_adj$Female_Age_Adjusted_Rate), na.rm = TRUE) * 1.1),
     xlab = "Year Diagnosed", ylab = "Age-Adjusted Incidence Rate per 100,000", 
     main = "Age-Standardized Lung Cancer Incidence by Sex")

lines(female_inc_adj$year, female_inc_adj$Female_Age_Adjusted_Rate, 
      type = "o", col = "red", pch = 17, lty = 2)

legend("topright", legend = c("Male", "Female"), col = c("blue", "red"), pch = c(16, 17), lty = c(1, 2))




