setwd('/Users/jasminelee/Desktop/MSDS/2026 Spring/PH_718')
ehr <- read.csv('sim_ehr_500.csv', stringsAsFactors = FALSE)
dict <- readxl::read_excel('sim_ehr_500_dictionary.xlsx')
str(ehr)
summary(ehr)
View(ehr)

unique(ehr$race_ethnicity)
ehr$race_ethnicity_f <- factor(ehr$race_ethnicity)
levels(ehr$race_ethnicity_f)
ehr$race_ethnicity_f <- factor(
  ehr$race_ethnicity,
  # levels = rev(unique(ehr$race_ethnicity)),
  levels = c('White', 'Black', 'Hispanic/Latino', 'Asian', 'Other/Multiracial', ''), # 'levels' re-arranges the levels
  # labels = rev(unique(ehr$race_ethnicity))
  labels = c('White', 'Black', 'H/L', 'Asian', 'Other/Multiracial', 'Unknown') #'labels' re-names the levels
)
levels(ehr$race_ethnicity_f)
View(ehr)

unique(ehr$insurance_type)
ehr$insurance_type_f <- factor(
  ehr$insurance_type,
  levels = c('Uninsured', 'Commercial', 'Medicare', 'Medicaid', 'Uninsured', ''),
  labels = c('Uninsured', 'Commercial', 'Medicare', 'Medicaid', 'Uninsured', 'Unknown')
)
levels(ehr$insurance_type_f)
View(ehr)

unique(ehr$sex)
ehr$sex_f <- factor(
  ehr$sex,
  levels = c('Male', 'Female'),
  labels = c('M', 'F')
)
levels(ehr$sex_f)
View(ehr)

table(is.na(ehr$bmi), ehr$sud_any) # 0 - no disease, 1 - disease exists, False - no missing BMI, True - missing BMI
# 460 patients have recorded BMI - 373 has no disease, 87 has disease
# 40 patients have missing BMI - 32 has no disease, 8 has disease

ehr_bmi_complete = ehr[complete.cases(ehr$bmi), ]
ehr_complete = ehr[complete.cases(ehr), ]
View(ehr_bmi_complete)
View(ehr_complete)

table(is.na(ehr$phq9_score), ehr$sud_any)
table(is.na(ehr$zip_income_quintile), ehr$sud_any)

stopifnot(all(ehr$age >= 0 & ehr$age <= 120, na.rm = TRUE))
stopifnot(all(ehr$age >= 18 & ehr$age <= 85, na.rm = TRUE))
stopifnot(all(ehr$age >= 48 & ehr$age <= 85, na.rm = TRUE))
stopifnot(all(ehr$bmi > 0, na.rm = TRUE))
stopifnot(all(ehr$bmi >= 16 & ehr$bmi <= 65, na.rm = TRUE))

ehr$age_group <- cut(
  ehr$age,
  breaks = c(0, 30, 50, 65, 120),
  labels = c("<30", "30-49", "50-64", "65+"),
  right = FALSE
)
ehr_sud <- ehr[ehr$sud_any== 1, ]
View(ehr_sud)

na_count <- rep(NA, ncol(ehr))
names(na_count) <- names(ehr)
for (j in 1:ncol(ehr)) {
  na_count[j] <- sum(is.na(ehr[[j]]))
}
sort(na_count, decreasing = TRUE)

clean_ehr <- function(df) {
  df$sex <- factor(df$sex)
  df$sud_any <- factor(df$sud_any, c(0,1), c("No SUD","SUD"))
  df$age_group <- cut(df$age,
                      breaks = c(0,30,50,65,120),
                      labels = c("<30","30–49","50–64","65+"),
                      right = FALSE)
  df
}
ehr_clean <- clean_ehr(ehr)

write.csv(ehr_clean, "sim_ehr_500_cleaned.csv", row.names = FALSE)