library(readxl)
library(dplyr)
library(mice)

# original_data <- read_excel("Original_data.xlsx")

##### sort data #####
# Full dataset with 133 variables used for single imputation for missing data later on.
data_1 <- original_data %>% select(
  # Online home survey measures.
  1:4, 6, # Background variables. 
  "ONSKER_TJENESTE_I_FORSVARET",
  "PASSER_TIL_TJENESTE",
  80:82, 109, 110, # Physical fitness.
  "HELSEPLAGER", 
  "HELSEPLAGER2",
  85:96, # Health.
  "NARKOTIKA_SISTE_AARET",
  98:108, # Social.
  7:66, # OCEAN.
  
  # Derived from the home survey.
  67:76, # Score OCEAN.
  "SCORE_HELSE_EGENERKLARING",
  "SCORE_SOSIAL",
  "sesjonsstatus",
  
  # Education.
  "HAR_KRITISK_UTDANNING",
  "HAR_FORDYPNINGSFAG",
  "STATUS_VGS",
  "SKOLEPOENG_BEREGNET",
  
  # On-site assessment.
  "HELSEPROFIL", 
  "AE",
  "SCORE_FYSISK",
  "VURDERNG_SESJON", 
  
  # Derived from on-site assessment .
  "RESULTAT_SESJON", 
  "HELSEPROFIL_SESJON",
  "HELSEPROFIL_I_DAG", 
  
  # During military service.
  "ALDER_UNDER_TJENESTE",
  "UTHOLDENDE_REKRUTT_KARAKTER",
  "STYRKE_REKRUTT_KARAKTER",
  "FGREN",
  
  # After military service.
  "UTHOLDENDE_DIMISJON_KARAKTER",
  "STYRKE_DIMISJON_KARAKTER",
  "HOVEDKARAKTER_TJENESTEUTTALELSE",
  "FGTJ_DAGER",
  "FULLFORT_FGTJ",
  "ANSATT_I_DAG",
  "GRAD_I_DAG",
)

##### Make new outcome variables ####
# Entered military service
data_1$ALDER_UNDER_TJENESTE <- ifelse(is.na(data_1$ALDER_UNDER_TJENESTE), 0, 1) # In full data set, 10716 entered military service from total data set
names(data_1)[names(data_1) == "ALDER_UNDER_TJENESTE"] <- "KOM_INN_I_FGTJ" # Change name on ALDER_UNDER_TJENESTE

# Military training completed
data_1$FULLFORT_RS <- ifelse(data_1$FGTJ_DAGER < 56 | is.na(data_1$FGTJ_DAGER), 0, 1) # In full data set, 9364 completed military training 

# Military service completed
data_1$FULLFORT_270_DAGER <- ifelse(data_1$FGTJ_DAGER < 270 | is.na(data_1$FGTJ_DAGER), 0, 1) # In full data set, 8909 completed military service


#### Make variables into factors ####
vars_to_factor <- c("sex", 
                    "kull",
                    "FYLKE",
                    "ONSKER_TJENESTE_I_FORSVARET",
                    "PASSER_TIL_TJENESTE",
                    "BMI",
                    "HVOR_OFTE_TRENER_DU",
                    "HVOR_OFTE_FRILUFTSLIV",
                    "KARAKTER_STYRKETEST_EGENERKLARING",
                    "KARAKTER_3000_M_EGENERKLARING",
                    "ER_DU_BLITT_OPERERT",
                    "POLLENALLERGI",
                    "HELAARSALLERGI", 
                    "HUDSYKDOMMER",
                    "ALVORLIG_LUNGESYKDOM", 
                    "MIGRENE_HODEPINE",
                    "COLIAKI_GLUTENINTOLERANSE",
                    "LAKTOSEINTOLERANSE",
                    "BRILLER_ELLER_LINSER",
                    "FRAVAR_GRUNNET_SYKDOM",
                    "ADHD_ADD",
                    "HEMMENDE_ANGST_URO",
                    "NARKOTIKA_SISTE_AARET",
                    
                    "LETT_KONTAKT_MED_ANDRE",
                    "TRIVES_MED_MEDELEVER",
                    "OMGAAS_REGELMESSIG_FLERE_VENNER",
                    "LIKER_AA_TA_ANSVAR",
                    "TAR_INITIATIV_BLANT_VENNER",
                    "HAANDTERER_STRESS_OG_FRISTER_BRA",
                    "PROBLEMATISK_AA_SOVE_BORTE",
                    "SINT_SAA_JEG_SKADER_SELV_ELLER_ANDRE",
                    "OFTE_I_SLOSSKAMP",
                    "ROLIG_OG_AVBALANSERT_PERSON",
                    "KOMMER_GODT_OVERENS_MED_ANDRE",
                    "SCORE_SOSIAL",
                    
                    "KOM_INN_I_FGTJ",
                    "FULLFORT_RS",
                    "FULLFORT_270_DAGER"
)

# Convert the variables to factors
data_1 <- mutate_at(data_1, vars_to_factor, as.factor)

# Make OCEAN into factor
data_1[,38:98] = lapply(data_1[,38:98], factor)

##### New dataset, ONLY SURVEY PREDICTORS + POTENTIAL OUTCOMES ####
data <- data_1[, c(1,2,3,6:103, 109, 110, 111, 115, 123, 134, 135)] # Potential outcomes = FULLFORT_RS, ALDER_UNDER_TJENESTE, FULLFORT_270_DAGER.

#### Single imputation for NAs in GPA####
# To deal with 3849 missing GPAs and 1397 missing on-site assessment statuses from the total dataset.

# start=Sys.time()
# imp <- mice(data_1, m=1, maxit=50, method='rf', seed = 500)
# stop=Sys.time()
# stop-start
# saveRDS(imp, "imputed_data.rds")

# imp <- readRDS("imputed_data.rds")
completed_data <- complete(imp, 1)

# Add imputet GPA variable to data.
data <- data %>%
  mutate(SKOLEPOENG_BEREGNET2 = completed_data$SKOLEPOENG_BEREGNET)
# Remove original GPA with NAs.
data <- subset(data, select = -SKOLEPOENG_BEREGNET)

# Add the imputed on-site assessment status to data.
data <- data %>%
  mutate(sesjonsstatus2 = completed_data$sesjonsstatus)
# Remove original on-site assessment status with NAs.
data <- subset(data, select = -sesjonsstatus)

##### Dealing with other NAs ####
data$BMI <- as.numeric(data$BMI)
data$BMI[is.na(data$BMI)] <- "Unknown" # All NAs are put into own category and set to Unknown
data$BMI <- as.factor(data$BMI)
new_levels_BMI <- c("Unknown", 6, 1, 2, 3, 4, 5, 7) 
new_names_BMI <- c("Unknown",
                   "Mindre enn 17", 
                   "17 - 19.9", 
                   "20 - 24.9", 
                   "25 - 29.9",
                   "30 - 32.9",
                   "33 - 34.9",
                   "Over 35") 
data$BMI <- factor(data$BMI, levels = new_levels_BMI, labels = new_names_BMI)

# Kull
data$kull <- as.numeric(data$kull)
data$kull[is.na(data$kull)] <- "Unknown" # All NAs are put into own category and set to Unknown
data$kull <- as.factor(data$kull)
new_levels_kull <- c("Unknown", 1, 2, 3, 4, 5, 6, 7, 8, 9) 
new_names_kull <- c("Unknown",
                    "1994", 
                    "1995", 
                    "1996", 
                    "1997", 
                    "1998", 
                    "1999",
                    "2000",
                    "2001",
                    "2002") 
data$kull <- factor(data$kull, levels = new_levels_kull, labels = new_names_kull)

# Remove 72 NAs that are NA on almost all variables
data <- data[!is.na(data$KARAKTER_3000_M_EGENERKLARING), ]

imputed_data <- data

#### REMOVE EXCLUDED CASES ####
# Confidential code

#
#
#
#
#
#
#
#
#

# To see how many went to on-site assessment due to geography/critical competence (Subract the two) -> 1,646
table(data_1$sesjonsstatus) 
table(data1_wout_exclution$sesjonsstatus2)

# To see how many entered military service due to geography/critical competence (Subract the two) -> 474
table(data_1$KOM_INN_I_FGTJ)
table(data1_wout_exclution$KOM_INN_I_FGTJ)

# Remove sesjonsstatus2
data1_wout_exclution <- data1_wout_exclution[,-108] 

# New data set for analysis with outcome = completed military service
FULLFORT_RS_data_wout_exclution <- data1_wout_exclution[, !(names(data1_wout_exclution) %in% c("KOM_INN_I_FGTJ", "FULLFORT_270_DAGER"))] 

#### EXPORT RDS FILE ####
#saveRDS(FULLFORT_RS_data_wout_exclution, file = "Completed_military_training_data.rds") # Including PID