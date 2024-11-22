packages <- c("jsonlite", "duckdb", "lubridate", "tidyverse", "dplyr","table1",'rvest', "readr", "arrow", "fst", "lightgbm", "caret", "Metrics", "ROCR", "pROC")

install_if_missing <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

sapply(packages, install_if_missing)

con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = ":memory:")

load_config <- function() {
  json_path <- file.path( "config.json")
  
  if (file.exists(json_path)) {
    config <- fromJSON(json_path)
    print("Loaded configuration from config.json")
  } else {
    stop("Configuration file not found. Please create config.json based on the config_template.")
  }
  
  return(config)
}

# Load the configuration
config <- load_config()



tables_location <- config$clif2_path
site <-'RUSH'
file_type <- paste0(".", config$filetype)


# Check if the output directory exists; if not, create it
if (!dir.exists("output")) {
  dir.create("output")
}

read_data <- function(file_path) {
  if (grepl("\\.csv$", file_path)) {
    return(read.csv(file_path))
  } else if (grepl("\\.parquet$", file_path)) {
    return(arrow::read_parquet(file_path))
  } else if (grepl("\\.fst$", file_path)) {
    return(fst::read.fst(file_path))
  } else {
    stop("Unsupported file format")
  }
}


# Read data using the function and assign to variables
location <- read_data(paste0(tables_location, "/clif_adt", file_type))
encounter <- read_data(paste0(tables_location, "/clif_hospitalization", file_type))
demog <- read_data(paste0(tables_location, "/clif_patient", file_type))
ventilator <- read_data(paste0(tables_location, "/clif_respiratory_support", file_type))

# Rename columns in the location data frame
location <- location %>%
  rename(encounter_id = hospitalization_id)

# Rename columns in the encounter data frame
encounter <- encounter %>%
  rename(encounter_id = hospitalization_id)

# Rename columns in the demog data frame
demog <- demog %>%
  rename(
    race = race_category,
    ethnicity = ethnicity_category,
    sex = sex_category
  )

ventilator <- ventilator %>%
  rename(
    encounter_id = hospitalization_id
  )

# First join operation
join <- location %>%
  select(encounter_id, location_category, in_dttm, out_dttm)

# Second join operation to get 'icu_data'
icu_data <- join %>%
  left_join(encounter %>% select(patient_id, encounter_id, age_at_admission, discharge_category,admission_dttm), by = "encounter_id") %>%
  mutate(
    admission_dttm = ymd_hms(admission_dttm), # Convert to POSIXct, adjust the function as per your date format
    in_dttm = ymd_hms(in_dttm), # Convert to POSIXct, adjust the function as per your date format
    out_dttm = ymd_hms(out_dttm)
  )

# Filter rows where location is ICU and in_dttm is within 48 hours of admission_dttm
icu_data <- icu_data %>%
  mutate(location_category = ifelse(location_category == "procedural", "OR", location_category)) %>%
  mutate(location_category = toupper(location_category))

icu_48hr_check <- icu_data %>%
  filter(location_category == "ICU",
         in_dttm >= admission_dttm,
         in_dttm <= admission_dttm + lubridate::hours(48),
         lubridate::year(admission_dttm) >= 2020,
         lubridate::year(admission_dttm) <= 2021,
         age_at_admission >= 18,
         !is.na(age_at_admission)) %>%
  distinct(encounter_id) %>%
  pull(encounter_id)
  
# Filter icu_data to only include rows with encounter_ids in icu_48hr_check and within 72 hours of admission
icu_data <- icu_data %>%
  filter(encounter_id %in% icu_48hr_check,
         in_dttm <= admission_dttm + hours(72)) %>%
  arrange(in_dttm) %>%
  mutate(RANK = rank(in_dttm, ties.method = "first")) %>%
  arrange(encounter_id, in_dttm) %>%
  group_by(encounter_id) %>%
  mutate(RANK = rank(in_dttm, ties.method = "first"))

  # Compute minimum rank for ICU locations
min_icu <- icu_data %>%
  filter(location_category == "ICU") %>%
  group_by(encounter_id) %>%
  summarize(min_icu = min(RANK))

# Merge the minimum ICU rank back into the original dataset
icu_data <- icu_data %>%
  left_join(min_icu, by = "encounter_id")

# Filter based on rank being at least the minimum ICU rank
icu_data <- icu_data %>%
  filter(RANK >= min_icu) %>%
  arrange(in_dttm)

# Change 'OR' to 'ICU' in location_category
icu_data <- icu_data %>%
  mutate(location_category = ifelse(location_category == "OR", "ICU", location_category))

# Create a new group_id based on changes in location_category
icu_data <- icu_data %>%
  group_by(encounter_id) %>%
  mutate(group_id = cumsum(location_category != lag(location_category, default = first(location_category)))) %>%
  ungroup()

icu_data <- icu_data %>%
  group_by(patient_id , encounter_id, location_category, group_id) %>%
  summarize(
    min_in_dttm = min(in_dttm),
    max_out_dttm = max(out_dttm),
    admission_dttm = first(admission_dttm),
    age = first(age_at_admission),
    dispo = first(discharge_category),
    .groups = 'drop'
  )

# Compute minimum group_id for each encounter_id where location_category is 'ICU'
min_icu <- icu_data %>%
  filter(location_category == "ICU") %>%
  group_by(encounter_id) %>%
  summarize(min_icu = min(group_id), .groups = 'drop')

# Merge the minimum ICU group_id back into the original dataset
icu_data <- left_join(icu_data, min_icu, by = "encounter_id")

# Filter based on group_id matching min_icu and duration condition
icu_data <- icu_data %>%
  filter(min_icu == group_id,
         interval(min_in_dttm, max_out_dttm) >= dhours(24)) %>%
  arrange(min_in_dttm)

  # Add 24 hours to the 'min_in_dttm' column
icu_data <- icu_data %>%
  mutate(after_24hr = min_in_dttm + hours(24))

# Select specific columns
icu_data <- icu_data %>%
  select(patient_id, encounter_id, min_in_dttm, after_24hr,max_out_dttm, age, dispo)

# Merge with demographic data and select specific columns
icu_data <- icu_data %>%
  left_join(demog, by = "patient_id") %>%
  select(encounter_id, min_in_dttm, after_24hr,max_out_dttm, age, dispo, sex, ethnicity, race)

ventilator <- ventilator %>%
  filter(device_category =="IMV")%>%
  select(encounter_id) %>% distinct() %>% deframe()


# Remove rows with missing 'sex' and create new variables
icu_data <- icu_data %>%
  filter(!is.na(sex)) %>%
  mutate(
    isfemale = as.integer(tolower(sex) == "female"),
    Mortality  = as.integer(grepl("dead|expired|death|died", dispo, ignore.case = TRUE)),
    site = site,
    Ventilator = ifelse(encounter_id %in% ventilator, 1, 0)
  )



# Define race and ethnicity mappings using case_when
icu_data <- icu_data %>%
  mutate(
    race = case_when(
      race == "White" ~ "White",
      race == "Black or African American" ~ "Black",
      race == "Black or African-American" ~ "Black",
      race == "Asian" ~ "Asian",
      race %in% c("Other", "Unknown", "Did Not Encounter", "Refusal", 
                  "American Indian or Alaska Native", 
                  "Native Hawaiian or Other Pacific Islander") ~ "Others",
      TRUE ~ "Others"  # Default case for NA and any other unexpected values
    ),
    ethnicity = case_when(
      ethnicity == "Hispanic" ~ "Hispanic or Latino",
      ethnicity == "Hispanic or Latino" ~ "Hispanic or Latino",
      TRUE ~ "Not Hispanic"  # Default case for NA and any other unexpected values
    )
  ) %>%
  mutate(sex = as.factor(sex),
         race = as.factor(race),
         ethnicity = as.factor(ethnicity),
         Mortality = as.factor(Mortality),
         Ventilator = as.factor(Ventilator))

# Calculate the difference in hours
icu_data$ICU_stay_hrs <- as.numeric(difftime(icu_data$max_out_dttm, icu_data$min_in_dttm, units = "secs")) / 3600

write.csv(icu_data, paste0( "output/ICU_cohort", '.csv'), row.names = FALSE)


# HTML content (make sure your actual HTML string is correctly input here)
html_content <- table1(~ sex + age + race + ethnicity + Mortality + Ventilator + ICU_stay_hrs, data=icu_data)

# Use rvest to read the HTML table
table <- read_html(html_content) %>%
  html_table(fill = TRUE)

# The first element of the list should be your table
df <- table[[1]]

# Rename 'Overall(N=14598)' to 'fabc(N=14598)' using the site variable
names(df) <- gsub("Overall\\(N=(\\d+)\\)", paste0(site, ' ', "(N=\\1)"), names(df))
write.csv(df, paste0( "output/table1_",site, '.csv'), row.names = FALSE)