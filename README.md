# Mortality Model

## Objective

The Mortality Model is designed to predict patient outcomes using various clinical data points. This document outlines the setup and requirements needed to utilize the model effectively.

## RCLIF Tables required
The following tables from the RCLIF database are required for the Mortality Model. Each table must include the specified fields below: 

* **patient_demographics** (`encounter_id`, `race`, `ethnicity`, `sex`)
* **encounter_demographics_dispo** (`encounter_id`, `age_at_admission`, `disposition`)
* **limited_identifiers** (`encounter_id`, `admission_dttm`)
* **adt** (`encounter_id`, `location_category`, `in_dttm`, `out_dttm`)
* **vitals** (`encounter_id`, `recorded_dttm`, `vital_category`, `vital_value`) 
    * `vital_category` must include `weight_kg`, `pulse`, `sbp`, `dbp`, `temp_c`,`height_inches`
* **labs** (`encounter_id`, `lab_order_dttm`, `lab_category`, `lab_value`, `lab_type_name`) 
    * `lab_category` must include  'albumin', 'alkaline_phosphatase', 'ast', 'basophil', 'bilirubin_conjugated', 'bilirubin_total', 'calcium', 'chloride', 'hemoglobin', 'lymphocyte', 'monocyte', 'glucose_serum',  'neutrophil', 'potassium', 'sodium', 'total_protein','platelet count', 'wbc'. 

Note- 
1. Make sure that the `lab_type_name` is correctly labelled as either "standard", "poc", etc. We filter for "standard" labs for this project.
2. Ensure that all the labs mentioned above are present in your dataset. The model will not run if any of these are missing. 
4. Please confirm the race and ethnicity mapping for your data is correctly mapped. 


## Setup instructions

### Creating and Activating a Virtual Environment and RUN!!!!!!!

First Pull the repo and update the config.json with the path to your tables & update site name

#### for Python 
Follow these steps to create a virtual environment.
1. Open terminal & run the `setup_mortality_model.sh`(mac) or `setup_mortality_model.bat` (win) script in terminal to set up a virtual environment for this project. 
    Mac Setup: 
    ```
        chmod +x setup_mortality_model.sh
        ./setup_mortality_model.sh
     ```
   Win Setup
     ```
        ./setup_mortality_model.bat
     ```
2.  Activate the virtual environment by running the below command in the terminal
    ```
    source .mortality_model/bin/activate
    ```
3. Select the new kernel `Python (mortality_model)` in your Jupyter notebook.

Choose the appropriate script based on CLIF version you have. Run the Inference_py notebook to execute the model (supports .pkl, .json, and .txt). Outputs are saved in the script's directory.

#### for R

Set your RStudio directory to the root of the Git repo. Select the script matching your CLIF version. Run Inference_r_script to execute the model (supports .pkl, .json, and .txt). Outputs are saved in the project root folder.


#### Run Table 1 & Temp Trajectory cohort 

Run the ICU_cohort_id_script - CLIF *.*.R to generate table 1 and cohort dataset for Temp trajectory project.