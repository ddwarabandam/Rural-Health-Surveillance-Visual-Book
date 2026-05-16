# Rural Health Surveillance Visual Book  
## Basic Instruction Manual

Prepared for sharing on GitHub for Nebraska DHHS and South Heartland District Health Department.

---

## 1. Purpose of the Application

The **Rural Health Surveillance Visual Book** is an R Shiny application designed to help public health teams explore, summarize, and visualize infectious disease laboratory surveillance and investigation data.

The application supports:

- Laboratory surveillance trend review
- Case/investigation trend review
- Combined lab and case volume visualization
- Program-level disease categorization
- Filtering by disease group, condition, county, sex, age group, race, ethnicity, case status, and investigation status
- De-identified data preview tables
- Simple summary metrics for situational awareness

This tool is intended for local public health surveillance, routine situational awareness, quality review, and internal data exploration.

---

## 2. Intended Users

This application is intended for:

- Local health department epidemiologists
- Disease surveillance staff
- Disease investigators
- Public health program managers
- Data analysts supporting communicable disease surveillance
- Nebraska DHHS or local health department partners reviewing surveillance trends

Users do not need advanced R programming knowledge to use the app once it is set up. Basic familiarity with CSV files, date fields, and surveillance data is helpful.

---

## 3. What the App Does

The script performs four major functions:

1. **Loads laboratory and investigation CSV files**
   - The app expects two CSV files stored in a local `data/` folder.
   - One file is for laboratory surveillance data.
   - One file is for investigation/case data.

2. **Cleans and standardizes the data**
   - Converts column names to uppercase.
   - Parses common date fields.
   - Handles mixed date formats, including Excel-style numeric dates.
   - Creates a standardized condition field.
   - Creates standardized age, race, and ethnicity variables.
   - Removes selected direct identifiers from the loaded dataset.

3. **Categorizes diseases into program buckets**
   - Respiratory
   - Enteric/Food-borne
   - STI
   - Vector-borne
   - Environmental
   - Healthcare-associated infections
   - Other/Viral/Zoonotic

4. **Creates interactive dashboards**
   - Summary metrics
   - Lab trends
   - Investigation trends
   - Combined lab and case trends
   - De-identified data preview tables

---

## 4. Required Software

Before running the app, install:

- R
- RStudio, recommended
- Required R packages listed below

### Required R Packages

The script uses the following packages:

```r
library(shiny)
library(shinybusy)
library(bslib)
library(dplyr)
library(lubridate)
library(plotly)
library(DT)
library(tidyr)
library(ggplot2)
library(stringr)
library(shinyWidgets)
```

### Install Packages

Run this once in R or RStudio:

```r
install.packages(c(
  "shiny",
  "shinybusy",
  "bslib",
  "dplyr",
  "lubridate",
  "plotly",
  "DT",
  "tidyr",
  "ggplot2",
  "stringr",
  "shinyWidgets"
))
```

---

## 5. Recommended Folder Structure

Place the app script and data files in a project folder using the structure below:

```text
rural-health-surveillance-visual-book/
│
├── app.R
│
├── data/
│   ├── lab_surveillance_data1.csv
│   └── investigations_data1.csv
│
└── README.md
```

### Important

The current script expects the data files to have these exact file paths:

```r
data/lab_surveillance_data1.csv
data/investigations_data1.csv
```

If your file names are different, either:

- Rename your files to match the expected names, or
- Update the file paths inside the script.

---

## 6. Input Data Requirements

### 6.1 Laboratory Surveillance File

Expected file name:

```text
data/lab_surveillance_data1.csv
```

The app is designed to work best when the lab file contains columns such as:

- `LAB_RPT_LOCAL_ID`
- `PATIENT_LOCAL_ID`
- `LAB_REPORT_DATE`
- `LAB_RPT_CREATED_DT`
- `SPECIMEN_COLLECTION_DT`
- `EVENT_DATE`
- `CONDITION`
- `DISEASE_CATEGORY`
- `CONDITION_CD`
- `PATIENT_COUNTY`
- `PATIENT_CURRENT_SEX`
- `PATIENT_DOB`
- `PATIENT_AGE_REPORTED`
- `PATIENT_REPORTED_AGE`
- `PATIENT_REPORTED_AGE_UNITS`
- `PATIENT_RACE_CALCULATED`
- `RACE`
- `PATIENT_ETHNICITY_CALCULATED`
- `ETHNICITY`

Not every column is required for the app to start, but some features depend on specific columns. For example, lab counts use `LAB_RPT_LOCAL_ID`.

### 6.2 Investigation File

Expected file name:

```text
data/investigations_data1.csv
```

The app is designed to work best when the investigation file contains columns such as:

- `INV_LOCAL_ID`
- `PATIENT_LOCAL_ID`
- `INV_REPORT_DT`
- `INV_START_DT`
- `RECORD_ADDED_TO_NBS_DTTIME`
- `EVENT_DATE`
- `REPORT_DT`
- `CONDITION`
- `DISEASE_CATEGORY`
- `CONDITION_CD`
- `PROGRAM_AREA`
- `JURISDICTION_NAME`
- `INVESTIGATION_STATUS`
- `INV_CASE_STATUS`
- `PATIENT_COUNTY`
- `PATIENT_CURRENT_SEX`
- `PATIENT_DOB`
- `PATIENT_AGE_REPORTED`
- `PATIENT_REPORTED_AGE`
- `PATIENT_REPORTED_AGE_UNITS`
- `PATIENT_RACE_CALCULATED`
- `RACE`
- `PATIENT_ETHNICITY_CALCULATED`
- `ETHNICITY`

Investigation counts use `INV_LOCAL_ID` when available. If `INV_LOCAL_ID` is not available, the app may count rows instead.

---

## 7. Privacy and De-identification Notes

The script removes selected direct identifiers during data loading, including:

- Patient first name
- Patient middle name
- Patient last name
- Patient street address
- Patient address
- Patient home phone number

However, users should still follow all applicable privacy, security, and data governance rules before uploading data to GitHub or sharing the app.

---

## 8. How to Run the App

### Step 1: Open the Project Folder

Open RStudio and set the working directory to the folder containing the app.

Example:

```r
setwd("path/to/rural-health-surveillance-visual-book")
```

### Step 2: Confirm Data Files Are Present

Make sure the following files exist:

```text
data/lab_surveillance_data1.csv
data/investigations_data1.csv
```

### Step 3: Open the App Script

Open the main script file, usually named:

```text
app.R
```

### Step 4: Run the App

In RStudio, click:

```text
Run App
```

Or run:

```r
shiny::runApp()
```

The app should open in the RStudio Viewer or in your default web browser.

---

## 9. Application Layout

The application contains five main tabs:

1. Summary
2. Data Preview
3. Combined Trends
4. Lab Surveillance
5. Investigations

Each tab is described below.

---

## 10. Summary Tab

The **Summary** tab provides high-level summary metrics for labs, investigations, or both.

### Main Controls

#### Summarize

Choose one of the following:

- **Labs**
- **Investigations**
- **Both (cases + labs)**

Use **Both** when you want to compare lab order volume and case/investigation volume side by side.

#### Show 5-year comparison

This optional checkbox displays a 5-year comparison using:

- 5-year median year-to-date value
- Percent change compared with the 5-year median
- Directional status indicator

This feature is useful for comparing current activity against prior years if the dataset includes enough historical data.

### Labs Settings

When Labs or Both is selected, users can choose:

- Date field for the X-axis/date range
- Grouping variable
- Date range
- Condition group
- Condition
- County
- Sex
- Age group
- Race
- Ethnicity

### Investigation Settings

When Investigations or Both is selected, users can choose:

- Date field for the X-axis/date range
- Grouping variable
- Date range
- Program bucket
- Condition
- County
- Sex
- Age group
- Race
- Ethnicity

### Summary Outputs

The Summary tab displays:

- Value boxes
- Summary table
- Bar chart

When Both is selected, the app can show:

- Total cases
- Total lab orders
- Cases per lab

The “cases per lab” value should be interpreted as a screening or surveillance signal, not as a diagnostic positivity rate unless the source data and denominator are confirmed to support that interpretation.

---

## 11. Data Preview Tab

The **Data Preview** tab shows de-identified filtered data tables.

It includes:

- Labs: de-identified data preview
- Investigations: de-identified data preview

The tables stay synchronized with the filters used in the corresponding Lab Surveillance and Investigations tabs.

### How to Use

1. Go to the Lab Surveillance or Investigations tab.
2. Apply filters.
3. Return to Data Preview.
4. Review the filtered records.

This is helpful for checking whether charts are being generated from the expected subset of records.

---

## 12. Combined Trends Tab

The **Combined Trends** tab displays laboratory volume and case/investigation volume in one visualization.

### Controls

#### Time frequency

Choose:

- Day
- Week
- Month
- Year

#### Plot type

Choose:

- Line
- Bar
- Horizontal Bar
- Bar + Line

### Recommended Use

Use this tab when you want to compare whether case/investigation activity is increasing, decreasing, or changing in relation to laboratory testing volume.

Example uses:

- Compare lab testing volume and confirmed case activity over time.
- Review whether increased case counts may reflect increased testing.
- Identify time periods where cases increased despite stable or declining testing.

---

## 13. Lab Surveillance Tab

The **Lab Surveillance** tab focuses on laboratory order volume trends.

### Main Controls

#### Choose X-Axis Date

Available date options include:

- `LAB_REPORT_DATE`
- `LAB_RPT_CREATED_DT`
- `SPECIMEN_COLLECTION_DT`
- `EVENT_DATE`

Use the date that best matches the surveillance question.

Common guidance:

- Use `LAB_REPORT_DATE` for routine lab reporting trends.
- Use `SPECIMEN_COLLECTION_DT` when interested in specimen collection timing.
- Use `LAB_RPT_CREATED_DT` when reviewing system entry or reporting workflow.
- Use `EVENT_DATE` only if it is consistently populated and meaningful in the dataset.

#### Group Data By

Options include:

- Program bucket
- Standardized condition
- Disease category
- County
- Sex
- Age group
- Race
- Ethnicity

#### Plot Type

Choose:

- Line
- Bar

If Bar is selected, users can choose:

- Vertical bar
- Horizontal bar

#### Time Frequency

Choose:

- Day
- Week
- Month
- Year

#### Plot Height

Use the slider to increase or decrease chart height.

### Filters

The Lab Surveillance tab includes multi-select filters for:

- Condition group
- Condition
- County
- Sex
- Age group
- Race
- Ethnicity

The filters are interactive and designed to narrow choices based on earlier selections.

### Recommended Use

Use this tab to answer questions such as:

- Which disease groups are driving laboratory volume?
- Are respiratory labs increasing over time?
- Which counties are contributing most lab reports?
- How do lab volumes vary by age group?
- Are certain conditions appearing more frequently in recent weeks?

---

## 14. Investigations Tab

The **Investigations** tab focuses on case and investigation trends.

### Main Controls

#### Choose X-Axis Date

Available date options include:

- `INV_REPORT_DT`
- `INV_START_DT`
- `RECORD_ADDED_TO_NBS_DTTIME`
- `EVENT_DATE`
- `REPORT_DT`

Common guidance:

- Use `INV_REPORT_DT` for routine investigation reporting trends.
- Use `INV_START_DT` to review investigation initiation timing.
- Use `RECORD_ADDED_TO_NBS_DTTIME` to review system entry timing.
- Use `EVENT_DATE` or `REPORT_DT` only if consistently populated and meaningful.

#### Group By

Options include:

- Program bucket
- Standardized condition
- Condition
- Program area
- Jurisdiction
- Investigation status
- Case status
- County
- Sex
- Age group
- Race
- Ethnicity

#### Plot Type

Choose:

- Line
- Bar

If Bar is selected, users can choose:

- Vertical bar
- Horizontal bar

#### Time Frequency

Choose:

- Day
- Week
- Month
- Year

#### Plot Height

Use the slider to increase or decrease chart height.

### Filters

The Investigations tab includes multi-select filters for:

- Condition group
- Condition
- Case status
- Investigation status
- County
- Age group
- Race
- Ethnicity

### Recommended Use

Use this tab to answer questions such as:

- Which disease categories are generating the most investigations?
- Are confirmed or probable cases increasing?
- Are open investigations accumulating?
- Which counties are reporting more cases?
- What are the age or demographic patterns among investigations?

---

## 15. Disease Program Buckets

The app groups conditions into broad program buckets to make surveillance review easier.

Current buckets include:

- Respiratory
- Enteric/Food-borne
- STI
- Vector-borne
- Environmental
- Healthcare-associated infections
- Other/Viral/Zoonotic

The script assigns a condition to a bucket based on the standardized condition label.

### Important Note

The disease bucket list is customizable. If new conditions appear in future datasets, they may fall into:

```text
Other/Viral/Zoonotic
```

If a condition is misclassified or missing from the expected bucket, update the `categorize_disease()` function in the script.

---

## 16. Date Handling

The script includes a helper function that attempts to parse different date formats.

It can handle:

- Standard R Date fields
- Character dates
- Factor-style dates
- Excel numeric dates
- Common formats such as year-month-day, month-day-year, and day-month-year

If a date column does not parse correctly, check the source CSV for inconsistent date formats.

---

## 17. Age Group Handling

The app creates age groups using the final calculated age.

The age calculation uses:

1. Reported age, if available
2. Date of birth-derived age, if reported age is missing

Age units such as days or months are converted into years when the age unit column is available.

Age groups are:

- 0–4
- 5–17
- 18–24
- 25–44
- 45–64
- 65+

Ages below 0 or above 120 are set to missing.

---

## 18. Race and Ethnicity Handling

The script creates standardized race and ethnicity fields.

For race, it checks:

- `PATIENT_RACE_CALCULATED`
- `RACE`

For ethnicity, it checks:

- `PATIENT_ETHNICITY_CALCULATED`
- `ETHNICITY`

This allows the app to work with slightly different export schemas.

---

## 19. Basic Interpretation Guidance

### Lab Counts

Lab counts represent the number of distinct lab report IDs, when `LAB_RPT_LOCAL_ID` is available.

These should generally be interpreted as lab report or lab order volume, not as unique cases.

### Case/Investigation Counts

Investigation counts represent the number of distinct investigation IDs, when `INV_LOCAL_ID` is available.

These should generally be interpreted as case or investigation volume, depending on the source dataset and case status filters used.

### Cases per Lab

Cases per lab is a simple ratio of case/investigation volume to lab volume.

Use this as a rough surveillance signal only. It should not automatically be interpreted as a positivity rate.

### Small Numbers

Use caution when interpreting small counts, especially for demographic subgroups or county-level breakdowns.

Small numbers can be unstable and may create privacy concerns. Apply Nebraska DHHS or local health department suppression rules before sharing outputs publicly.

---

## 20. Recommended Workflow for Users

### Routine Surveillance Review

1. Open the app.
2. Go to the Summary tab.
3. Select Both.
4. Choose the date range of interest.
5. Group by Program Bucket or Condition.
6. Review total cases, total lab orders, and cases per lab.
7. Go to Combined Trends.
8. Select week or month as the time frequency.
9. Compare lab and case trends.
10. Go to Lab Surveillance or Investigations for deeper review.

### Disease-Specific Review

1. Go to Lab Surveillance or Investigations.
2. Select the disease program bucket.
3. Select one or more specific conditions.
4. Choose the appropriate date field.
5. Select week or month as the time frequency.
6. Review the chart.
7. Use Data Preview to check the filtered records.

### County-Level Review

1. Go to the desired tab.
2. Select county under filters.
3. Group by condition, age group, or case status.
4. Review trends and counts.
5. Avoid public reporting of small cell counts unless approved and appropriately suppressed.

---

## 21. Basic Troubleshooting

### App does not start

Check that all required packages are installed.

Run:

```r
install.packages(c(
  "shiny",
  "shinybusy",
  "bslib",
  "dplyr",
  "lubridate",
  "plotly",
  "DT",
  "tidyr",
  "ggplot2",
  "stringr",
  "shinyWidgets"
))
```

### File not found error

Confirm that the data folder exists and contains:

```text
data/lab_surveillance_data1.csv
data/investigations_data1.csv
```

Also confirm that RStudio is using the correct working directory.

### Column not found error

The selected date, grouping, or ID column may not exist in the uploaded dataset.

Check that the CSV column names match expected names. The script converts column names to uppercase, so spelling matters more than capitalization.

### Blank chart

Possible reasons:

- Date field is missing or not parsed correctly.
- Selected filters removed all records.
- Date range does not include any records.
- Required ID column is missing.
- The selected grouping variable has only missing values.

Try clearing filters, expanding the date range, or selecting a different date field.

### Unexpected “Other/Viral/Zoonotic” category

The condition may not be listed in the current disease categorization function.

Update the `categorize_disease()` function to assign the condition to the correct program bucket.

---

## 22. Recommended GitHub Repository Notes

For GitHub posting, recommended files include:

```text
app.R
README.md
data/sample_lab_surveillance_data.csv
data/sample_investigations_data.csv
```

The sample data should be fake or synthetic.

Suggested GitHub README sections:

- Project overview
- Intended users
- Required packages
- Folder structure
- Input data format
- How to run the app
- Feature overview
- Privacy and data use notice
- Troubleshooting
- Contact or maintainer information

---

## 23. Suggested Disclaimer

This tool is intended for public health surveillance support, data exploration, and situational awareness. It does not replace official case classification, epidemiologic review, or agency-approved reporting workflows. Users are responsible for ensuring that all data use, sharing, and publication follow applicable privacy, security, and data governance requirements.

---

## 24. Current Version Notes

This instruction manual is based on the current working script version shared for the Rural Health Surveillance Visual Book. Future versions may add more advanced walkthroughs, screenshots, deployment instructions, example datasets, automated reports, or additional modeling features.

