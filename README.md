# The Solow Growth Model

This model was built and published in **RStudio**, using **R version 4.2.3** as of **April 3, 2025**. It was developed under the assumption that users input valid values for both exogenous and endogenous variables (e.g., a savings rate equal to or greater than 1 will result in division by zero and or error with results).

---

## 📂 File Structure

The R file `a_solow_growth_model.R` contains the Shiny app code.  
The `modules` and `data` folders need to be in the **same root directory** as the Shiny app R file.

Refer to the following files for more information on the code structure:

- `app_structure.txt`  
- `variable_names.md`

---

## 🔗 Shiny App Link

[https://4o85b6-eduardo-ramirez.shinyapps.io/The-Solow-Growth-Model/](https://4o85b6-eduardo-ramirez.shinyapps.io/The-Solow-Growth-Model/)




**File description**

**master_app**

-   **a_solow_growth_model.R**

This is the main app for the long run solow model. This version is in my portfolio.

-   **data** folder

has savings data for different countries. This function is not yet to its full intended potential. The savings data is for the user to get the average savings rate for a country during a set time period. The average savings rate will then be use in the model as the original savings rate.

-   **modules** folder

This folder has the modules used in the app. The modules are used to separate the code into smaller parts. This makes it easier to read and maintain.

-   **app_structure.txt**

this txt file has the structure of the app.

-   **variable_names.md**

This file has the variable names used in the app. It is used to help understand the code and the variables used in the app.

**version_notes**

has notes on changes made to models.

**extra_files** has the following

-   **CH6_Solow.qmd**

Made to house older and or different version of the Long Run Solow Growth Model

This has the first models for chapter 6 python solow model

it also has the first r version of chapter 6 model.

also has a few sample for the aspire to do project

has shiny app older version

-   **module_versions**

older versions

-   **python_testing.qmd**

This file is just to test my original python models for the solow model.

-   **Test_module_sample_template**

Has a shiny app v2.3 that uses modules. The calculations in that test have not been verified.

-   **params_sample.xlsx**

sample excel file use to upload parameters to the model.

the code is set to only accept excel files with this columns. It is assume the proper param names and values will be inputted by the user. Abnormal values for the parameters can result in errors (for example a value of zero for some parameters can result in div by zero which will result in calculation errors).
