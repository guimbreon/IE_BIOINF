# README

This README file provides an overview and instructions for the R script provided.

### Dataset

The "Grupo10" dataset includes information about a group of individuals, such as their age, sex, study hours, sleep hours, social media usage hours, and TV watching hours. It also includes their responses to several burnout-related questions. 

### Description

The R script performs various data cleaning and statistical analysis tasks on a dataset named "Grupo10". The script includes the following major sections:

1. Data Cleaning: The script cleans the data by removing invalid values and replacing them with `NA` (missing values) for the variables: `HorasEstudo`, `HorasRedes`, `HorasTV`, and `HorasSono`.

2. Data Organization: The script organizes the data by converting numeric codes into meaningful labels for the variables: `Sexo`, `opcao1`, `Escolhi`, and `PMentoria`.

3. Required Packages: The script installs and loads the "nortest" package, which is used for performing statistical tests.

4. Statistical Analysis: The script performs several statistical analyses and calculates confidence intervals for different variables and research questions. The analyses include:

   - Calculation of the confidence interval for the mean age of students.
   - Calculation of the confidence interval for the proportion of male students choosing a specific course as their first option.
   - Calculation of the confidence interval for the difference in mean daily hours spent on social networks between first and third-year students.
   - Calculation of the confidence interval for the proportion of Bioinformatics students with knowledge of the Mentoring Program.
   - Binomial hypothesis testing for the proportion of students choosing option 1 and the proportion of hours spent on social networks greater than or equal to 0.
   - Hypothesis testing for the mean number of weekly study hours of first-year students and its comparison to the population mean of 12.
   - Hypothesis testing for the difference in hours dedicated to social networks among students and the population mean.
   - Hypothesis testing for the median number of weekly study hours of first-year students and its comparison to the population median of 8.
   - Hypothesis testing for the median hours dedicated to social networks among students and its comparison to the population median of 2.

### Usage

1. Data Preparation: Before running the script, make sure you have a dataset named "Grupo10" available in your R environment.

2. Install Required Packages: The script requires the "nortest" package. If you don't have it installed, uncomment the line `install.packages("nortest")` to install it.

3. Run the Script: Execute the R script in your preferred R environment (e.g., RStudio) or run it line by line.

4. Review the Results: The script will provide output for each statistical analysis, including confidence intervals and hypothesis test results. The results will be displayed in the console.

## Note

Please note that the script assumes the availability of the dataset "Grupo10" and specific variable names such as "Idade", "Sexo", "opcao1", "Escolhi", "PMentoria", "HorasEstudo", "HorasRedes", "HorasTV", and "HorasSono". Make sure your dataset has these variables or modify the script accordingly.

Feel free to modify the script to suit your specific needs and adapt it to different datasets or research questions.

For any questions or issues, please contact the authors of the script (Grupo 10) using the provided student IDs.

