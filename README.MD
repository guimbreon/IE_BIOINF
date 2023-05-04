## IE

This project is for the class of introduction to Statistics, it includes data cleaning, descriptive bivariate analysis and Regression Analysis of a dataset named "Grupo10".

###




### Dataset

The "Grupo10" dataset includes information about a group of individuals, such as their age, sex, study hours, sleep hours, social media usage hours, and TV watching hours. It also includes their responses to several burnout-related questions. 

All this information was gathered by a survey, from which the participants were the people doing the courrses IE, PE, ATED, EA and ATMD at ESTBarreiro/IPS.

### Cleaning and Organizing Data

In the first part of the project, the data was cleaned to remove any values that were not within the expected range of hours. For example, any study or sleep hours that were greater than 24 were replaced with "NA". This was done to ensure that the data used in the analysis was accurate and reliable. And then the variable "Sexo" is converted from numeric to categorical, where 1 is "Female" and 2 is "Male". The variables "opcao1", "Escolhi", and "PMentoria" are also converted from numeric to categorical, where 1 is "Yes" and 2 is "No".


### Univariate Descriptive analysis

In the second part, univariate descriptive analysis is performed on some variables of the "Grupo10" dataset. The "pander" library is used to create tables with absolute frequency, relative frequency, and percentage relative frequency for the variables "Age", "Sex", "Course", and "AcademicYear". Each table is created in three steps: calculating absolute frequency, calculating relative frequency, and calculating percentage relative frequency. Then, the three measures are organized into a table using the "pander" library.

### Descriptive Bivariate Analysis

Study Questions:

1. How does mean of sleep hours vary along the age.
2. Do woman study more than men?  Or is it the oposite?
3. Is there any relashion between emotional exaustion from studying across the genres?



In the third part of the project, a descriptive bivariate analysis was performed to explore the relationship between two variables in the dataset.

In the first part of the analysis, the mean sleep hours for each age group in the dataset were calculated and displayed in a table. In the second part of the analysis, the mean study hours for males and females in the dataset were compared, and the percentage difference was calculated and displayed in a table. In the third part of the analysis, a contingency table and bar plot were created to visualize the distribution of burnout-related responses for males and females in the dataset.

### Regression Analysis


Study Questions:
1. Do people watch tv more than they sleep?
2. Is the TikTok generation sleeping less than it uses social media?


In the fourth part of the project, a simple linear regression analysis was performed to explore the relationship between two variables in the dataset. Specifically, the relationship between TV watching hours and sleep hours was examined and hours in social media and sleeping. A scatter plot was created to visualize the relationship, and a linear model was fitted to the data. The coefficient of determination (R-squared) was calculated to determine the strength of the relationship.

### Conclusion

Overall, this project provides an example of how to clean and analyze a dataset using R. The results of the analysis can be used to gain insights into the relationships between variables in the dataset and to inform future decision-making processes.
	
