﻿# Data-Science-Project
#Introduction: 
Cardiovascular illnesses (CVDs) are the major cause of death worldwide. CVDs include 
coronary heart disease, cerebrovascular disease, rheumatic heart disease, and other heart and 
blood vessel problems. According to the World Health Organization, 17.9 million people die 
each year. Heart attacks and strokes account for more than four out of every five CVD deaths, 
with one-third of these deaths occurring before the age of 70. A comprehensive database for 
factors that contribute to a heart attack has been constructed. 
The main purpose here is to collect characteristics of Heart Attack or factors that contribute to 
it.  
The size of the dataset is 152 samples, which have seven fields, where six fields are for input 
fields and one field for an output field. Age, gender(0 for Female, 1 for Male) ,heart rate 
(impulse), systolic BP (pressurehight), diastolic BP (pressurelow), blood sugar(glucose) are 
representing the input fields, while the output field pertains to the presence of heart attack 
(class), which is divided into two categories (negative and positive); negative refers to the 
absence of a heart attack, while positive refers to the presence of a heart attack. 
The Heart Disease Classification Dataset provides a crucial resource for investigating the 
factors contributing to cardiovascular illnesses, the leading cause of global mortality. 
Comprising 1319 patient samples, the dataset encompasses nine key variables. Eight input 
features – age, gender (coded as 0 for female and 1 for male), heart rate (impulse), systolic 
blood pressure (pressurehight), diastolic blood pressure (pressurelow)and  blood sugar 
(glucose) – offer a detailed physiological profile. The output variable, 'class', indicates the 
presence or absence of a heart attack, categorized as 'positive' or 'negative', respectively. This 
carefully constructed database aims to facilitate the identification of significant characteristics 
associated with heart attacks. 

 The following preprocessing and exploratory steps have already been completed as part of the project development:

All missing values in the dataset were identified and appropriately handled using suitable methods based on the nature of the data.

Missing values were visualized using graphs to better understand their distribution.

Outliers were detected and addressed using relevant techniques to minimize their impact on analysis.

Data transformation was applied where necessary, including conversions between numeric and categorical attributes.

Normalization techniques were used on continuous attributes to bring them to a comparable scale.

Duplicate entries in the dataset were detected and removed to ensure data integrity.

Filtering methods were applied to refine the dataset and retain relevant information.

Invalid data points were identified and treated using appropriate data-cleaning strategies.

The dataset was balanced to address class imbalance issues and improve model performance.

The data was split into training and testing sets to enable effective model evaluation.

Central tendencies (mean, median, mode) of the Age attribute were compared across Gender groups, with interpretations provided.

Similar comparisons of Age's central tendencies were performed across different Glucose levels.

Measures of spread (Range, IQR, Variance, Standard Deviation) of Age were also compared across Gender groups, along with corresponding interpretations.
