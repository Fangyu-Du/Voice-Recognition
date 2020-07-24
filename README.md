# Voice Recognition by Gender
machine learning-Python
 
## Project Overview
In our daily life, we can easily recognize one’s gender by hearing his or her voice most of the time. But how can a machine recognize voice gender? In our project, we are trying to train machine learning models to recognize the gender of voice based on acoustic properties.

## Data cleaning
Our data sources are mainly from two datasets, one from Kaggle: “Gender Recognition by Voice”, and the other from “Common Voice”. The “Gender Recognition by Voice” dataset already contains the gender label and the voice features we need, while the “Common Voice” dataset only includes gender labels and the original MP3 files with them. We pre-processed the dataset by doing acoustic analysis of the MP3 files in R using the seewave and tuneR packages and extracted the required voice features. After combining all the data we got, we now have a complete dataset with over 17,000 observations - each with 20 voice features and a gender label
