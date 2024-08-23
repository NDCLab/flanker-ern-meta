# Derivatives

### Explanation of files and their origin
The file named ern_meta_data_for_r_2024-08-21.csv is the final data file used for the meta-analyses. It was constructed as follows:

The file named AnxOverall_and_DERN.csv was downloaded from the Saunders & Inzlicht (2020) OSF site: https://osf.io/r7dvc/. This file contains the effect sizes that Saunders & Inzlicht (2020) previously extracted for their meta-analysis. 

To perform additional coding of the studies denoted in the AnxOverall_and_DERN.csv file, we converted the file to .xlsx and added additional columns for independent coding of various task parameters, reconciliation of codes, and for tracking and updating codes based on reaching out to authors. These full details are depicted in the file named 
parameter_coding_reconciliation_author-updates_and_final-codes_cleaned-copy.xlsx. Note that there is an additional version of this latter file, with more verbose notation, but the latter version was cleaned to produce the file named parameter_coding_reconciliation_author-updates_and_final-codes_cleaned-copy.xlsx.

A duplicate .xlsx version of the file named parameter_coding_reconciliation_author-updates_and_final-codes_cleaned-copy.xlsx was made and renamed as ern_meta_data_for_r_2024-08-21.xlsx. Within this duplicate file, columns unneeded for analyses were removed. Finally, an additional version of this file was saved in .csv format for the purpose of conducting meta-analyses in R. 
