# empathy-and-acceptance

## Introduction
The Empathy and Acceptance study was conducted at Bar Ilan University. This repository contains essential information and scripts for analyzing the data collected during this psychology emotions practicum. In this README, we provide an overview of the key components of this project and how to use the scripts provided.

## Repository Structure
preprocessing.R: This script is responsible for preprocessing the data collected during the study. It takes the TSV files downloaded from Qualtrics in the 'rawdata' folder, cleans the dataset by removing participants who didn't meet the requirements, and calculates scale scores. The cleaned and processed data is stored in the 'preprocessed' folder.

analysis.R: The analysis script builds upon the preprocessed data generated by preprocessing.R. It conducts statistical analyses and generates visualizations to help us gain insights into the study's findings.

## Instructions
To effectively use the scripts in this repository, follow these steps:

Data Preparation: Make sure you have the TSV files obtained from Qualtrics saved in the 'rawdata' folder.

Preprocessing: Run the preprocessing.R script to clean the data and calculate scale scores. The results will be saved in the 'preprocessed' folder.

Analysis: Execute the analysis.R script to perform statistical analyses and create visualizations based on the preprocessed data. This will help in interpreting and understanding the outcomes of the study.
