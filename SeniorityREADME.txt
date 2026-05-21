What Explains Seniority in the U.S. House of Representatives?
Author: Bryson Benford — Texas A&M University
Updated: March 2026
________________


Overview
This script replicates the analysis from the accompanying research note examining what predicts seniority in the 116th U.S. House of Representatives (2019–2021). It tests whether party affiliation explains variation in how long members serve, controlling for district safety and ideological distance from the chamber median.
________________


Requirements
R version: 4.0 or higher recommended
Packages:
* readr — data import
* dplyr — data wrangling
* ggplot2 — figures
* forcats — factor reordering
* broom — tidy regression output
* sandwich — robust standard errors
* lmtest — coefficient testing
* scales — plot formatting
Install all at once with:
install.packages(c("readr", "dplyr", "ggplot2", "forcats", "broom", "sandwich", "lmtest", "scales"))


________________


Data
Place congress116data.csv in the same folder as the script before running. The script will stop with a clear error message if the file is not found.
Key variables used:
Variable
	Description
	seniority
	Number of terms served (dependent variable)
	party
	Party affiliation
	inc_pres_pct_2p
	Incumbent party's share of the two-party presidential vote in the district (district safety proxy)
	meddist
	Ideological distance from the congressional median
	________________


How to Run
1. Open Benford_Seniority_Finished.R in RStudio or any R environment
2. Set your working directory to the folder containing the script and data file
3. Run the script top to bottom
All outputs are saved automatically to an output/ folder created in the working directory.
________________


Outputs
File
	Description
	figure1_mean_seniority_by_party.png
	Bar chart of average terms served by party
	figure2_seniority_distribution.png
	Histogram of seniority across all members
	figure3_coefficient_plot.png
	Coefficient plot for the controlled regression model
	table_descriptive_by_party.csv
	Descriptive summary statistics by party
	table_major_party_summary.csv
	Summary for major-party sample used in regression
	table_regression_results.csv
	Full regression output with HC3 robust standard errors
	________________


Methods Note
The regression analysis is restricted to Democrats and Republicans. The dataset includes only one Independent and one Libertarian, which are insufficient for reliable inference. Heteroskedasticity-robust standard errors (HC3) are used throughout to produce defensible estimates.