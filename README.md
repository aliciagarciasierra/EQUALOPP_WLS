The data folder should contain the three raw datasets in .dta, and, once created with the MASTER file, also the .rda versions of the same datasets.
The .gitignore file indicates which files should be ignored for the commits, meaning that changes to the files that are in the .gitignore will remain in your local copy, but will not be pushed remotely.
In this way you can keep the data in the repository remotely, without publishing it on github :) 

## new points:
- 02_ANALYSES_WLS.R now performs the analysis for both PGIs and observed abilities, you just have to set the value you want at the beginning of the script

