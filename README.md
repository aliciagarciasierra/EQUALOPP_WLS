The data folder should contain the three raw datasets in .dta, and, once created with the MASTER file, also the .rda versions of the same datasets.
The .gitignore file indicates which files should be ignored for the commits, meaning that changes to the files that are in the .gitignore will remain in your local copy, but will not be pushed remotely.
In this way you can keep the data in the repository remotely, without publishing it on github :) 


## other points:
- regarding the iterations, I'm using m=20, maxit= 5 and n_boot=1000. I haven't updated the code to preserve the current state, but just so you know

## things to discuss:
- with imputation there's some varying in the final number of observations of each dataset (small variations, of 2 or 4 observations)

## new points:
- 01_CLEAN_DATA.R now creates a separate rds file for each outcome (all given the same sample), which can then be read and combined. 
  Multiple imputation, age filtering, and outlier removal can all be applied by changing the arguments at the top of the script.
- 01_CLEAN_DATA_single_outcomes.R creates a separate rds file for each outcome, each with the maximum available sample for that outcome (this can be removed at some point if not used).
- 02_ANALYSES_MAIN.R and 02_ANALYSES_IMPUT.R produce results for both PGIs and observed abilities, for the desired outcomes (to specify at the top).
  The scripts also produce a graph with all the outcomes together (if multiple are given), and the separate plots for each outcome, to be read and 
  combined in 05_GRAPHS_PRESENTATIONS.R 

