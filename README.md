Hello, 
this R code was compiled by [Natalie Böhm] for her seminar paper. The code was gathered from the following repositories and adapted by me for my own use:
https://github.com/kassteele/Nowcasting
https://github.com/epinowcast/epinowcast
https://github.com/dwolffram/hospitalization-nowcast-hub-evaluation
https://github.com/adrian-lison/covid-reporting-switzerland

To use the code, one needs to clone the master branch and add the R project into the cloned folder (at the same level as the Epinow, Kassteele folders, etc.).
To perform a nowcast, only the 'Nowcast.R' file is needed.
To replot the plots with plot.R, the corresponding CSV files that were generated during the nowcasts are loaded from the Epinow and Kassteele directories.
The 'prepare model output' file is only used to concatenate information from the monthly nowcasts.
The 'dataanalysis' script can be used to trace the data analysis process.
