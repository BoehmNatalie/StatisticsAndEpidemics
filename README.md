Hello, 
this R code was compiled by [Natalie Böhm] for her seminar paper. The code was gathered from the following repositories and adapted by me for my own use:
https://github.com/kassteele/Nowcasting
https://github.com/epinowcast/epinowcast
https://github.com/dwolffram/hospitalization-nowcast-hub-evaluation
https://github.com/adrian-lison/covid-reporting-switzerland

To use the code, the main branch needs to be cloned. After cloning, the R project file should be added into the cloned folder, at the same level as the Epinow, Kassteele, and other directories.

Performing a Nowcast:

To perform a nowcast, only the Nowcast.R file is required.
The key parameters that need to be adjusted for different nowcasts using the van de Kassteele model are:

    nowcast_date,
    while_end_date,
    locationfilter.

The parameters mean delay, maximal delay, and days back should remain unchanged to ensure the nowcasts are comparable to those presented in the paper.

For EpiNow2, one can adjust:

    locationfilter,
    end_date,
    start_date.

According to the seminar paper, max_delay can be set to 20 for GE and 10 for BS.
Note: Nowcast time spans were chosen to cover two days to allow for performance testing currently.

Plotting Results:

To replot the results using plots.R, the corresponding CSV files generated during the nowcasts are loaded from the Epinow and Kassteele directories. Here, horizons and location filters can also be adjusted to customize the plots.
Other Scripts:

The prepare_model_output.R script is used only to concatenate outputs from monthly nowcasts and is not required for others.
The dataanalysis.R script documents the data analysis process. In this file, location filters, dates, and even the dataset can be modified (e.g. use cases with maybe some renames).

Note on Auxiliary Folders:

The folders prepfiles and functions are imported and adapted from the sources mentioned above. They do not need to be modified by the user.

