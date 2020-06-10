# ONSdeaths
The following code estimates excess non-COVID deaths in England &amp; Wales from ONS data.

- 'data' folder is where the most recent ONS data is uploaded
- ExcessDeathsCalculations.R forecasts deaths for the next 2 years
- CleanUp.R and Covid_ons_clean_up.R clean up the data that was placed in the 'data' folder. In these scripts, 'latestdata' (at the top of the code) needs to be manually changed to the most recent data file name
- Plots_tables_auto.R outputs tables and figures of interest (Note: function pp needs to be manually changed to toggle between the full forecast (supplementary figures A3-A5 in the report) and the March onwards forecast (Figures 2 and 3, Supplementary figures A6 and A7 in the report)
