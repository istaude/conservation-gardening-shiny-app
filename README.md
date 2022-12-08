
<!-- README.md is generated from README.Rmd. Please edit that file -->

# A Shiny App for Conservation Gardening

This Shiny app shows declining native plant species that are amenable to
gardening, how to grow them, and where to buy them for each German
federal state. The application can be accessed here in German:
<https://conservation-gardening.shinyapps.io/app-de/>; and here in
English: <https://conservation-gardening.shinyapps.io/app-en/>

The repository contains all the R code and data to reproduce the
analyses and figures in XXXX. In addition, it contains the [source
code](https://github.com/istaude/conservation-gardening-shiny-app/tree/master/CG-App)
for the application.

## Folder structure

1.  `Data-inputs` contains the master red list (i.e., the combined RLs
    across the 16 German federal states) and the respective pdf files of
    each individual RL.

2.  `Data-outputs` contains all the text-mining data from gardening and
    seller websites.

3.  `Data-shiny` is the data that streamline into the Shiny application.

4.  `R-code-for-database` contains the preamble loading all R packages,
    all text-mining scripts and scripts for data carpentry integrating
    the data used in the Shiny application.

5.  `R-code-for-analysis` contains all the scripts to reproduce the
    figures in our study.

6.  `Cg-app-de` contains the data, frontend and backend for the Shiny
    application in German.

7.  `Cg-app-en` contains the data, frontend and backend for the Shiny
    application in English.

## Contact

Please contact us at <ingmar.staude@uni-leipzig.de> if you have further
questions, or information on plants we have not listed yet among
conservation gardening species.
