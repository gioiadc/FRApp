
# FRApp

<!-- badges: start -->
<!-- badges: end -->

**FRApp** is an R package and an R-based Shiny application providing a
user-friendly interactive interface to streamline the data analysis by
fitting nonlinear mixed-effects regression models with an asymptotic
exponential functional relationship on data with a hierarchical
structure. The package provides a comprehensive suite of tools tailored
for efficient data analysis and visualization.

The package includes data from Fluorescence Recovery After
Photobleaching (FRAP) experiments on actin dynamics in dendritic spines.

To explore all the functionalities of the **FRApp**, a comprehensive
guide is available in a dedicated vignette.

## How to install **FRApp**

To use the **FRApp** application first download and install R and
RStudio from [posit.co](https://posit.co/download/rstudio-desktop/).

You can install the package executing the following commands from the
RStudio console:

``` r
install.package("FRApp")
```

You can install the development version of **FRApp** from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("gioiadc/FRApp")
```

## How to launch **FRApp**

To load the package and make the functions available to be used run the
command:

``` r
library(FRApp)
```

You must execute this command every time you restart Rstudio.

To launch the Shiny app, copy and paste the following code in the
console:

``` r
FRApp()
```

The application opens automatically in the browser.

**FRApp** allows you to

- Load the data

- Estimate and compare exponential mixed-effects models

- Print a model report and export the data and the model

<center>

<figure>
<img src="man/figures/FRApp1.png" style="width:80.0%"
alt="FRApp front page: Mixed Effects Model" />
<figcaption aria-hidden="true">FRApp front page: Mixed Effects
Model</figcaption>
</figure>

</center>

## FRApp functionalities

The **Fit the model** button allows you to estimate the model. At the
end of the estimation process, the following objects will appear on the
right side of the application:

- summary information on the model

- diagnostic plots: scatterplot of residuals vs. estimated values and
  quantile-quantile plot of the residuals

- approximate 95% confidence intervals of the parameters of interest.

Fit is perfomed with the function <tt>`nlme`</tt>.

You can save models for model comparison with the button **Add to model
list**. Saved models must have different names. When multiple models are
saved, the table at the bottom of the page displays comparisons among
the saved models using AIC, BIC, and likelihood ratio tests.

The **Reset model list** button allows you to delete the saved models
from the list.

From the drop-down menu in the Download section, it is possible to
select a model saved in the model list and download a report and some
model’s objects.

The **Download Report** button allows you to export the results printed
in the application (model summary, residual graphs, and intervals) into
a PDF document.

The **Download RData** button allows you to export an RData file
containing six objects:

- data: the dataset in the format used for the analysis

- fit: the output of the estimated model

- pred: the values estimated by the model

- CI: the approximate 95% confidence intervals

- resid: the residuals of the model

- raneff: the random effects available at the different hierarchical
  levels.

## References

As a reference to the construction of the model and the different
options to specify, we refer to the book: Pinheiro, J., & Bates, D.
(2006). Mixed-effects models in S and S-PLUS. Springer science &
business media.

As a reference to the FRAP data analysis example: Di Credico, G.,
Pelucchi, S., Pauli, F., Di Luca, M., Marcello, E., & Edefonti, V.
Nonlinear mixed-effects models to analyze actin dynamics in dendritic
spines.
