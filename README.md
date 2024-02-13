
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

A detailed guide describing the analysis of these data using **FRApp**
is available in a dedicated vignette. 


## How to install **FRApp**

To use the **FRApp** application first download and install R and RStudio from [posit.co](https://posit.co/download/rstudio-desktop/).

You can install the package executing the following commands from the RStudio console:

``` r
install.package("FRApp")
```

You can install the development version of **FRApp** from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("gioiadc/FRApp")
```

## How to launch **FRApp**

To load the package and make the functions available to be used run the command:

``` r
library(FRApp)
```
You have to execute this command every time you restart Rstudio.

To launch the Shiny app, copy and paste the following code in the console:

``` r
FRApp()
```

The application opens automatically in the browser.

The **FRApp** application has two tabs, termed **Mixed Effects Model**
and **Compare**.

The **Mixed Effects Model** tab allows you to:

- load the data

- estimate the model

- print a model report

- export the data and the model

- save the estimated models for model comparison.

<figure>

<img src="man/figures/README-2FRApp.png"

alt="Shiny app example screenshot" />

<figcaption aria-hidden="true">
FRApp front page: Mixed Effects Model tab
</figcaption>
</figure>

The **Compare** tab allows you to compare saved models by using
information criteria and likelihood ratio tests, when appropriate.

## How to prepare data

The **FRApp** application accepts csv files and, by default, it uses the
comma as the field separator and the period as the decimal separator. 
The first line of the file must contain variable names.

When you open the csv file with a text editor it should look like this:

<figure>
<img src="man/figures/README-1FRApp.png" alt="Data example screenshot" />
<figcaption aria-hidden="true">


You can specify (up to three) levels of the hierarchical structure by

indicating the highest one in the first column and the others in the
next columns. In detail, in our example, for data on spines nested in

neurons and neurons nested in cultures, the first level is the culture,
the second is the neuron, and the third is the spine.

The experimental condition is indicated in our example as *Genetic
condition* and applies to the neuron level in the FRAP experiments on
cultures.

The remaining columns include time of the recovery and the corresponding
value of intensity recovery, which is our dependent variable, y.

To recap, in our example, each row considers the intensity recovery at a
predefined combination of culture, neuron, spine, and time, collected
under a prespecified experimental condition.

## How to import data

Within the **Mixed Effects Model** tab, the *Browse…* button allows you
to load your own data file to be analyzed. Check that your import is correct by looking at the
summary description of the variables and at the number of observations imported on the right.

The response variable must be numeric (num) but the explanatory variables can be either integer (int) or numeric (num). *Factor* variable type is not allowed. Double-check variable type and/or variable names in the original data file.

Alternatively, to showcase the functionality and usability of **FRApp**, the *Load example data* button allows you to load the dataset analyzed in the paper and automatically selects the specification of the final
selected model.

## How to specify the model

A full model specification requires specification of response and explanatory variables within the nested data structure subsumed by random effects.

Among explanatory variables, fixed effects can be considered with the experimental condition (indicated as *Genetic condition*) being the main effect of interest in the data analyzed in the paper.

### Response and explanatory variables

Select the response variable (e.g., y) and the explanatory variable (e.g., time) by name from the drop-down menu.

### Hierarchical structure and random effects

In the same way, select the (up to three) levels of the hierarchical structure starting from the highest. For each specified hierarchical level, you must select at least one random effect.

### Variance and autocorrelation functions

The variance section allows you to specify different variance functions to model the heteroscedasticity of the error term. The default function is *Constant*, which represents the assumption of constant error variance.

The temporal dependence section allows you to select the order of an autoregressive model on the error term. The default value is 0, which corresponds to the uncorrelated errors structure.

### Interactions with experimental conditions

The last section allows you to specify potential interactions involving the experimental condition (name selected from the drop-down menu) and other model parameters, including those of:

- the fixed effects

- the random effects available at each specified hierarchical level

- the variance function.

## How to fit the model

The *Fit the model* button allows you to estimate the model. At the end

of the estimation process, the following options will appear on the
right side

of the application:

- summary information on the model

- scatterplot of residuals vs. estimated values

- quantile-quantile plot of the residuals

- approximate 95% confidence intervals of the parameters of interest.

The *Download report* button allows you to export the results printed in

the application (model summary, residual graphs, and intervals) into a

PDF document.

The *Download RData* button allows you to export an RData file

containing six objects:

- data: the dataset in the format used for the analysis

- fit: the output of the estimated model

- pred: the values estimated by the model

- CI: the approximate 95% confidence intervals

- resid: the residuals of the model

- raneff: the random effects available at the different hierarchical
  levels.

You can open the Rdata file with RStudio, and use related objects to
create

further graphs (e.g., curves estimated at different hierarchical levels,
residual or random

effects graphs).

In the last section, it is possible to specify a name for the estimated

model and, with the *Add to model list* button, save it in the list of

models to be compared within the model selection procedure. Saved models
must have different names. The \*Reset

model list\* button allows you to delete the saved models from the list.

## How to compare fitted models

The Compare tab allows you to compare the saved models by selecting
their name from the drop-down menu. Once the models

are selected from the list, the table with information criteria and
likelihood ratio test results is provided.

## References

As a reference to the construction of the model and the different

options to specify, we refer to the book: Pinheiro, J., & Bates, D.

(2006). Mixed-effects models in S and S-PLUS. Springer science &

business media.
