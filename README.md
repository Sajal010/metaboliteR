# metaboliteR <img src="https://gitlab.com/metaboliter/metaboliter/raw/master/inst/shinyApp/www/logo.png" height="132" width="110" align="right"/>

[![pipeline status](https://gitlab.com/metaboliter/metaboliter/badges/master/pipeline.svg)](https://gitlab.com/metaboliter/metaboliter/pipelines)
[![coverage report](https://gitlab.com/metaboliter/metaboliter/badges/master/coverage.svg)](https://metaboliter.gitlab.io/metaboliter/coverage)

## Overview

metaboliteR is a R package that provides few Probabilistic PCA (PPCA) methods [(Nyamundanda G. et al. 2010)](http://hdl.handle.net/10197/2835) to analyse metabolomic data. PPCA is an extension of the PCA method with probabilistic basis which offers quantification of statistical uncertainty in the model. While PCA uses orthogonal transformation to obtain Principal Components (PC) scores, PPCA assumes a multivariate Gaussian distribution on the Principal Components scores and estimate the model parameter through Expectation-Maximisation (EM) algorithm. There are also further extension with PPCCA (including covariates), MPPCA (accounting groupings) and DPPCA (different time points) [(Nyamundanda G. et al. 2014)](http://hdl.handle.net/10197/7107). 

In addition, metaboliteR also incorporates a method of estimating sample size for metabolomic experiments introduced by [(Nyamundanda G. et al. 2013)](http://hdl.handle.net/10197/5043).

metaboliteR also has a graphical user interface (GUI) application through the use of Shiny. This can be accessed through [here]() or the image below:

[<img src="https://gitlab.com/metaboliter/metaboliter/raw/master/inst/shinyApp/www/app-logo.png" height="90" width="179"/>](https://metaboliter.shinyapps.io/metaboliter/)
[<img src="https://gitlab.com/metaboliter/metaboliter/raw/master/inst/shinyApp/www/webpage-logo.png" height="90" width="179"/>](https://metaboliter.gitlab.io/metaboliter)


## Installation

```r
# Install from CRAN (To be published.. )
install.packages("metaboliteR")

# Or the development version from Gitlab
# install.packages("devtools")
devtools::install_gitlab("metaboliteR/metaboliteR")
```

You can also clone this repositary and install it locally.
However, make sure to install dependencies in your computer for the package to work.

Run the following code: 
`remotes::install_local(dependencies = TRUE)`

## Quick Guide


<img src="https://gitlab.com/metaboliter/metaboliter/raw/master/inst/shinyApp/www/Homepage.jpg" height="600" width="450"/>


## References




### Websites (removing later)

[Original PPCA function repositary](https://gitlab.com/metabol/ppca)

[GitLab](https://gitlab.com/metaboliter/metaboliter)

[Webpage](https://metaboliter.gitlab.io/metaboliter)

ShinyApp: can only host app when package is available in CRAN



