# databraryr <img src="man/figures/logo.png" align="right" height="139" alt="" />

This repository contains code for the `databraryR` R package.

## Documentation

Full package documentation built using `pkgdown` can be found [here](https://databrary.github.io/databraryr).

## Installation (development release)

### Official CRAN release

- Install the package from CRAN via `install.packages("databraryr")`.

### Development release

- Install the `devtools` package from CRAN: `install.packages("devtools")` if you have not already done so.
- Load `devtools` into your local environment: `library(devtools)`
- Install the `databraryr` package via `install_github("databrary/databraryr")`. Required dependencies will be installed at this time.

## Use

The package is under active development, as is the documentation.
Running `devtools::install_github("databrary/databraryr", force=TRUE)` regularly to get updates is strongly recommended.

### Databrary credentials

Databrary ([databrary.org](https://databrary.org)) is a research data library specialized for storing and sharing video with capabilities of storing [other types](https://nyu.databrary.org/asset/formats) of associated data.
Access to restricted data requires [registration](https://databrary.org/user/register) and formal approval by an institution.
The registration process involves the creation of an (email-account-based) user account and secure password.
Once institutional authorization has been granted, a user may gain access to shared video, audio, and other data.
Many commands in the `databraryr` package return meaningful results *without* or *prior to* formal authorization.
