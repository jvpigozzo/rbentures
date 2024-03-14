
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rbentures

<!-- badges: start -->
<!-- badges: end -->

A debenture is a type of debt security issued by a company to raise
capital from investors, promising to pay a fixed rate of interest over a
specified period of time. **debentures.com.br** ([debentures](http://www.debentures.com.br/)) is a Brazilian website that
provides information and resources related to debentures. In its website
we can find a vast number of datasets regarding prices and transactions
for debentures available such as:

- Future Events (coupon, amortization etc)
- Issuing Price
- Historical Prices
- Duration

Package rbentures uses webscraping tools to download and read these
datasets from **debentures.com.br**, making it easy to consume it in R
in a structured way.

## Installation

You can install rbentures from
[GitHub](https://github.com/) with:

``` r
devtools::install_github("jvpigozzo/rbentures")
```

To install the package locally, first build the package and then install it:

``` r
devtools::build()
devtools::install()
```
