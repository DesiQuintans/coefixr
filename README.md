
- [About `coefixr`](#about-coefixr)
- [Installation](#installation)
- [Package contents](#package-contents)
- [Package manual](#package-manual)
- [Worked example](#worked-example)
- [Details re. the calculations
  performed](#details-re-the-calculations-performed)
  - [Adjusted interaction
    coefficients](#adjusted-interaction-coefficients)
  - [Adjusted standard error (SE)](#adjusted-standard-error-se)
  - [Adjusted 95% confidence interval](#adjusted-95-confidence-interval)
  - [Comparison to package output](#comparison-to-package-output)
- [Advanced usage](#advanced-usage)
  - [Variables of interest](#variables-of-interest)

<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- PDF manual is created in terminal with:  R CMD Rd2pdf . -o coefixr.pdf -->

<!-- TOC start (generated with https://github.com/derlin/bitdowntoc) -->

- [About `coefixr`](#about-coefixr)
- [Installation](#installation)
- [Package contents](#package-contents)
- [Package manual](#package-manual)
- [Worked example](#worked-example) + [Automated calculation with
  `adjust_interaction_model()`](#automated-calculation-with-adjust_interaction_model)
- [Details re. the calculations
  performed](#details-re-the-calculations-performed)
  - [Adjusted interaction
    coefficients](#adjusted-interaction-coefficients)
  - [Adjusted standard error (SE)](#adjusted-standard-error-se)
  - [Adjusted 95% confidence interval](#adjusted-95-confidence-interval)
  - [Comparison to package output](#comparison-to-package-output)
- [Advanced usage](#advanced-usage)
  - [Variables of interest](#variables-of-interest)

<!-- TOC end -->

# About `coefixr`

<!-- badges: start -->

<!-- badges: end -->

`coefixr` adjusts the coefficients and confidence intervals associated
with interaction terms in models, including interactions that involve
the reference levels of Factors. It should work with a variety of
models.

# Installation

You can install the development version of `coefixr` from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("DesiQuintans/coefixr")
```

It is not currently on CRAN, nor are there plans to release it there.

# Package contents

| Function | Description |
|:---|:---|
| `adjust_interaction_model()` | Takes a model and returns a dataframe of adjusted coefficients and confidence intervals. |
| `cancer_modified` | A built-in dataset for testing. |

# Package manual

<https://github.com/DesiQuintans/coefixr/blob/main/coefixr.pdf>

# Worked example

Here is an example of fitting a linear model with an interaction on a
dataset that is included with `coefixr`.

``` r
library(coefixr)

# cancer_modified is a dataset provided with the `coefixr` package.
str(cancer_modified)
#> 'data.frame':    228 obs. of  10 variables:
#>  $ inst     : Factor w/ 3 levels "Sites 01-10",..: 1 1 1 1 1 2 1 2 1 1 ...
#>  $ time     : num  306 455 1010 210 883 ...
#>  $ status   : num  2 2 1 2 2 1 2 2 2 2 ...
#>  $ age      : num  74 68 56 57 60 74 68 71 53 61 ...
#>  $ sex      : Factor w/ 2 levels "Female","Male": 2 2 2 2 2 2 1 1 2 2 ...
#>  $ ph.ecog  : Factor w/ 3 levels "Asymptomatic",..: 2 1 1 2 1 2 3 3 2 3 ...
#>  $ ph.karno : num  90 90 90 90 100 50 70 60 70 70 ...
#>  $ pat.karno: num  100 90 90 60 90 80 60 80 80 70 ...
#>  $ meal.cal : num  1175 1225 NA 1150 NA ...
#>  $ wt.loss  : num  NA 15 15 11 0 0 10 1 16 34 ...

Map(levels, Filter(is.factor, cancer_modified))
#> $inst
#> [1] "Sites 01-10" "Sites 11-20" "Sites 20-33"
#> 
#> $sex
#> [1] "Female" "Male"  
#> 
#> $ph.ecog
#> [1] "Asymptomatic"                         
#> [2] "Symptomatic but completely ambulatory"
#> [3] "Not completely ambulatory"
```

This is the example model. It has two interactions: one between two
Factors (`sex * ph.ecog`) and another between a Factor and a Numeric
(`sex * wt.loss`).

``` r
my_model <- 
    lm(status ~ inst + age + sex * ph.ecog + sex * wt.loss, 
       data = cancer_modified)

summary(my_model)$coefficients
#>                                                          Estimate  Std. Error    t value     Pr(>|t|)
#> (Intercept)                                           1.143486254 0.214009307  5.3431613 2.464046e-07
#> instSites 11-20                                      -0.153189909 0.065657611 -2.3331630 2.062831e-02
#> instSites 20-33                                      -0.208429891 0.081715078 -2.5506907 1.149539e-02
#> age                                                   0.004164404 0.003333512  1.2492543 2.130249e-01
#> sexMale                                               0.471133734 0.114185200  4.1260490 5.402737e-05
#> ph.ecogSymptomatic but completely ambulatory          0.415336393 0.108288785  3.8354516 1.677053e-04
#> ph.ecogNot completely ambulatory                      0.465297213 0.134264731  3.4655207 6.469179e-04
#> wt.loss                                              -0.003792171 0.003524417 -1.0759710 2.832300e-01
#> sexMale:ph.ecogSymptomatic but completely ambulatory -0.434351990 0.140825777 -3.0843216 2.327118e-03
#> sexMale:ph.ecogNot completely ambulatory             -0.289047417 0.173924212 -1.6619159 9.808840e-02
#> sexMale:wt.loss                                       0.002719025 0.004616528  0.5889762 5.565386e-01
```

### Automated calculation with `adjust_interaction_model()`

We can use `adjust_interaction_model()` to adjust the coefficients and
confidence intervals of the interactions. For details of those
adjustments, see the [Calculations
section](#details-re-the-calculations-performed).

Here, I request non-exponentiated (i.e. log) coefficients, a global
Type-II p-value for each variable, and output formatted to 2 decimal
places for coefficients and 3 decimals places for p-values.

``` r
adjust_interaction_model(
    modelobj     = my_model, 
    data         = cancer_modified, 
    exponentiate = FALSE,
    add.global.p = TRUE,
    intercept    = TRUE,
    digits.n     = 2, 
    digits.p     = 3,
    global_args  = list(type = "II")
)
#>                                                     covar   adj.with is.top is.intx   ref ref.intx global.p p.value ci.95lwr  coef ci.95upr
#> 1                                             (Intercept) Unadjusted   TRUE   FALSE FALSE    FALSE     <NA>  <0.001     0.72  1.14     1.56
#> 2                                                    inst Unadjusted   TRUE   FALSE FALSE    FALSE    0.014    <NA>     <NA>  <NA>     <NA>
#> 3                                         instSites 01-10 Unadjusted  FALSE   FALSE  TRUE    FALSE     <NA>    <NA>     0.00  0.00     0.00
#> 4                                         instSites 11-20 Unadjusted  FALSE   FALSE FALSE    FALSE     <NA>   0.021    -0.28 -0.15    -0.02
#> 5                                         instSites 20-33 Unadjusted  FALSE   FALSE FALSE    FALSE     <NA>   0.011    -0.37 -0.21    -0.05
#> 6                                                     age Unadjusted   TRUE   FALSE FALSE    FALSE    0.213   0.213    -0.00  0.00     0.01
#> 7                                                     sex Unadjusted   TRUE   FALSE FALSE    FALSE   <0.001    <NA>     <NA>  <NA>     <NA>
#> 8                                               sexFemale Unadjusted  FALSE   FALSE  TRUE    FALSE     <NA>    <NA>     0.00  0.00     0.00
#> 9                                                 sexMale Unadjusted  FALSE   FALSE FALSE    FALSE     <NA>  <0.001     0.25  0.47     0.69
#> 10                                                ph.ecog Unadjusted   TRUE   FALSE FALSE    FALSE    0.003    <NA>     <NA>  <NA>     <NA>
#> 11                                    ph.ecogAsymptomatic Unadjusted  FALSE   FALSE  TRUE    FALSE     <NA>    <NA>     0.00  0.00     0.00
#> 12           ph.ecogSymptomatic but completely ambulatory Unadjusted  FALSE   FALSE FALSE    FALSE     <NA>  <0.001     0.20  0.42     0.63
#> 13                       ph.ecogNot completely ambulatory Unadjusted  FALSE   FALSE FALSE    FALSE     <NA>  <0.001     0.20  0.47     0.73
#> 14                                                wt.loss Unadjusted   TRUE   FALSE FALSE    FALSE    0.337   0.283    -0.01 -0.00     0.00
#> 15                                            sex:ph.ecog  All terms   TRUE    TRUE FALSE    FALSE    0.010    <NA>     <NA>  <NA>     <NA>
#> 16                          sexFemale:ph.ecogAsymptomatic  All terms  FALSE    TRUE FALSE     TRUE     <NA>    <NA>     0.00  0.00     0.00
#> 17 sexFemale:ph.ecogSymptomatic but completely ambulatory  All terms  FALSE    TRUE FALSE     TRUE     <NA>    <NA>     0.20  0.42     0.63
#> 18             sexFemale:ph.ecogNot completely ambulatory  All terms  FALSE    TRUE FALSE     TRUE     <NA>    <NA>     0.20  0.47     0.73
#> 19                            sexMale:ph.ecogAsymptomatic  All terms  FALSE    TRUE FALSE     TRUE     <NA>    <NA>     0.25  0.47     0.69
#> 20   sexMale:ph.ecogSymptomatic but completely ambulatory  All terms  FALSE    TRUE FALSE    FALSE     <NA>   0.002     0.24  0.45     0.66
#> 21               sexMale:ph.ecogNot completely ambulatory  All terms  FALSE    TRUE FALSE    FALSE     <NA>   0.098     0.39  0.65     0.90
#> 22                                            sex:wt.loss  All terms   TRUE    TRUE FALSE    FALSE    0.557    <NA>     <NA>  <NA>     <NA>
#> 23                                      sexFemale:wt.loss  All terms  FALSE    TRUE FALSE     TRUE     <NA>    <NA>    -0.01 -0.00     0.00
#> 24                                        sexMale:wt.loss  All terms  FALSE    TRUE FALSE    FALSE     <NA>   0.557     0.25  0.47     0.69
```

`adjust_interaction_model()` returns a data frame with these columns:

- **covar**
  - The covariate.
- **adj.with**
  - Terms included in the adjustment of interactions. `Unadjusted` for
    non-interactions. See [Advanced Usage](#variables-of-interest) for
    more details.
- **is.top**
  - `TRUE` marks top-level covariates (i.e. names used in the model
    formula).
- **is.intx**
  - `TRUE` marks all interactions.
- **ref**
  - `TRUE` marks the reference levels of covariates.
- **ref.intx**
  - `TRUE` marks interactions that involve the reference level of a
    covariate.
- **global.p**
  - The global p-value of the covariate. Column is omitted if
    `add.global.p = FALSE`.
- **p.value**
  - The p-value of the covariate. P-values are inherited from the
    model’s `summary()` method and not recalculated.
- **ci.95lwr** or **exp_ci.95lwr**
  - Lower 95% confidence interval of the coefficient. If
    `exponentiate = TRUE`, the column’s name is changed and the contents
    are exponentiated.
- **coef** or **exp_coef**
  - The coefficient. If `exponentiate = TRUE`, the column’s name is
    changed and the contents are exponentiated.
- **ci.95upr** or **exp_ci.95upr**
  - Upper 95% confidence interval of the coefficient. If
    `exponentiate = TRUE`, the column’s name is changed and the contents
    are exponentiated.

# Details re. the calculations performed

## Adjusted interaction coefficients

``` r
summary(my_model)$coefficients
#>                                                          Estimate  Std. Error    t value     Pr(>|t|)
#> (Intercept)                                           1.143486254 0.214009307  5.3431613 2.464046e-07
#> instSites 11-20                                      -0.153189909 0.065657611 -2.3331630 2.062831e-02
#> instSites 20-33                                      -0.208429891 0.081715078 -2.5506907 1.149539e-02
#> age                                                   0.004164404 0.003333512  1.2492543 2.130249e-01
#> sexMale                                               0.471133734 0.114185200  4.1260490 5.402737e-05
#> ph.ecogSymptomatic but completely ambulatory          0.415336393 0.108288785  3.8354516 1.677053e-04
#> ph.ecogNot completely ambulatory                      0.465297213 0.134264731  3.4655207 6.469179e-04
#> wt.loss                                              -0.003792171 0.003524417 -1.0759710 2.832300e-01
#> sexMale:ph.ecogSymptomatic but completely ambulatory -0.434351990 0.140825777 -3.0843216 2.327118e-03
#> sexMale:ph.ecogNot completely ambulatory             -0.289047417 0.173924212 -1.6619159 9.808840e-02
#> sexMale:wt.loss                                       0.002719025 0.004616528  0.5889762 5.565386e-01
```

Coefficients involved in interactions are adjusted by adding the
coefficient of the interaction and all of its components together.
Reference levels of Factors are set to 0.

For example, the interaction of
`sexMale:ph.ecogNot completely ambulatory` involves:

- `sexMale` ($0.471133734$)
- `ph.ecogNot completely ambulatory` ($0.465297213$)
- `sexMale:ph.ecogNot completely ambulatory` ($-0.289047417$)

$$0.471133734 + 0.465297213 + -0.289047417 = 0.6473835$$

## Adjusted standard error (SE)

``` r
covm <- vcov(my_model)
covm[upper.tri(covm, diag = FALSE)] <- NA

covm
#>                                                        (Intercept) instSites 11-20 instSites 20-33           age       sexMale ph.ecogSymptomatic but completely ambulatory ph.ecogNot completely ambulatory       wt.loss sexMale:ph.ecogSymptomatic but completely ambulatory sexMale:ph.ecogNot completely ambulatory sexMale:wt.loss
#> (Intercept)                                           4.579998e-02              NA              NA            NA            NA                                           NA                               NA            NA                                                   NA                                       NA              NA
#> instSites 11-20                                      -1.825811e-03    4.310922e-03              NA            NA            NA                                           NA                               NA            NA                                                   NA                                       NA              NA
#> instSites 20-33                                      -1.158165e-03    2.011808e-03    6.677354e-03            NA            NA                                           NA                               NA            NA                                                   NA                                       NA              NA
#> age                                                  -6.513531e-04    1.030395e-05   -4.224510e-06  1.111230e-05            NA                                           NA                               NA            NA                                                   NA                                       NA              NA
#> sexMale                                              -3.873201e-03   -8.543543e-04   -8.302885e-04 -5.086312e-05  1.303826e-02                                           NA                               NA            NA                                                   NA                                       NA              NA
#> ph.ecogSymptomatic but completely ambulatory         -5.541673e-03   -8.964428e-04   -1.423555e-03 -1.060507e-05  6.860447e-03                                 1.172646e-02                               NA            NA                                                   NA                                       NA              NA
#> ph.ecogNot completely ambulatory                      6.404868e-04   -1.447970e-03   -1.524366e-03 -1.154527e-04  7.501756e-03                                 7.516018e-03                     1.802702e-02            NA                                                   NA                                       NA              NA
#> wt.loss                                              -7.093004e-05   -6.042435e-07    3.278817e-05  4.775330e-08  6.068152e-05                                -5.590635e-05                    -4.006172e-05  1.242152e-05                                                   NA                                       NA              NA
#> sexMale:ph.ecogSymptomatic but completely ambulatory  5.206499e-03    8.818351e-04    1.649619e-03  1.582929e-05 -1.168061e-02                                -1.176932e-02                    -7.603125e-03  5.730190e-05                                         1.983190e-02                                       NA              NA
#> sexMale:ph.ecogNot completely ambulatory              1.118253e-03    1.669698e-03    1.476544e-03  8.467373e-05 -1.191112e-02                                -7.507006e-03                    -1.775626e-02  3.887027e-05                                         1.304107e-02                             0.0302496314              NA
#> sexMale:wt.loss                                       7.651555e-05    4.719507e-06   -6.726917e-06 -2.232767e-07 -1.224352e-04                                 5.095555e-05                     3.661753e-05 -1.228432e-05                                        -9.507685e-05                            -0.0001188322    2.131233e-05
```

The standard error (SE) is adjusted using
$\sqrt{\sum\text{variances} + 2(\sum\text{covariances})}$ for all terms
involved in the interaction. The variance-covariance table is accessed
using `stats::vcov()`. Terms that do not appear in the `vcov()` matrix
(namely, reference levels and interactions involving reference levels)
are set to 0. For example, using `vcov(my_model)`, the interaction of
`sexMale:ph.ecogNot completely ambulatory` involves:

- The variances
  - `sexMale` ($0.01303826$)
  - `ph.ecogNot completely ambulatory` ($0.01802702$)
  - `sexMale:ph.ecogNot completely ambulatory` ($0.03024963$)
- The covariances
  - `sexMale` vs `ph.ecogNot completely ambulatory` ($0.007501756$)
  - `sexMale` vs `sexMale:ph.ecogNot completely ambulatory`
    ($-0.01191112$)
  - `ph.ecogNot completely ambulatory` vs
    `sexMale:ph.ecogNot completely ambulatory` ($-0.01775626$)

$$\sqrt{0.01303826 + 0.01802702 + 0.03024963 + 2(0.007501756 + -0.01191112 + -0.01775626)} = 0.1303214$$

## Adjusted 95% confidence interval

The adjusted confidence interval is calculated by subtracting/adding
$1.96 \times SE$ from the adjusted coefficient. Again, for the
interaction of `sexMale:ph.ecogNot completely ambulatory` which has an
adjusted coefficient of $0.6473835$ and an adjusted SE of $0.1303214$:

$$\text{Lower CI} = 0.6473835 - 1.96 \times 0.1303214 = 0.3919536$$
$$\text{Upper CI} = 0.6473835 + 1.96 \times 0.1303214 = 0.9028134$$

## Comparison to package output

``` r
res <- adjust_interaction_model(my_model, cancer_modified, digits.n = Inf)

res[which(res$covar == "sexMale:ph.ecogNot completely ambulatory"),
    c("covar", "ci.95lwr", "coef", "ci.95upr")]
#>                                       covar  ci.95lwr      coef  ci.95upr
#> 17 sexMale:ph.ecogNot completely ambulatory 0.3919536 0.6473835 0.9028135
```

# Advanced usage

## Variables of interest

If you want the coefficients, SEs, and confidence intervals of your
interactions to be adjusted only using some variable(s) of interest, you
can specify this through the `interest` argument.

For example, adjustment of the interaction of
`sexMale:ph.ecogNot completely ambulatory` in the worked example uses
the coefficients/variances/covariances of these three terms:

- `sexMale`
- `ph.ecogNot completely ambulatory`
- `sexMale:ph.ecogNot completely ambulatory`

If you only want to look at the effect of sex in the interaction, then
running:

``` r
adjust_interaction_model(
    modelobj = my_model, 
    data     = cancer_modified, 
    interest = "sex"
)
```

would perform the adjustment using any term that matches the regular
expression `sex` and ignoring all others. Which is to say, it would use

- `sexMale`
- `sexMale:ph.ecogNot completely ambulatory`

And not use the fixed effect of `ph.ecogNot completely ambulatory`
because it does not match.

Because `interest` is a case-sensitive regular expression, you have a
lot of flexibility about what you want to specify, like telling it that
you’d like to adjust by any term that first matches “ecog” and then
later matches “ymp”, which means that you would be adjusting with
`ph.ecogAsymptomatic` or `ph.ecogSymptomatic but completely ambulatory`,
but NOT `ph.ecogNot completely ambulatory`:

``` r
adjust_interaction_model(
    modelobj = my_model, 
    data     = cancer_modified, 
    interest = "ecog.*?ymp"
)
```

It’s left to the user to know what they are asking for and why.
