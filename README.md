
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GoFluxYourself: A user-friendly way to calculate GHG fluxes yourself, regardless of user experience<img src="man/figures/GoFluxYourself.png" align="right" width="200"/>

#### BETA VERSION

The package is ready to use, but errors may still occur. Please report
any issues to the maintainer, Karelle Rheault (<karh@ign.ku.dk>),
including script and raw data if necessary. Thank you for helping me in
the development of this tool! 🙏

### One Package to rule them all

Non-steady state chambers are widely used for measuring soil greenhouse
gas (GHG) fluxes, such as CO<sub>2</sub>, CH<sub>4</sub>,
N<sub>2</sub>O, NH<sub>3</sub>, CO, and water vapor (H<sub>2</sub>O).
While linear regression (LM) is commonly used to estimate GHG fluxes,
this method tends to underestimate the pre-deployment flux
(*f*<sub>0</sub>). Indeed, a non-linearity is expected when gas
concentration increases inside a closed chamber, due to changes in
diffusion gradients between the soil and the air inside the chamber. In
addition, lateral gas flow and leakage contribute to non-linearity. Many
alternative to LM have been developed to try and provide a more accurate
estimation of *f*<sub>0</sub>, for instance the method of [Hutchinson
and Mosier (HM)
(1981)](https://doi.org/10.2136/sssaj1981.03615995004500020017x), which
was implemented in the [`HMR`](https://cran.r-project.org/package=HMR)
package by [Pedersen et al.,
2010](https://doi.org/10.1111/j.1365-2389.2010.01291.x). However,
non-linear models have a tendency to largely overestimate some flux
measurements, due to an exaggerated curvature. Therefore, the user is
expected to decide which method is more appropriate for each flux
estimate. With the advent of portable greenhouse gas analyzers
(e.g. [Los Gatos Research (ABB) laser gas
analyzers](https://new.abb.com/products/measurement-products/analytical/laser-gas-analyzers/laser-analyzers/lgr-icos-portable-analyzers)),
soil GHG fluxes have become much easier to measure, and more efficient
ways to calculate these flux estimates are needed in order to process
large amounts of data. A recent approach was developed by [Hüppi et al.,
2018](https://doi.org/10.1371/journal.pone.0200876) to restrict the
curvature in the HM model for a more reliable flux estimate. In the HM
model, the curvature is controlled by the kappa parameter. Hüppi et
al. suggest the use of the kappa.max to limit the maximal curvature
allowed in the model (see their R package
[`gasfluxes`](https://cran.r-project.org/package=gasfluxes), available
on the CRAN). This procedure introduces less arbitrary decisions in the
flux estimation process.

Previous software that have been developed to calculate GHG fluxes were
limited in many aspects that this package is meant to overcome. Most
were limited to the linear regression approach (e.g. [LI-COR
SoilFluxPro](https://www.licor.com/env/products/soil-flux/soilfluxpro),
[Flux
Puppy](https://www.sciencedirect.com/science/article/pii/S0168192319301522),
and the R packages [`flux`](https://cran.r-project.org/package=flux) and
[`FluxCalR`](https://github.com/junbinzhao/FluxCalR)), others were
compatible with only one designated system (e.g. [LI-COR
SoilFluxPro](https://www.licor.com/env/products/soil-flux/soilfluxpro),
[Flux
Puppy](https://www.sciencedirect.com/science/article/pii/S0168192319301522)),
and some were impractical with large amounts of measurements (e.g. the R
package [`HMR`](https://cran.r-project.org/package=HMR)) or simply did
not include data pre-processing (e.g. the R packages
[`HMR`](https://cran.r-project.org/package=HMR),
[`flux`](https://cran.r-project.org/package=flux) and
[`gasfluxes`](https://cran.r-project.org/package=gasfluxes)).

This new R package, `GoFluxYourself` is meant to be “student proof”,
meaning that no extensive knowledge or experience is needed to import
raw data into R, chose the best model to calculate fluxes (LM or HM),
quality check the results objectively and obtain high quality flux
estimates. The package contains a wide range of functions that allows
the user to import raw data from a variety of instruments (LI-COR, LGR,
GAIA2TECH, Gasmet and Picarro); calculate fluxes from a variety of GHG
(CO<sub>2</sub>, CH<sub>4</sub>, N<sub>2</sub>O, NH<sub>3</sub>, CO and
H<sub>2</sub>O) with both linear (LM) and non-linear (HM) flux
calculation methods; align instruments clocks after data import;
interactively identify measurements (start and end time) if there are no
automatic chamber recordings (e.g. LI-COR smart chamber); plot
measurements for easy visual inspection; and quality check the
measurements based on objective criteria and non-arbitrary thresholds.

> *Three R packages for the Elven-kings under the CRAN,  
> Seven for the Dwarf-lords in their halls of open software,  
> Nine for Mortal Men doomed to write scripts themselves,  
> One for the Dark Lady on her dark throne  
> In the Land of GitHub where the Shadows lie.  
> One Package to rule them all, One Package to find them,  
> One Package to bring them all and in the darkness bind them  
> In the Land of GitHub where the Shadows lie.*

## About the package

This package `GoFluxYourself` is meant to be “student proof”, meaning
that no extensive knowledge or experience is needed to import raw data
into R (except for knowing how to use R, of course), chose the best
model to calculate fluxes (LM or HM, that is the question. -Shakespeare,
1603), quality check the results objectively (hence the no experience
needed) and obtain high quality flux estimates from static chamber
measurements (wonderful!).

### Import and measurement identification

The package contains a wide range of functions that let the user import
raw data from a variety of instruments:

- [**LI-COR trace gas
  analyzers**](https://www.licor.com/env/products/trace-gas/): LI-7810
  for CO<sub>2</sub>, CH<sub>4</sub> and H<sub>2</sub>O, LI-7820 for
  N<sub>2</sub>O and H<sub>2</sub>O
- [**LI-COR Smart
  Chamber**](https://www.licor.com/env/products/soil-flux/survey): for
  an easy import of data from any gas analyzer
- [**Los Gatos Research (ABB) laser gas
  analyzers**](https://new.abb.com/products/measurement-products/analytical/laser-gas-analyzers/laser-analyzers/lgr-icos-portable-analyzers):
  Ultra-portable Greenhouse Gas Analyzer (UGGA) and Microportable
  Greenhouse Gas Analyzer (MGGA) for CO<sub>2</sub>, CH<sub>4</sub> and
  H<sub>2</sub>O
- [**Picarro G2508 gas
  analyzer**](https://www.picarro.com/g2508_gas_concentration_analyzer):
  for CO<sub>2</sub>, CH<sub>4</sub>, N<sub>2</sub>O, NH<sub>3</sub> and
  H<sub>2</sub>O
- [**GAIATECH Automated ECOFlux
  chamber**](https://www.dmr.eu/technologies/gas-emission-measurements-eco2flux/automated-eco2flux-chamber/):
  for an easy import of data from any gas analyzer
- [**Gasmet DX4015 portable analyzer for humid
  conditions**](https://www.gasmet.com/products/category/portable-gas-analyzers/dx4015/):
  for CO, CO<sub>2</sub>, CH<sub>4</sub>, N<sub>2</sub>O, NH<sub>3</sub>
  and H<sub>2</sub>O.

After import, the user can chose from two methods for identification of
measurements:

- **Manual identification of measurements** - based on `start.time`,
  provided separately in an auxiliary file. The function `obs.win()`
  splits the imported data into a list of data frame (divided by
  `UniqueID`) and creates an observation window around the `start.time`
  to allow for a manual selection of the start and end points of each
  measurements, using the function `click.peak.loop()`.
- **Automatic selection of measurements** - based on automatic
  recordings of chamber opening and closing from an instrument such as
  the LI-COR Smart Chamber or the GAIATECH Automated ECOFlux chamber.

### Flux calculation

The function `goFlux()` calculates fluxes from a variety of greenhouse
gases (CO<sub>2</sub>, CH<sub>4</sub>, N<sub>2</sub>O, NH<sub>3</sub>,
CO, and H<sub>2</sub>O) using both linear (LM) and non-linear (HM;
[Hutchinson and Mosier,
1981](https://doi.org/10.2136/sssaj1981.03615995004500020017x)) flux
calculation methods. The HM model for the chamber concentration $C_t$ at
time $t > 0$ after deployment is given by:

$$\mathbf{Eqn~1}~~~~~~C_t = \varphi~+~(C_0 - \varphi)^{-~\kappa~t}$$

Where $\varphi$ is the assumed concentration of constant gas source
below the surface (also known as $C_i$), $C_0$ is the concentration in
the chamber at the moment of chamber closure and $\kappa$ (kappa)
determines the curvature of the model. A large kappa returns a strong
curvature.

A maximum threshold for this parameter, kappa-max ($k.max$), can be
calculated from the linear flux estimate ($LM.flux$), the minimal
detectable flux ($MDF$) and the time of chamber closure ($t$) ([Hüppi et
al., 2018](https://doi.org/10.1371/journal.pone.0200876)).

$$\mathbf{Eqn~2}~~~~~~k.max = \frac{LM.flux}{MDF~\times~t}$$

Where $LM.flux$ and $MDF$have the same units (nmol or
$\mu$mol$\cdot$m<sup>-2</sup>$\cdot$s<sup>-1</sup>) and $t$ is in
seconds. Therefore, the units of kappa-max is s<sup>-1</sup>. This limit
of kappa-max is included in the `goFlux()` function, so that the
non-linear flux estimate cannot exceed this maximum curvature. See below
for more details about the minimal detectable flux (MDF).

All flux estimates, including the MDF, are multiplied by a $flux.term$
which is used to correct for water vapor inside the chamber, as well as
convert the units to obtain a term in nmol or
$\mu$mol$\cdot$m<sup>-2</sup>$\cdot$s<sup>-1</sup>:
$$\mathbf{Eqn~3}~~~~~~flux.term = \frac{(1 - H_2O)~V~P}{A~R~T}$$Where
$H_2O$ is the water vapor in mmol$\cdot$mol<sup>-1</sup>, $V$ is the
volume inside the chamber in liters, $P$ is the pressure in kPa, $A$ is
the surface area inside the chamber in m<sup>2</sup>, $R$ is the
universal gas constant in
L$\cdot$kPa$\cdot$K<sup>-1</sup>$\cdot$mol<sup>-1</sup>. Each parameters
are measured inside the chamber at $t = 0$.

### Automatic selection of the best flux estimate

Following flux calculation, the user can select the best flux estimate
(LM or HM) based on objective criteria and non-arbitrary thresholds,
using the `best.flux()` function:

- **Assumed non-linearity**: If all criteria are respected, the best
  flux estimate is assumed to be the non-linear estimate from the
  Hutchinson and Mosier model.
- flux estimate.
- and kappa-max ($k.max$).
- on a selection of indices of model fit: MAE, RMSE, SE and AICc. In
  addition to the automatic selection of the best flux estimate based on
  these indices of model fit, measurements can be flagged “noisy” using
  these criteria.

In addition, the `best.flux()` function can flag measurements that are
below detection limit (MDF and *p-value*), out of bounds (intercept), or
too short (number of observations).

- is considered below the detection limit.
- flux estimate is considered statistically non-significant, and below
  the detection limit.
- models.
- for being too short.

By default, all criteria are included:
`criteria = c("MAE", "RMSE", "AICc", "SE", "g-factor", "kappa", "MDF", "nb.obs", "p-value", "intercept")`

#### **G-factor**

The g-factor is the ratio between the result of a non-linear flux
calculation model (e.g. Hutchinson and Mosier; HM) and the result of a
linear flux calculation model ([Hüppi et al.,
2018](https://doi.org/10.1371/journal.pone.0200876)).
$$\mathbf{Eqn~4}~~~~~~g-factor = \frac{HM.flux}{LM.flux}$$With the
`best.flux()` function, one can chose a limit at which the HM model is
deemed to overestimate (*f*<sub>0</sub>). Recommended thresholds for the
g-factor are \<4 for a flexible threshold, \<2 for a medium threshold,
or \<1.25 for a more conservative threshold. The default threshold is
`g.limit = 2`. If the g-factor is above the specified threshold, the
best flux estimate will switch to LM instead of HM. If `HM.flux/LM.flux`
is larger than `g.limit`, a warning is given in the columns
`HM.diagnose` and `quality.check`.

#### **Minimal Detectable Flux (MDF)**

The minimal detectable flux ($MDF$) is based on instrument precision
($prec$) and measurement time ($t$) ([Christiansen et al.,
2015](https://doi.org/10.1016/j.agrformet.2015.06.004)).
$$\mathbf{Eqn~5}~~~~~~MDF = \frac{prec}{t}~\times~flux.term$$Where the
instrument precision is in the same units as the measured gas (ppm or
ppb) and the measurement time is in seconds.

Below the MDF, the flux estimate is considered under the detection
limit, but not null. Therefore, the function will not return a 0. There
will simply be a warning in the columns `quality.check`, `LM.diagnose`
or `HM.diagnose` to warn of a flux estimate under the detectable limit.
No best flux estimate is chosen based on MDF.

#### **Kappa ratio**

The parameter kappa determines the curvature of the non-linear
regression in the Hutchinson and Mosier model, as shown in equation 1.
The limit of kappa-max, as calculated in equation 2, is included in the
`goFlux()` function, so that the non-linear flux estimate cannot exceed
this maximum curvature.

In the function `best.flux()`, one can choose the linear flux estimate
over the non-linear flux estimate based on the ratio between kappa and
kappa-max (`k.ratio`). The ratio is expressed as a percentage, where 1
indicates `HM.k = k.max`, and 0.5 indicates `HM.k = 0.5*k.max`. The
default setting is `k.ratio = 1`. If `HM.k/k.max` is larger than
`k.ratio`, a warning is issued in the columns `HM.diagnose` and
`quality.check`.

#### **Indices of model fit**

In the `best.flux()` function, we included multiple choices of indices
of model fit, described below. One can chose to include however many of
them in the function. If multiple of them are included, the selection of
the best model will be made based on a scoring system. Both models start
with a score of 0. For each criteria, whichever model performs worst is
given +1. After all selected criteria have been evaluated, the model
with the lowest score wins. In case of a tie, the non-linear flux
estimate is always chosen by default, as non-linearity is assumed.

##### **Mean Absolute Error (MAE) and Root Mean Square Error (RMSE)**

The mean absolute error (MAE) is the arithmetic mean of the absolute
residuals of a model, calculated as follows:
$$\mathbf{Eqn~6}~~~~~~MAE = \frac{\sum_{i = 1}^{n}{\lvert{y_i-\hat{y_i}}\rvert}}{n}$$Where
$y_i$ is the measured value, $\hat{y_i}$ is the predicted value and $n$
is the number of observations.

The root mean square error (RMSE) is very similar to the MAE. Instead of
using absolute errors, it uses squared errors, and the mean of the
squared errors is then rooted as follows:
$$\mathbf{Eqn~7}~~~~~~RMSE = \sqrt{\frac{\sum_{i = 1}^{n}{({y_i-\hat{y_i}})^2}}{n}}$$Because
of the squared errors, RMSE is sensitive to outliers. Indeed, a few
large errors will have a significant impact on the RMSE. Therefore, RMSE
will always be larger than or equal to MAE ([Pontius et al.,
2008](https://doi.org/10.1007/s10651-007-0043-y)).

Mathematically, RMSE is the standard deviation of the residuals:
$$\mathbf{Eqn~8}~~~~~~\sigma = \sqrt{\frac{\sum_{i = 1}^{N}{({x_i-\mu})^2}}{N}}$$Where
$x_i$ is the measured value, $N$ is the size of the population and $\mu$
is the population mean. The standard deviation is used to calculate the
precision of an instrument. In that case, $\mu$ is a known constant gas
concentration and $N$ is the number of observations.

Considering all of the above, MAE, RMSE and the standard deviation are
all measures of how much the data points are scattered around the true
mean or the regression. Therefore, MAE and RMSE can be compared to the
instrument precision to determine if a measurement is noisy. For a MAE
or RMSE larger than the instrument precision, the measurement is
considered to have more noise than normally expected.

If MAE is chosen as a criteria in `best.flux()`, the model with the
smallest MAE is chosen. If both models have a MAE smaller than the
instrument precision, the non-linear flux estimate is always chosen by
default, as non-linearity is assumed. When MAE is larger than the
instrument precision, a warning is given in the columns `quality.check`,
`LM.diagnose` or `HM.diagnose` to warn of a noisy measurement. RMSE
functions exactly the same was as MAE in the `best.flux()` function.

##### **Standard Error**

While the standard deviation tell about how the data points are
scattered around the true mean, the standard error of a measurement
tells how accurate that measurement is compared to the true population
mean ([Altman and Bland,
2005](https://doi.org/10.1136%2Fbmj.331.7521.903)). If considering the
standard deviation as used to calculate instrument precision (equation
8), the instrument standard error (instrument accuracy) is the standard
deviation divided by the square root of the number of observations:
$$\mathbf{Eqn~9}~~~~~~\sigma_\bar{x} = \frac{\sigma}{\sqrt{n}}$$Practically,
this means that a larger sample size increases the accuracy of a
measurement. In other words, if an instrument is imprecise and the
measurement has a lot of noise, it is still possible to get a very
accurate estimate of the true mean by increasing the number of
observations. With high-frequency GHG analyzers, that means increasing
the chamber closure time.

To calculate the standard error of a regression, one can use the delta
method (`deltamethod()` from the `msm` package), which propagates the
total error of the flux calculation for each parameter included in the
formula. The delta method approximates the standard error of a
regression $g(X)$ of a random variable $X = (x_1, x_2, ...)$, given
estimates of the mean and covariance matrix of $X$ ([Oehlert,
1992](https://doi.org/10.2307/2684406); [Mandel,
2013](https://doi.org/10.1080/00031305.2013.783880)).

In the function `best.flux()`, the standard error (SE) of the
measurements can be compared to the standard error of the instrument
($\sigma_\bar{x}$). If SE is chosen as a criteria in `best.flux()`, the
model with the smallest SE is chosen. If both models have a SE smaller
than the instrument precision, the non-linear flux estimate is always
chosen by default, as non-linearity is assumed. When SE is larger than
the instrument accuracy ($\sigma_\bar{x}$), a warning is given in the
columns `quality.check`, `LM.diagnose` or `HM.diagnose` to warn of a
noisy measurement.

##### **Akaike Information Criterion corrected for small sample size (AICc)**

The AIC estimates the relative quality of a statistical model and is
used to compare the fitting of different models to a set of data
([Akaike, 1974](https://doi.org/10.1109/TAC.1974.1100705)). Consider the
formula for AIC: $$\mathbf{Eqn~10}~~~~~~AIC = 2k - 2ln(\hat{L})$$Where
$k$ is the number of parameters in the model and $\hat{L}$ is the
maximized value of the likelihood function for the model. AIC deals with
the trade-off between the goodness of fit of a model and the simplicity
of the model. In other words, the AIC is a score that deals with both
the risk of underfitting and the risk of overfitting, and the model with
the lowest score has the best model fit.

In flux calculation, the linear model contains two parameters: the slope
and the intercept ($C_0$), whereas the Hutchinson and Mosier model
(equation 1) contains three parameters: the assumed concentration of
constant gas source below the surface ( $\varphi$ ), is the
concentration in the chamber at the moment of chamber closure ($C_0$)
and the curvature, kappa ($\kappa$). If both models have a very similar
fit (maximum likelihood), then the linear model would win because it has
less parameters. However, when the sample size is small (\<40
observations per parameter; i.e. \<120 observations when calculating
HM), there is an increased risk that AIC selects a model with too many
parameters. To address this risk of overfitting, AICc was developed
([Sugiura, 1978](https://doi.org/10.1080/03610927808827599)):
$$\mathbf{Eqn~11}~~~~~~AICc = AIC + \frac{2k^2 + 2k}{n - k - 1}$$Where
$n$ denotes the number of observations and $k$ the number of parameters
in the model.

If AICc is selected as a criteria in the `best.flux()` function, the
model with the lowest AICc wins.

#### **Intercept**

If the initial gas concentration (*C<sub>0</sub>*) calculated for the
flux estimates are more or less than 10% of the difference between
*C<sub>0</sub>* and the final gas concentration at the end of the
measurement (*C<sub>t</sub>*), a warning is issued in the columns
`quality.check`, `LM.diagnose` or `HM.diagnose` to warn of an intercept
out of bounds. Alternatively, one can provide boundaries for the
intercept, for example: `intercept.lim = c(380, 420)` for a true
*C<sub>0</sub>* of 400 ppm.

#### **Statistically significant flux (*p-value*)**

This criteria is only applicable to the linear flux. Under a defined
*p-value*, the linear flux estimate is deemed non-significant, i. e.,
flux under the detectable limit. The default threshold is
`p.val = 0.05`. No best flux estimate is chosen based on *p-value*. If
`LM.p.val < p.val`, a warning is given in the columns `quality.check`
and `LM.diagnose` to warn of an estimated flux under the detection
limit.

#### **Number of observations**

Limit under which a measurement is flagged for being too short
(`nb.obs < warn.length`). With nowadays’ portable greenhouse gas
analyzers, the frequency of measurement is usually one observation per
second. Therefore, for the default setting of `warn.length = 60`, the
chamber closure time should be approximately one minute (60 seconds). If
the number of observations is smaller than the threshold, a warning is
issued in the column `quality.check`.

### Visual inspection

Finally, after finding the best flux estimates, one can plot the results
and visually inspect the measurements using the function `flux.plot()`
and save the plots as pdf using `flux2pdf()`.

## How to use

Here is a simple example on how to use the package with a single raw
file from LGR gas analyzers.

> BETA VERSION
>
> The package is ready to use, but errors may still occur. Please report
> any issues to the maintainer, Karelle Rheault (<karh@ign.ku.dk>),
> including script and raw data if necessary. Thank you for helping me
> in the development of this tool! 🙏

### Installation

To install a package from GitHub, one must first install the package
`remotes` from the CRAN:

``` r
if (!require("remotes", quietly = TRUE)) install.packages("remotes")
```

Then, install the `GoFluxYourself` package from GitHub. If it is not the
first time you install the package, it must first be detached before
being updated.

> The package is actively being updated **every day**. To make sure that
> you are using the latest version, re-install the package every time
> you use it.

``` r
try(detach("package:GoFluxYourself", unload = TRUE), silent = TRUE)
remotes::install_github("Qepanna/GoFluxYourself")
```

**If prompted, it is recommended to update any pre-installed packages.**
The functioning of the package depends on many other packages
(`AICcmodavg`, `data.table`, `dplyr`, `ggnewscale`, `ggplot2`, `ggstar`,
`graphics`, `grDevices`, `grid`, `gridExtra`, `lubridate`, `minpack.lm`,
`msm`, `pbapply`, `plyr`, `purrr`, `rjson`, `rlist`, `SimDesign`,
`stats`, `stringr`, `tibble`, `tidyr`, `utils`), which will be installed
when installing `GoFluxYourself`.

Troubleshoot problems with `install_github()`:

- [Warning: package is in use and will not be
  installed](#warning-package-is-in-use-and-will-not-be-installed)
- [Error: API rate limit exceeded](#error-api-rate-limit-exceeded)

### Import raw data into R

``` r
library(GoFluxYourself)

# Get the example raw file from the package
file.path <- system.file("extdata", "LGR/LGR.txt", package = "GoFluxYourself")

# Import raw data from an LGR gas analyzer
?LGR_import
LGR_imp <- LGR_import(inputfile = file.path)
```

Note that raw data can also be imported from a multitude of other
instruments, and example data files are provided for all of them:

- LI-COR: LI-6400, LI-7810, LI-7820, LI-8100, LI-8200 (smart chamber)
- Los Gatos Research instruments: (e.g. UGGA and m-GGA)
- GAIA2TECH (DMR) automated chamber ECOFlux
- Picarro: G2508
- Gasmet: DX4015

To import multiple files from a folder, use the wrapper function
`import2RData()`.

``` r
# Get help for import functions from the GoFluxYourself package
?DX4015_import
?G2508_import
?GAIA_import
?LI6400_import
?LI7810_import
?LI7820_import
?LI8100_import
?LI8200_import
?import2RData
```

### Manual identification of measurements

In this example, the `start.time` for each measurement (`UniqueID`) was
noted manually in the field, and are provided in an auxiliary file
(`auxfile`).

``` r
# Other usefull packages
require(dplyr)
require(purrr)

# The auxfile requires start.time and UniqueID
# start.time must be in the format "%Y-%m-%d %H:%M:%S"
aux.path <- system.file("extdata", "LGR/LGR_aux.txt", package = "GoFluxYourself")
auxfile <- read.delim(aux.path) %>% 
  mutate(start.time = as.POSIXct(start.time, tz = "UTC"))
```

When running the function `click.peak()`, for each measurement, a window
will open, in which you must click on the start point and the end point.
The observation window is based on the `start.time` given in the
`auxfile`, the length of the measurement (`obs.length`), and a
`shoulder` before `start.time` and after `start.time + obs.length`. In
this example, the observation time is 3 minutes (180 seconds) and the
shoulder is 30 seconds. Therefore, the observation window shows 30
seconds before the `start.time` and 210 seconds after.

``` r
?obs.win

# Define the measurements' window of observation
LGR_ow <- obs.win(inputfile = LGR_imp, auxfile = auxfile,
                  obs.length = 180, shoulder = 30)
```

![](man/figures/click.peak1.png)

Pay attention to the warning message given by `obs.win()` when there are
more than 20 measurements (which is not the case in this example).

Note that for more than one gas measurement, it is better to use the
function `click.peak.loop()` with `lapply()` rather than using
`click.peak()` for each measurement individually.

``` r
?click.peak
?click.peak.loop

# Manually identify measurements by clicking on the start and end points
LGR_manID <- lapply(seq_along(LGR_ow), click.peak.loop, flux.unique = LGR_ow) %>%
  map_df(., ~as.data.frame(.x))
```

If the number of observation is under a certain threshold
(`warn.length = 60`), a warning will be given after clicking on the
start and end points as such:

    ## Warning message: Number of observations for UniqueID: 733_C_C is 59 observations

Otherwise, if the number of observation satisfies this threshold, then
the following message is given instead:

    ## Good window of observation for UniqueID: 733a_C_C

Between each measurement, the result of the `click.peak()` function is
displayed for 3 seconds. To increase this delay, change the parameter
`sleep` in the function `click.peak.loop()`.

![](man/figures/click.peak2.png)

To convert the flux estimate’s units into nmol
CO<sub>2</sub>/H<sub>2</sub>O m<sup>-2</sup>s<sup>-1</sup> or µmol
CH<sub>4</sub>/N<sub>2</sub>O m<sup>-2</sup>s<sup>-1</sup>, the
temperature inside the chamber (`Tcham`; °C) and the atmospheric
pressure inside the chamber (`Pcham`; kPa) are also required. If `Pcham`
and `Tcham` are missing, normal atmospheric pressure (101.325 kPa) and
normal air temperature (15 °C) are used.

Additionally, one must provide the surface area inside the chamber
(`Area`; cm<sup>2</sup>) and the total volume in the system, including
tubing, instruments and chamber (`Vtot`; L). If `Vtot` is missing, one
must provide an offset (distance between the chamber and the soil
surface; cm) and the volume of the chamber (`Vcham`; L). In that case,
the volume inside the tubing and the instruments is considered
negligible, or it should be added to `Vcham`.

The final output, before flux calculation requires: `UniqueID`, `Etime`,
`flag`, `Vtot` (or `Vcham` and `offset`), `Area`, `Pcham`, `Tcham`,
`H2O_ppm` and other gases.

### Flux calculation

The hardest part is now behind you. All that’s left is to run the flux
calculation with the `goFlux()` function. Then use the `best.flux()`
function to select the best flux estimates (LM or HM) with our list of
non-arbitrary thresholds, and plot the results for visual inspection.

``` r
# Flux calculation -------------------------------------------------------------
?goFlux
?best.flux

# Calculate fluxes for all gas types
CO2_flux <- goFlux(LGR_manID, "CO2dry_ppm")
CH4_flux <- goFlux(LGR_manID, "CH4dry_ppb")
H2O_flux <- goFlux(LGR_manID, "H2O_ppm")

# Use best.flux to select the best flux estimates (LM or HM)
# based on a list of criteria
CO2_flux_res <- best.flux(CO2_flux, criteria = c("MAE", "g.factor", "MDF"))
CH4_flux_res <- best.flux(CH4_flux, criteria = c("MAE", "g.factor", "MDF"))
H2O_flux_res <- best.flux(H2O_flux, criteria = c("MAE", "AICc", "MDF"))

# Plots results ----------------------------------------------------------------
?flux.plot
?flux2pdf

# Make a list of plots of all measurements, for each gastype.
# With the function flux.plot, all parameters can be displayed.
# You can chose what parameters to display on the plots.
plot.legend = c("MAE", "RMSE", "AICc", "k.ratio", "g.factor")
plot.display = c("MDF", "prec", "nb.obs", "flux.term")
quality.check = TRUE

CO2_flux_plots <- flux.plot(CO2_flux_res, LGR_manID, "CO2dry_ppm", shoulder=20,
                            plot.legend, plot.display, quality.check)
CH4_flux_plots <- flux.plot(CH4_flux_res, LGR_manID, "CH4dry_ppb", shoulder=20,
                            plot.legend, plot.display, quality.check)
H2O_flux_plots <- flux.plot(H2O_flux_res, LGR_manID, "H2O_ppm", shoulder=20, 
                            plot.legend, plot.display, quality.check)

# Combine plot lists into one list
flux_plot.ls <- c(CO2_flux_plots, CH4_flux_plots, H2O_flux_plots)

# Save plots to pdf
flux2pdf(flux_plot.ls, outfile = "demo.results.pdf")
```

Here is an example of how the plots are saved as pdf:

![](man/figures/demo.results_Page_4.png)

You can then save the results as RData or in an Excel sheet to further
process the results.

``` r
require(openxlsx)

# Save RData
save(CO2_flux_res, file = "CO2_flux_res.RData")
save(CH4_flux_res, file = "CH4_flux_res.RData")
save(H2O_flux_res, file = "H2O_flux_res.RData")

# Save to Excel
write.xlsx(CO2_flux_res, file = "CO2_flux_res.xlsx")
write.xlsx(CH4_flux_res, file = "CH4_flux_res.xlsx")
write.xlsx(H2O_flux_res, file = "H2O_flux_res.xlsx")
```

## Troubleshoot problems with `install_github()`

### Warning: package is in use and will not be installed

If you get this warning while trying to install an R package from
GitHub:

    ## Warning: package ‘GoFluxYourself’ is in use and will not be installed

This error means that the package is loaded. Before re-installing the
package, you must first detach it:

``` r
detach("package:GoFluxYourself", unload = TRUE)
```

If this does not solve the problem, restart your session and try again.

------------------------------------------------------------------------

### Error: API rate limit exceeded

If you get this error while trying to install an R package from GitHub:

    ## Error: Failed to install 'GoFluxYourself' from GitHub:
    ##   HTTP error 403.
    ##   API rate limit exceeded for xxx.xxx.xxx.x (But here's the good news: Authenticated requests get a higher rate limit. Check out the documentation for more details.)
    ## 
    ##   Rate limit remaining: 0/60
    ##   Rate limit reset at: 2023-10-16 09:08:07 UTC
    ## 
    ##   To increase your GitHub API rate limit
    ##   - Use `usethis::create_github_token()` to create a Personal Access Token.
    ##   - Use `usethis::edit_r_environ()` and add the token as `GITHUB_PAT`.

#### Step 1: Set up a GitHub account

**Go to <https://github.com/join> .**

1.  Type a user name, your email address, and a password.

2.  Choose Sign up for GitHub, and then follow the instructions.

#### Step 2: Create a GitHub token

Run in R:

``` r
usethis::create_github_token()
```

On pop-up website, generate and copy your token.

#### Step 3: Set your GitHub PAT from R

Run in R with your own token generated in step 2:

``` r
credentials::set_github_pat("YourTokeninStep2")
```

## Community Guidelines

Authors: Karelle Rheault and Klaus Steenberg Larsen

To report problems, seek support or contribute to the package, please
contact the maintainer, Karelle Rheault (<karh@ign.ku.dk>). Suggestions
for new features or improvements are always welcome.
