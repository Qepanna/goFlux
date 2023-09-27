
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GoFluxYourself: A user-friendly way to calculate GHG fluxes yourself, regardless of user experience <img src="man/figures/GoFluxYourself.png" align="right" width="200"/>

### One Package to rule them all

Non-steady state chambers are widely used for measuring soil greenhouse
gas (GHG) fluxes, such as CO<sub>2</sub>, CH<sub>4</sub>,
N<sub>2</sub>O, and water vapor (H<sub>2</sub>O). While linear
regression (LM) is commonly used to estimate GHG fluxes, this method
tends to underestimate the pre-deployment flux (*f*<sub>0</sub>).
Indeed, a non-linearity is expected when gas concentration increases
inside a closed chamber, due to changes in diffusion gradients between
the soil and the air inside the chamber. In addition, lateral gas flow
and leakage contribute to non-linearity. Many alternative to LM have
been developed to try and provide a more accurate estimation of
*f*<sub>0</sub>, for instance the method of [Hutchinson and Mosier (HM)
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
al. suggested the use of the kappa.max to limit the maximal curvature
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
raw data into R, chose the best model to calculate fluxes (LM, HM or no
flux), quality check the results objectively and obtain high quality
flux estimates. The package contains a wide range of functions that lets
the user import raw data from a variety of instruments (LI-COR, LGR,
GAIA2TECH and Picarro); calculate fluxes from a variety of GHG
(CO<sub>2</sub>, CH<sub>4</sub>, N<sub>2</sub>O, and H<sub>2</sub>O)
with both linear (LM) and non-linear (HM) flux calculation methods;
align instruments clocks after data import; interactively identify
measurements (start and end time) if there are no automatic chamber
recordings (e.g. LI-COR smart chamber); plot measurements for easy
visual inspection; and quality check the measurements based on objective
criteria and non-arbitrary thresholds.

> *Three R packages for the Elven-kings under the CRAN,  
> Seven for the Dwarf-lords in their halls of open software,  
> Nine for Mortal Men doomed to write scripts themselves,  
> One for the Dark Lady on her dark throne  
> In the Land of GitHub where the Shadows lie.  
> One Package to rule them all, One Package to find them,  
> One Package to bring them all and in the darkness bind them  
> In the Land of GitHub where the Shadows lie.*

### About the package

This package `GoFluxYourself` is meant to be “student proof”, meaning
that no extensive knowledge or experience is needed to import raw data
into R (except for knowing how to use R, of course), chose the best
model to calculate fluxes (LM or HM, that is the question. -Shakespeare,
1603), quality check the results objectively (hence the no experience
needed) and obtain high quality flux estimates from static chamber
measurements (wonderful!).

The package contains a wide range of functions that lets the user import
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

After import, the user can chose from two methods for identification of
measurements:

- **Manual selection of measurements** - based on `start.time`, provided
  separately in an auxiliary file. The function `obs.win()` splits the
  imported data into a list of data frame (divided by `UniqueID`) and
  creates an observation window around the `start.time` to allow for a
  manual selection of the start and end points of each measurements,
  using the function `click.peak.loop()`.
- **Automatic selection of measurements** - based on automatic
  recordings of chamber opening and closing from an instrument such as
  the LI-COR Smart Chamber or the GAIATECH Automated ECOFlux chamber.

The function `goFlux()` calculates fluxes from a variety of greenhouse
gases (CO<sub>2</sub>, CH<sub>4</sub>, N<sub>2</sub>O, and
H<sub>2</sub>O) using both linear (LM) and non-linear (Hutchinson and
Mosier) flux calculation methods.

Following flux calculation, the user can select the best flux estimate
(LM or HM) based on objective criteria and non-arbitrary thresholds,
using the `best.flux()` function:

- **Assumed non-linearity**: If all criteria are respected, the best
  flux estimate is assumed to be the non-linear estimate from the
  Hutchinson and Mosier model.
- **G-factor**: the g-factor is the ratio between the result of a
  non-linear flux calculation model (e.g. Hutchinson and Mosier; HM) and
  the result of a linear flux calculation model ([Hüppi et al.,
  2018](https://doi.org/10.1371/journal.pone.0200876)). With the
  `best.flux()` function, one can chose a limit at which the HM model is
  deemed to overestimate (*f*<sub>0</sub>). Recommended thresholds for
  the g-factor are \<4 for a flexible threshold, \<2 for a medium
  threshold, or \<1.25 for a more conservative threshold. The default
  setting in the function `best.flux()` is `g.limit = 2`. If the
  g-factor is above the specified threshold, the best flux estimate will
  switch to LM instead of HM.
- **Minimal Detectable Flux**: The minimal detectable flux (MDF) is
  based on instrument precision, measurements time, and the number of
  measurement points ([Bréchet et al.,
  2021](https://doi.org/10.1111/nph.17352); [Christiansen et al.,
  2015](https://doi.org/10.1016/j.agrformet.2015.06.004)). Under the
  MDF, the flux estimate is considered null, but the function will not
  return a 0 to avoid heteroscedasticity of variances. There will simply
  be a comment in the columns “LM.diagnose” or “HM.diagnose” saying that
  there is “No detectable flux (MDF)”.
- **Kappa max**: The parameter kappa determines the curvature of the
  non-linear regression in the Hutchinson and Mosier model. A large
  kappa, returns a strong curvature. A maximum threshold for this
  parameter, kappa-max, is calculated based on the minimal detectable
  flux (MDF), the linear flux estimate and the measurement time ([Hüppi
  et al., 2018](https://doi.org/10.1371/journal.pone.0200876)). The
  units of the kappa-max is s<sup>-1</sup>. This limit of kappa-max is
  included in the `goFlux()` function, so that the non-linear flux
  estimate cannot exceed this maximum curvature.  
  An important limitation of kappa-max needs to be considered. [Hüppi et
  al. (2018)](https://doi.org/10.1371/journal.pone.0200876) developed
  this threshold based on low frequency measurements (4 points at best)
  and kappa-max is proportional to the number of measurements.
  Therefore, with high frequency measurements, such as provided with
  laser-based analyzers, the number of measurements is so large (at
  least 60 points), that the maximum curvature allowed by kappa-max is
  also very large. To reduce the overestimation of kappa-max while using
  laser-based analyzers, one can use the parameter `k.mult` (kappa
  multiplier) in the `goFlux()` function. Using a small fraction, such
  as 1/10 will help further control the curvature of the non-linear
  regression.
- **Statistically significant flux (p-value)**: more info to come…
- **Coefficient of determination (r<sup><sub>2</sub></sup>)**: more info
  to come…

### Community Guidelines

Authors: Karelle Rheault and Klaus Steenberg Larsen

To report problems, seek support or contribute to the package, please
contact the maintainer, Karelle Rheault (<karh@ign.ku.dk>). Suggestions
for new features or improvements are always welcome.
