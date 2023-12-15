goFlux: A user-friendly way to calculate GHG fluxes yourself, regardless
of user experience
================
15 December 2023

# Summary

TEST

This new R package `goFlux` has been developed for calculating
greenhouse gas (GHG) flux estimates from static chamber measurements.
Compared to previous software developed for the same purpose, the
`goFlux` package is not limited to the linear regression approach (LM),
but also estimates GHG fluxes from a non-linear regression approach, the
Hutchinson and Mosier model (HM). An automatic selection procedure has
been implemented in the package to help users select the best flux
estimate (LM or HM) based on objective criteria. In addition, this
package can be used to import raw data directly downloaded from a vast
selection of instruments (LI-COR, LGR, GAIA2TECH, Gasmet, Picarro and
PP-Systems).

The package is divided into five steps: 1. import raw data into R; 2.
identify measurements; 3. calculate GHG flux estimates (LM and HM); 4.
automatically select the best flux estimate (LM or HM) based on our
default choices of objective criteria; 5. and visually inspect the
results on plots that can be saved as pdf. For a detailed protocol on
how to use this package, visit the webpage
<https://qepanna.quarto.pub/goflux/>.

# Statement of need

Non-steady state chambers are widely used for measuring soil greenhouse
gas (GHG) fluxes, such as CO<sub>2</sub>, CH<sub>4</sub>,
N<sub>2</sub>O, NH<sub>3</sub>, CO, and water vapor (H<sub>2</sub>O).
While linear regression (LM) is commonly used to estimate GHG fluxes,
this method tends to underestimate the pre-deployment flux
(*f*<sub>0</sub>). Indeed, non-linearity is expected when the gas
concentration increases or decreases inside a closed chamber, due to
changes in gas gradients between the soil and the air inside the
chamber. In addition, lateral gas flow and leakage contribute to
non-linearity. Many alternative to LM have been developed to try and
provide a more accurate estimation of *f*<sub>0</sub>, for instance the
method of Hutchinson and Mosier Hutchinson and Mosier (1981) (HM), which
was implemented in the [`HMR`](https://cran.r-project.org/package=HMR)
package by Pedersen et al. (2010) Pedersen, Petersen, and Schelde
(2010). However, non-linear models have a tendency to largely
overestimate some flux measurements, due to an exaggerated curvature.
Therefore, the user is expected to decide which method is more
appropriate for each flux estimate. With the advent of portable
greenhouse gas analyzers (e.g. [Los Gatos Research (ABB) laser gas
analyzers](https://new.abb.com/products/measurement-products/analytical/laser-gas-analyzers/laser-analyzers/lgr-icos-portable-analyzers)),
soil GHG fluxes have become much easier to measure, and more efficient
ways to calculate these flux estimates are needed in order to process
large amounts of data. A recent approach was developed by Hüppi et
al. (2018) Hüppi et al. (2018) to restrict the curvature in the HM model
for a more reliable flux estimate. In the HM model, the curvature is
controlled by the kappa parameter. Hüppi et al. Hüppi et al. (2018)
suggest the use of the kappa.max to limit the maximal curvature allowed
in the model (see their R package
[`gasfluxes`](https://cran.r-project.org/package=gasfluxes), available
on the CRAN). This procedure introduces less arbitrary decisions in the
flux estimation process.

Previous software developed to calculate GHG fluxes are limited in many
aspects that the `goFlux` package is meant to overcome. Most are limited
to the linear regression approach (e.g. [Flux
Puppy](https://www.sciencedirect.com/science/article/pii/S0168192319301522),
and the R packages [`flux`](https://cran.r-project.org/package=flux)
(Jurasinski et al. 2022) and
[`FluxCalR`](https://github.com/junbinzhao/FluxCalR) (Zhao 2019)),
others do not include data pre-processing (e.g. the R packages
[`HMR`](https://cran.r-project.org/package=HMR) Pedersen, Petersen, and
Schelde (2010), [`flux`](https://cran.r-project.org/package=flux) and
[`gasfluxes`](https://cran.r-project.org/package=gasfluxes) (Fuss
2023)), or if they do, they are compatible with only a few designated
systems (e.g. [Flux
Puppy](https://www.sciencedirect.com/science/article/pii/S0168192319301522)
and [`FluxCalR`](https://github.com/junbinzhao/FluxCalR)), and almost
none include an automatic selection of the best flux estimate (LM or HM)
based on objective criteria.

This new R package, `goFlux` is meant to be “student proof”, meaning
that no extensive knowledge or experience is needed to import raw data
into R, choose the best model to calculate fluxes (LM or HM), quality
check the results objectively and obtain high quality flux estimates.
The package contains a wide range of functions that allows the user to
import raw data from a variety of instruments (LI-COR, LGR, GAIA2TECH,
Gasmet, Picarro and PP-Systems); calculate fluxes from a variety of GHG
(CO<sub>2</sub>, CH<sub>4</sub>, N<sub>2</sub>O, NH<sub>3</sub>, CO and
H<sub>2</sub>O) with both linear (LM) and non-linear (HM) flux
calculation methods; interactively identify measurements (start and end
time) if there are no automatic chamber recordings (e.g. LI-COR smart
chamber); plot measurements for easy visual inspection; and quality
check the measurements based on objective criteria.

# Acknowledgements

We acknowledge Qiaoyan Li, Annelie Skov Nielsen and Frederik Nygaard
Philipsen for their constant feedback that greatly improved the package.
This study was supported by the SilvaNova project funded by the NOVO
Nordisk Foundation (grant no. NNF20OC0059948).

# References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-gasfluxes" class="csl-entry">

Fuss, Roland. 2023. “Gasfluxes: Greenhouse Gas Flux Calculation from
Chamber Measurements.” <https://CRAN.R-project.org/package=gasfluxes>.

</div>

<div id="ref-hüppi2018" class="csl-entry">

Hüppi, Roman, Raphael Felber, Maike Krauss, Johan Six, Jens Leifeld, and
Roland Fuß. 2018. “Restricting the Nonlinearity Parameter in Soil
Greenhouse Gas Flux Calculation for More Reliable Flux Estimates.”
Edited by Upendra M. Sainju. *PLOS ONE* 13 (7): e0200876.
<https://doi.org/10.1371/journal.pone.0200876>.

</div>

<div id="ref-hutchinson1981" class="csl-entry">

Hutchinson, G. L., and A. R. Mosier. 1981. “Improved Soil Cover Method
for Field Measurement of Nitrous Oxide Fluxes.” *Soil Science Society of
America Journal* 45 (2): 311–16.
<https://doi.org/10.2136/sssaj1981.03615995004500020017x>.

</div>

<div id="ref-flux" class="csl-entry">

Jurasinski, Gerald, Franziska Koebsch, Anke Guenther, and Sascha Beetz.
2022. “Flux: Flux Rate Calculation from Dynamic Closed Chamber
Measurements.” <https://CRAN.R-project.org/package=flux>.

</div>

<div id="ref-pedersen2010" class="csl-entry">

Pedersen, A. R., S. O. Petersen, and K. Schelde. 2010. “A Comprehensive
Approach to Soil-Atmosphere Trace-Gas Flux Estimation with Static
Chambers.” *European Journal of Soil Science* 61 (6): 888–902.
<https://doi.org/10.1111/j.1365-2389.2010.01291.x>.

</div>

<div id="ref-zhao2019" class="csl-entry">

Zhao, Junbin. 2019. “FluxCalR: A r Package for Calculating CO 2  and
CH 4  Fluxes from Static Chambers.” *Journal of Open Source Software* 4
(43): 1751. <https://doi.org/10.21105/joss.01751>.

</div>

</div>
