# GoFluxYourself: A user-friendly way to calculate GHG fluxes yourself, regardless of user experience <img src="man/figures/GoFluxYourself.png" align="right" width="200"/>

### One Package to rule them all

Non-steady state chambers are widely used for measuring soil greenhouse gas (GHG) fluxes, such as CO<sub>2</sub>, CH<sub>4</sub>, N<sub>2</sub>O, and water vapor (H<sub>2</sub>O). While linear regression (LM) is commonly used to estimate GHG fluxes, this method tends to underestimate the pre-deployment flux (*f<sub>0</sub>*). Indeed, a non-linearity is expected when gas concentration increases inside a closed chamber, due to changes in diffusion gradients between the soil and the air inside the chamber. In addition, lateral gas flow and leakage contribute to non-linearity. Many alternative to LM have been developped to try and provide a more accurate estimation of *f<sub>0</sub>*, for instance the method of [Hutchinson and Mosier (HM) (1981)](https://doi.org/10.2136/sssaj1981.03615995004500020017x), which was implemented in the [`HMR` package](https://cran.r-project.org/package=HMR) by [Pedersen et al., 2010](https://doi.org/10.1111/j.1365-2389.2010.01291.x). However, non-linear models have a tendency to largely overestimate some flux measurements, due to an exaggerated curvature. Therefore, the user is expected to decide which method is more appropriate for each flux estimate. With the advent of portable greenhouse gas analyzers (e.g. [Los Gatos Research (ABB) laser gas analyzers](https://new.abb.com/products/measurement-products/analytical/laser-gas-analyzers/laser-analyzers/lgr-icos-portable-analyzers)), soil GHG fluxes have become much easier to measure, and more efficient ways to calculate these flux estimates are needed in order to process large amounts of data. A recent approach was developed by [Hüppi et al., 2018](https://doi.org/10.1371/journal.pone.0200876) to restrict the curvature in the HM model for a more reliable flux estimate. In the HM model, the curvature is controlled by the kappa parameter. Hüppi et al. suggested the use of the kappa.max to limit the maximal curvature allowed in the model (see their R package [`gasfluxes`](https://cran.r-project.org/package=gasfluxes), available on the CRAN). This procedure introduces less arbitrary decisions in the flux estimation process.

Previous software that have been developed to calculate GHG fluxes were limited in many aspects that this package is meant to overcome. Most were limited to the linear regression approach (e.g. [LI-COR SoilFluxPro](https://www.licor.com/env/products/soil-flux/soilfluxpro), [Flux Puppy](https://www.sciencedirect.com/science/article/pii/S0168192319301522), and the R packages [`flux`](https://cran.r-project.org/package=flux) and [`FluxCalR`](https://github.com/junbinzhao/FluxCalR)), others were compatible with only one designated system (e.g. [LI-COR SoilFluxPro](https://www.licor.com/env/products/soil-flux/soilfluxpro), [Flux Puppy](https://www.sciencedirect.com/science/article/pii/S0168192319301522)), and some were impractical with large amounts of measurements (e.g. the R package [`HMR`](https://cran.r-project.org/package=HMR)) or simply did not include data pre-processing (e.g. the R packages [`HMR`](https://cran.r-project.org/package=HMR), [`flux`](https://cran.r-project.org/package=flux) and [`gasfluxes`](https://cran.r-project.org/package=gasfluxes)).

This new R package, `GoFluxYourself` is meant to be "student proof", meaning that no extensive knowledge or experience is needed to import raw data into R, chose the best model to calculate fluxes (LM, HM or no flux), quality check the results objectively and obtain high quality flux estimates. The package contains a wide range of functions that lets the user import raw data from a variety of instruments (LI-COR, LGR, GAIA2TECH and Picarro); calculate fluxes from a variety of GHG (CO<sub>2</sub>, CH<sub>4</sub>, N<sub>2</sub>O, and H<sub>2</sub>O) with both linear (LM) and non-linear (HM) flux calculation methods; align instruments clocks after data import; interactively identify measurements (start and end time) if there are no automatic chamber recordings (e.g. LI-COR smart chamber); plot measurements for easy visual inspection; and quality check the measurements based on objective criteria and non-arbitrary thresholds. 

<p>*Three R packages for the Elven-kings under the CRAN,*<br>
*Seven for the Dwarf-lords in their halls of open softwares,*<br>
*Nine for Mortal Men doomed to write scripts themselves,*<br>
*One for the Dark Lady on her dark throne*<br>
*In the Land of GitHub where the Shadows lie.*<br>
*One Package to rule them all, One Package to find them,*<br>
*One Package to bring them all and in the darkness bind them*<br>
*In the Land of GitHub where the Shadows lie.*</p>

<p>Authors: Karelle Rheault and Klaus Steenberg Larsen<br>
Maintainer: Karelle Rheault <karh@ign.ku.dk></p>
