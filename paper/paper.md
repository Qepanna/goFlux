---
title: "goFlux: A user-friendly way to calculate GHG fluxes yourself, regardless of
  user experience"
tags:
- R
- greenhouse gas fluxes
- closed chamber
date: "15 December 2023"
authors:
- name: Karelle Rheault
  orcid: "0000-0002-4888-7232"
  equal-contrib: no
  affiliation: 1
- name: Jesper Riis Christiansen
  orcid: "0000-0002-3277-0734"
  equal-contrib: no
  affiliation: 1
- name: Klaus Steenberg Larsen
  orcid: "0000-0002-1421-6182"
  equal-contrib: no
  affiliation: 1
bibliography: references.bib
affiliations:
- name: Department of Geosciences and Natural Resource Management, University of Copenhagen,
    1958 Frederiksberg C, Denmark
  index: 1
---

# Summary

The R package `goFlux` has been developed for calculating greenhouse gas (GHG) flux estimates from static chamber measurements. Compared to previous software developed for the same purpose, the `goFlux` package is not limited to the linear regression approach (LM), but also estimates GHG fluxes from a non-linear regression approach, i.e., the Hutchinson and Mosier model (HM). An automatic selection procedure has been implemented in the package to help users select the best flux estimate (LM or HM) based on objective criteria. In addition, this package can be used to import raw data directly downloaded from a broad selection of instruments (LI-COR, LGR, GAIA2TECH, Gasmet, Picarro, Aeris and PP-Systems).

The package is divided into five steps: 1. import raw data into R; 2. define the start and end points of each measurement and assign a UniqueID; 3. calculate GHG flux estimates (LM and HM); 4. automatically select the best flux estimate (LM or HM) based on our default choices of objective criteria; 5. visually inspect the results on plots that can be saved as pdf. For a detailed protocol on how to use this package, visit the webpage [https://qepanna.quarto.pub/](https://qepanna.quarto.pub/goflux/){.uri}

# Statement of need

Non-steady state (static) chambers are widely used for measuring soil greenhouse gas (GHG) fluxes, such as CO~2~, CH~4~, N~2~O, NH~3~, CO, and water vapor (H~2~O). While linear regression (LM) is commonly used to estimate GHG fluxes, this method tends to underestimate the pre-deployment flux (*f*~0~). Indeed, non-linearity is expected when the gas concentration increases or decreases inside a closed chamber, due to changes in gas gradients between the soil and the air inside the chamber. In addition, lateral gas flow and leakage contribute to non-linearity. Many alternatives to LM have been developed to try to provide a more accurate estimation of *f*~0~, for instance the method of @hutchinson1981 (HM), which was implemented in the [`HMR`](https://cran.r-project.org/package=HMR) package by @pedersen2010. However, non-linear models have a tendency to largely overestimate some flux measurements, due to an exaggerated curvature. Therefore, the user is expected to decide which method is more appropriate for each flux estimate. With the advent of portable greenhouse gas analyzers (e.g. [Los Gatos Research (ABB) laser gas analyzers](https://new.abb.com/products/measurement-products/analytical/laser-gas-analyzers/laser-analyzers/lgr-icos-portable-analyzers)), soil GHG fluxes have become much easier to measure, and more efficient ways to calculate these flux estimates are needed in order to process large amounts of data. A recent approach was developed by @hüppi2018 to restrict the curvature in the HM model for a more reliable flux estimate. In the HM model, the curvature is controlled by the kappa parameter, $\kappa$. @hüppi2018 suggest the use of the `KAPPA.MAX` to limit the maximal curvature allowed in the model (see their R package [`gasfluxes`](https://cran.r-project.org/package=gasfluxes), available on the CRAN). This procedure introduces less arbitrary decisions in the flux estimation process.

Previous software developed to calculate GHG fluxes are limited in many aspects that the `goFlux` package is meant to overcome. Most are limited to the linear regression approach (e.g. [Flux Puppy](https://www.sciencedirect.com/science/article/pii/S0168192319301522), and the R packages [`flux`](https://cran.r-project.org/package=flux) [@flux] and [`FluxCalR`](https://github.com/junbinzhao/FluxCalR) [@zhao2019]), others do not include data pre-processing (e.g. the R packages [`HMR`](https://cran.r-project.org/package=HMR) [@pedersen2010], [`flux`](https://cran.r-project.org/package=flux) and [`gasfluxes`](https://cran.r-project.org/package=gasfluxes) [@gasfluxes]), or if they do, they are compatible with only a few designated systems (e.g. [Flux Puppy](https://www.sciencedirect.com/science/article/pii/S0168192319301522) and [`FluxCalR`](https://github.com/junbinzhao/FluxCalR)), and none include an automatic selection of the best flux estimate (LM or HM) based on objective criteria, except the R packages [`gasfluxes`](https://cran.r-project.org/package=gasfluxes) and [`HMR`](https://cran.r-project.org/package=HMR).

This new R package, `goFlux`, is meant to be "student proof", meaning that no extensive knowledge or experience is needed to import raw data into R, choose the best model to calculate fluxes (LM or HM), quality check the results objectively and obtain high quality flux estimates. The package contains a wide range of functions that allows the user to import raw data from a variety of instruments (LI-COR, LGR, GAIA2TECH, Gasmet, Picarro, Aeris and PP-Systems); calculate fluxes from a variety of GHG (CO~2~, CH~4~, N~2~O, NH~3~, CO and H~2~O) with both linear (LM) and non-linear (HM) flux calculation methods; interactively identify measurements (start and end time) if there are no automatic chamber recordings (e.g. LI-COR smart chamber); plot measurements for easy visual inspection; and quality check the measurements based on objective criteria.

# Acknowledgements

We acknowledge Qiaoyan Li, Annelie Skov Nielsen and Frederik Nygaard Philipsen for their constant feedback that greatly improved the package. This study was supported by the SilvaNova project funded by the NOVO Nordisk Foundation (grant no. NNF20OC0059948).

# References
