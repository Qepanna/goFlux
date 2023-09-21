# GoFluxYourself <img src="man/figures/GoFluxYourself.png" align="right" width="200"/>
GoFluxYourself: A user-friendly way to calculate GHG fluxes yourself, regardless of user experience

Authors: Karelle Rheault and Klaus Steenberg Larsen

Maintainer: Karelle Rheault <karh@ign.ku.dk>

This package is meant to be "student proof", meaning that no extensive knowledge or experience is needed to import raw data into R, chose the best model to calculate fluxes (LM, HM or no flux), quality check the results objectively and obtain high quality flux estimates.

The package contains a wide range of functions that lets the user import raw data from a variety of instruments (LI-COR, LGR, GAIA2TECH and Picarro); calculate fluxes from a variety of GHG (CO2, CH4, N2O and H2O) with both linear (LM) and non-linear (HM) flux calculation methods; align instruments clocks after data import; identify measurements (start and end time) if there are no automatic chamber recordings (e.g. LI-COR smart chamber); plot measurements for easy visual inspection; and quality check the measurements based on objective criteria and non-arbitrary thresholds. 