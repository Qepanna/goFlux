# Plan: Fix goFlux Documentation Website Automation

**TL;DR:** The documentation system has a broken metadata extraction pipeline that fails to process most R functions, causing missing API documentation and empty generated files. We need to fix the core extraction logic, remove broken API navigation, create dynamic linkage between R code and QMD files, and add installation checking. This will ensure the website automatically reflects all changes in the R package.

**Steps**

Read all the sections in the website within the quarto/ directory to udnerstand the structure and content. do this one by one to see which sections are linked to which files and content. 

1. **Fix the metadata extraction pipeline in** [quarto/_generate.R](quarto/_generate.R)
   - Debug and repair the help database lookup logic (lines 250-280)  
   - Ensure all exported functions from [NAMESPACE](NAMESPACE) are processed
   - Test that `import.eosMX12`, import.{}.R are properly extracted and included in the installation section of the website. the automation should include all functions in the R/ directory, not just a hardcoded list. This will ensure that any new functions added to the package are automatically documented without needing manual updates to the generation script. 

   In 'Installation' section of the website, include the requirements for the package, in the correct section: "If prompted, it is recommended to update any pre-installed packages. The functioning of the package depends on many other packages (AICcmodavg, data.table, dplyr, ggnewscale, ggplot2, ggstar, graphics, grDevices, grid, gridExtra, lubridate, minpack.lm, msm, pbapply, plyr, purrr, rjson, rlist, SimDesign, stats, stringr, tibble, tidyr, toOrdinal, utils), which will be installed when installing goFlux."
   Other required packages should also be updated in the 'Installation' section of the website, in the correct section. This will ensure that users are aware of all necessary dependencies and can install them correctly to use the goFlux package without issues.

   in the 'Import' section of the website, we update the list of instruments for the 'Import raw data section' and search first in the whole repository if there is information for the description, and finally in the internet to supply the one liner description for the instrument *see current list for examples*. we use the internet link to hyperlink the name of the instrument to the website. This will ensure that users have accurate and up-to-date information about the supported instruments and can easily access additional details if needed. check if the current list of instruments matches with the isntruments in inst/extdata and R/import.{}.R files, and update the list accordingly. This will ensure that the documentation accurately reflects the capabilities of the package and provides users with the correct information about supported instruments.
  check each section of Import to reflect what arguments, usage, and details, and examples are avaibale for each instrument from the repository.

  For goflux, bestflux, plots sections of the website, mostly the changes are in the arguments list and usage which are sourced from what the developers write inR/best.flux.R documentation and R/{}.R files. We need to make sure that the documentation in the website reflects the current state of the codebase, and that any changes or updates to the functions are accurately represented in the documentation. This will ensure that users have access to the most up-to-date information about how to use the package and its functions effectively. 

  Other Functions should be updated in the same way, by checking the documentations in R/{}.R files; the Other Functions is room for functions that are not already included in other sections but exist in the codebase; for example iso.comp, crop.meas, and auto.deadband functions. We generate the needed sections (check existing secions, iso.comp for example, which has Usage, Arguments, Details, and Example sections whcih can be found in the R files and comments, as well as the Roxygen generated documentations. 

  Examples, Troubleshoot, and About sections are mostly static content. 


2. **Remove broken API Reference navigation from** [quarto/_quarto.yml](quarto/_quarto.yml)  
   - Delete the "API Reference" section (lines 40-44) that links to empty generated files
   - Keep the main tutorial navigation structure intact

3. **Create dynamic R package integration in QMD files**
   - Modify [quarto/import.qmd](quarto/import.qmd) to automatically list all available import functions
   - Add R code chunks that scan the NAMESPACE and R/ directory for functions
   - Generate instrument tables dynamically from function metadata rather than manual lists

4. **Add package installation validation**
   - Create new QMD section that checks if goFlux is properly installed
   - Add R code chunks in [quarto/install.qmd](quarto/install.qmd) to verify dependencies
   - Include automated testing that examples can run successfully

5. **Implement automatic content synchronization**
   - Modify [quarto/_examples.R](quarto/_examples.R) to validate that all R functions have corresponding examples
   - Add warning system when new functions are added to R/ but not documented in QMDs
   - Create automated cross-referencing between manual content and available functions

6. **Update GitHub Actions workflow** [.github/workflows/quarto-deploy.yaml](.github/workflows/quarto-deploy.yaml)
   - Add step to verify generation was successful before deployment  
   - Include validation that critical files were properly created
   - Add failure notifications when automation breaks

**Verification**
- Run `quarto render` locally to test all QMD files render correctly
- Check that missing functions (`import.eosMX12`, `import.HT8850`, `import.LI8150`) appear in documentation
- Verify website navigation works without broken API reference links
- Confirm that adding a new function to R/ triggers documentation updates

**Decisions**
- **Remove API Reference navigation**: Since it creates empty pages and you mentioned it's not needed
- **Dynamic content over manual lists**: Automatically scan R package instead of maintaining manual instrument lists
- **Validation-first approach**: Check installation and generation success before deploying to catch failures early

This plan will create a robust system where any changes to the R package automatically reflect in the website documentation, eliminating the current disconnect between your codebase and published documentation.

