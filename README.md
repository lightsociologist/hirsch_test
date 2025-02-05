# Inequality in measuring scholarly success: Variation in the h-index within and between disciplines

This is the replication package for the PLoS ONE article ["Inequality in measuring scholarly success: Variation in the h-index within and between disciplines"](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0316913) by [Ryan Light](https://ryanlight.netlify.app/), [Aaron Gullickson](https://aarongullickson.netlify.app), and [Jill Ann Harrison](https://cas.uoregon.edu/directory/sociology/all/jah). From the abstract of the paper:

> Scholars and university administrators have a vested interest in building equitable valuation systems of academic work for both practical (e.g., resource distribution) and more lofty purposes (e.g., what constitutes “good” research). Well-established inequalities in science pose a difficult challenge to those interested in constructing a parsimonious and fair method for valuation as stratification occurs within academic disciplines, but also between them. The h-index, a popular research metric, has been formally used as one such method of valuation. In this article, we use the case of the h-index to examine how the distribution of research metrics reveal within and between discipline inequalities. Using bibliometric data from 1960-2019 on over 50,000 high-performing scientists—the top 2% most frequently cited authors—across 174 disciplines, we construct random effects within-between models predicting the h-index. Results suggest significant within-discipline variation in several forms, specifically sole-authorship and female penalties. Results also show that a sole authorship penalty plays a significant role in well-known between-discipline variation. Field-specific models emphasize the “apples-to-oranges,” or incommensurable, property of cross-discipline comparison with significant heterogeneity in sole-authorship and female penalties within fields. In conclusion, we recommend continued caution when using the h-index or similar metrics for valuation purposes and the prioritization of substantive valuations from disciplinary experts.

The analysis is done exclusively in R. All code and data to reproduce the analysis 
is available in the `analysis` subdirectory as well as a README that describes the 
analysis in more detail. The `data` directory where the data are stored. 

The easiest way to run the entire project is to do the following:

1. Source the `check_packages.R` script in the `utils` directory to install all package dependencies.
2. Render the entire project as a quarto project. This will require the installation of [Quarto](https://quarto.org). This is easiest to do from the Build tab in RStudio, but you can also just run `quarto render` from the project home directory. 

All output from rendering the project will be placed in a local `_products` directory.
