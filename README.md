# Survival package for R

## Summer Research, 2018

#### _Students:_ Ashley Jacobson and Victor Wilson

#### _Faculty advisor:_ Shannon Pileggi

### Objective

The objective of this summer research is to create an R package that can execute parametric and non-parametric survival analysis techniques similar to those in Minitab.

### Deliverables

**A GitHub repository which contains the following:**

1.  An R package for the survival functions. 

2.  A log of hours spent on the summer research by each student, which includes date, hours, and activity summary.

3.  A presentation to the Statistics Department.

4.  A presentation at the CSM annual research conference.

5.  A manuscript to submit to the [R Journal](https://journal.r-project.org/) detailing the work and submit an abstract to the [RStudio Conference](https://www.rstudio.com/conference/).

### Specific Aims

**1.  Utilize GitHub to collaborate on project materials and updates.**

  * Karl Broman's [github tutorial](http://kbroman.org/github_tutorial/).

  * Jenny Bryan's [Happy git with R](http://happygitwithr.com/).
  
  * DataCamp's [Introduction to git for data science](https://www.datacamp.com/courses/introduction-to-git-for-data-science) course (good for learning command line, not necessary if using the RStudio IDE).
  
  * Also check out [using version control with RStudio](https://support.rstudio.com/hc/en-us/articles/200532077-Version-Control-with-Git-and-SVN) and [this video on Git and RStudio](https://www.rstudio.com/resources/webinars/rstudio-essentials-webinar-series-managing-part-2/).


**2.  Adhere to good programming practices.**
  
  * Write all R code according to [Hadley Wickam's Style Guide](http://adv-r.had.co.nz/Style.html).
  
  * Use the [tidyverse style guide](http://style.tidyverse.org/) for an additional reference.
  
  * Learn about how to write R functions from DataCamp's [Writing functions in R](https://www.datacamp.com/courses/writing-functions-in-r) course.
  
  * Use Hadley Wickham's [R for Data Science](http://r4ds.had.co.nz/) book as a reference (Ch19 also discusses functions).
  
  
  **3.  Create an R package that contains survival functions.**  At a minimum, this should be downloadable through devtools; as time allows, consider putting it on CRAN.

  *  DataCamp's blog post [R Packages: A Beginner's Guide](https://www.datacamp.com/community/tutorials/r-packages-guide)
    
  *  Hillary Parker's blog post [Writing an R package from scratch](https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/)
  
  *  Hadley Wickham's [R pacakges](http://r-pkgs.had.co.nz/) book.
  
  *  RStudio's video on [Package writing in RStudio](https://www.rstudio.com/resources/webinars/rstudio-essentials-webinar-series-programming-part-3/).
  
  
   **4.  Provide documentation for the R package.**

  *  Use the [roxygen package](https://cran.r-project.org/web/packages/roxygen2/vignettes/roxygen2.html) to document code.
  
  *  Write a [vignette](http://r-pkgs.had.co.nz/vignettes.html) to accompany the package.
  
  *  Consider using [pkgdown](http://pkgdown.r-lib.org/index.html) to create a website. 
  
  
  **5. Review existing R packages for survival analysis.**
  
  * A comprehensive list:  [CRAN Task View: Survival Analysis](https://cran.r-project.org/web/views/Survival.html).
  
  * Pay attention to: [survival](https://cran.r-project.org/web/packages/survival/index.html), [fitdistrplus](https://cran.r-project.org/web/packages/fitdistrplus/index.html), [flexsurv](https://cran.r-project.org/web/packages/flexsurv/index.html), and [survminer](https://cran.r-project.org/web/packages/survminer/index.html).
  
  **6.  Create functions for items that are not currently easy to achieve in R.  (Make sure that these cannot be accomplished in the existing R packages).**  Pay attention to the parameterization of a distribution, which is often different between [Minitab](https://support.minitab.com/en-us/minitab/18/help-and-how-to/modeling-statistics/reliability/how-to/distribution-overview-plot-right-censoring/methods-and-formulas/parametric-methods-and-formulas/distribution-functions/) and R.
  
*Parametric:*

* Lab 2:
    + Compute survival probabilities based on a specified parametric distribution.
    + Display an estimated survival, hazard, or cumulative hazard curve based on a specified parametric distribution (singular as well as by groups).
    + Estimate median survival time based on a specified parametric distribution.
    
* Lab 3:
    + Estimate parameters based on a specified parametric distribution.
    + Compute the Anderson-Darling test statistic, or otherwise appropriate measure of fit, for a specified parametric distribution.
    + Estimate various statistics, including median, mean, standard deviation, and percentiles of survival time based on a specified parametric distribution.
    + Produce a qqplot to examine distribution fit.

*Non-parametric:*

* Lab 4:
    + Plot estimated hazard/cumulative hazard functions
    
* Lab 5:
    + Compute the Wilcoxon test statistic and p-value to compare survival experiences of two groups.
    
**7.  Consider also converting some of the functions to a Shiny App.**
  
  * [Shiny tutorial on RStudio](https://shiny.rstudio.com/tutorial/)
  
  * DataCamp's [Building Web Applications in R with Shiny](https://www.datacamp.com/courses/building-web-applications-in-r-with-shiny)
    
    
**8.  Other potentially useful DataCamp courses.**    

   * [Working with the RStudio IDE Part 1](https://www.datacamp.com/courses/working-with-the-rstudio-ide-part-1)  
   
   * [Working with the RStudio IDE Part 2](https://www.datacamp.com/courses/working-with-the-rstudio-ide-part-2) 
   
   * [Introduction to the tidyverse](https://www.datacamp.com/courses/introduction-to-the-tidyverse)
   
   * [Data visualization with ggplot, 1](https://www.datacamp.com/courses/data-visualization-with-ggplot2-1)
   
   * [Data visualization with ggplot, 2](https://www.datacamp.com/courses/data-visualization-with-ggplot2-2)
   
   * [Data manipulation in R with dplyr](https://www.datacamp.com/courses/dplyr-data-manipulation-r-tutorial)
  

