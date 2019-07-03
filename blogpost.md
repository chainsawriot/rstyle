A Computational Analysis of the Dynamics of R Style Based on 94 Million Lines of Code from All CRAN Packages in the Past 20 Years
================
Chia-Yi Yen, Mia Huai-Wen Chang, Chung-hong Chan

This is a blog post accompanied the useR! 2019 poster presentation of the paper *"A Computational Analysis of the Dynamics of R Style Based on 94 Million Lines of Code from All CRAN Packages in the Past 20 Years"* by Ms. Chia-Yi Yen, Ms. Mia Huai-Wen Chang and Dr. Chung-hong Chan. The full paper is pending.

Five-sentence Summary
---------------------

-   There are many programming style variations. R is particularly problematic.
-   We have analyzed 94 Million Lines of R code and quantified the evolution in popularities of many style-elements.
-   We attribute 3 main factors that drive changes in programming style: effect of style-guides, effect of introducing new features and effect of editors.
-   We have identified communitity-specific programming style variations. For example, there are communities which do not use snake\_case at all!
-   A consensus in programming style is forming. We have summarised it into a **Consensus-based Style**.

Introduction
------------

R is flexible. For example, one can use *&lt;-* or *=* as the assignment operator. The following two functions can both be correctly evaluated by R.

``` r
sum_of_square <- function(x) {
    return(sum(x^2))
}
```

``` r
sum_of_square = function(x) {
    return(sum(x^2))
}
```

One area that can highlight this flexible is the naming conventions. According to the previous research by [Bååth (2012)](https://journal.r-project.org/archive/2012-2/RJournal_2012-2_Baaaath.pdf), there are at least 6 styles and none of the 6 has dominated the scene. There are still some other style-elements that R programmers have freedom to adopt, e.g. whether or not to add spaces around infix operators, use double quotation marks or single quotation marks to denote strings, etc.

Various efforts to standardize the programming style (e.g. [Google's R Style Guide](https://google.github.io/styleguide/Rguide.xml), The [Tidyverse Style Guide](https://style.tidyverse.org/), [Bioconductor Coding Style](https://www.bioconductor.org/developers/how-to/coding-style/), etc...) are available. These style guides are based on normative assessment of code quality, e.g. using style-elements that improve clarity. However, we argue that we should first study the current situation, and preferably, the historical development, of programming style variations (PSV) to supplement these standardization efforts. We have undertaken such a task, so that the larger R community can have a baseline to evaluate the effectiveness of those standardization efforts. Also, we can have better understanding on the factors driving increase and decrease in PSV historically, such that more effective efforts can be formulated.

Analysis
--------

### Data Source

In January 2019, we created an local mirror of CRAN using the rsync method suggested by the [CRAN Mirror HOWTO](https://cran.r-project.org/mirror-howto.html). In our local mirror, it contains all package as tarball files (.tar.gz). By all package, we mean all packages actively listed online on the CRAN websites and all packages delisted for whatever reasons e.g. no maintainer, etc. In this analysis, we include all packages, including those delisted.

In order to faciliate the analysis, we have developed the [baaugwo package](https://github.com/chainsawriot/baaugwo) to extract all R sourcecode and metadata from these tarballs. In this study, only the source code from the */R* directory of each tarball file is included. We have also archived the meta data from the DESCRIPTION and NAMESPACE files from these tarballs.

In order to cancel out the effect of multiple submissions in a year by one particular package, we have applied the "one-submission-per-year" rule to randomly selected only one submission from a year for each package. Unless explicitly notice, we present below the analysis of this "one-submission-per-year" sample. Similarly, unless explicitily notice, the unit of the analysis is **function**.

### Quantification of PSV

Every function in our sample are parsed into a parse tree (or [expression](https://stat.ethz.ch/R-manual/R-devel/library/base/html/expression.html)) using the [parser](https://github.com/jimhester/lintr/blob/master/R/get_source_expressions.R) from the [lintr package](https://github.com/jimhester/lintr).

These parse trees were then linted using the linters from the lintr package to detect for specific style-elements. The style-element considered in this study are:

-   Using T/F instead of TRUE / FALSE
