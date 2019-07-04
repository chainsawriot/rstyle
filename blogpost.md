A Computational Analysis of the Dynamics of R Style Based on 94 Million Lines of Code from All CRAN Packages in the Past 20 Years
================
Chia-Yi Yen, Mia Huai-Wen Chang, Chung-hong Chan

This is a preliminary report accompanied the useR! 2019 poster presentation of the paper *"A Computational Analysis of the Dynamics of R Style Based on 94 Million Lines of Code from All CRAN Packages in the Past 20 Years"* by Ms. Chia-Yi Yen, Ms. Mia Huai-Wen Chang and Dr. Chung-hong Chan. The full paper is pending.

Please cite this as:

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

In January 2019, we cloned a local mirror of CRAN using the rsync method suggested by the [CRAN Mirror HOWTO](https://cran.r-project.org/mirror-howto.html). In our local mirror, it contains all package as tarball files (.tar.gz). By all package, we mean all packages actively listed online on the CRAN websites and all packages delisted for whatever reasons e.g. no maintainer, etc. In this analysis, we include all packages, including those delisted.

In order to faciliate the analysis, we have developed the [baaugwo package](https://github.com/chainsawriot/baaugwo) to extract all R sourcecode and metadata from these tarballs. In this study, only the source code from the */R* directory of each tarball file is included. We have also archived the meta data from the DESCRIPTION and NAMESPACE files from these tarballs.

In order to cancel out the effect of multiple submissions in a year by one particular package, we have applied the "one-submission-per-year" rule to randomly selected only one submission from a year for each package. Unless explicitly notice, we present below the analysis of this "one-submission-per-year" sample. Similarly, unless explicitily notice, the unit of the analysis is **exported function**.

### Quantification of PSV

Every function in our sample are parsed into a parse tree (or [expression](https://stat.ethz.ch/R-manual/R-devel/library/base/html/expression.html)) using the [parser](https://github.com/jimhester/lintr/blob/master/R/get_source_expressions.R) from the [lintr package](https://github.com/jimhester/lintr).

These parse trees were then filtered for function definitions and then linted using the linters from the lintr package to detect for specific style-elements. Style-elements considered in this study are:

-   **fx\_assign**: Use = as assignment operators

``` r
softplusFunc = function(value, leaky = FALSE) {
    if (leaky) {
        warnings("using leaky RELU!")
        return(ifelse(value > 0L, value, value * 0.01))
    }
    return(log(1L + exp(value)))
}
```

-   **fx\_opencurly**: An open curly is on its own line

``` r
softplusFunc <- function(value, leaky = FALSE) 
    {
    if (leaky) 
        {
        warnings("using leaky RELU!")
        return(ifelse(value > 0L, value, value * 0.01))
    }
    return(log(1L + exp(value)))
}
```

-   **fx\_infix**: No spaces are added around infix operators.

``` r
softplusFunc<-function(value, leaky=FALSE) {
    if (leaky) {
        warnings("using leaky RELU!")
        return(ifelse(value>0L, value, value*0.01))
    }
    return(log(1L+exp(value)))
}
```

-   **fx\_integer**: Not explicitly type integers

``` r
softplusFunc <- function(value, leaky = FALSE) {
    if (leaky) {
        warnings("using leaky RELU!")
        return(ifelse(value > 0, value, value * 0.01))
    }
    return(log(1 + exp(value)))
}
```

-   **fx\_singleq**: Use single quotation marks for strings

``` r
softplusFunc <- function(value, leaky = FALSE) {
    if (leaky) {
        warnings('using leaky RELU!')
        return(ifelse(value > 0L, value, value * 0.01))
    }
    return(log(1L + exp(value)))
}
```

-   **fx\_commas**: No spaces are added around commas

``` r
softplusFunc <- function(value,leaky = FALSE) {
    if (leaky) {
        warnings("using leaky RELU!")
        return(ifelse(value > 0L,value,value * 0.01))
    }
    return(log(1L + exp(value)))
}
```

-   **fx\_semi**: Use semicolons to terminate lines

``` r
softplusFunc <- function(value, leaky = FALSE) {
    if (leaky) {
        warnings("using leaky RELU!");
        return(ifelse(value > 0L, value, value * 0.01));
    }
    return(log(1L + exp(value)));
}
```

-   **fx\_t\_f**: Use T/F instead of TRUE / FALSE

``` r
softplusFunc <- function(value, leaky = F) {
    if (leaky) {
        warnings("using leaky RELU!")
        return(ifelse(value > 0L, value, value * 0.01))
    }
    return(log(1L + exp(value)))
}
```

-   **fx\_closecurly**: An close curly is not on its own line.

``` r
softplusFunc <- function(value, leaky = F) {
    if (leaky) {
        warnings("using leaky RELU!")
        return(ifelse(value > 0L, value, value * 0.01)) }
    return(log(1L + exp(value))) }
```

-   **fx\_tab**: Use tab to indent

``` r
softplusFunc <- function(value, leaky = FALSE) {
    if (leaky) {
        warnings("using leaky RELU!")
        return(ifelse(value > 0L, value, value * 0.01))
    }
    return(log(1L + exp(value)))
}
```

We have studied also the naming conventions of all included functions. Using the similar technique of [Bååth (2012)](https://journal.r-project.org/archive/2012-2/RJournal_2012-2_Baaaath.pdf), we classified function names into the following 7 categories:

-   **alllowercase**: softplusfunc
-   **ALLUPPERCASE**: SOFTPLUSFUNC
-   **UpperCamelCase**: SoftPlusFunc
-   **lowerCamelCase**: softPlusFunc
-   **snake\_case**: soft\_plus\_func
-   **dotted.case**: soft.plus.func
-   **other**: sOfTPluSfunc

The last style-element is the line-length. For each R file, we counted the distribution of line-length. In this analysis, the unit of analysis is line.

By not considering line-length, we have studied 10 binary style-elements and one multinomial style-element with 7 categories. Therefore, the possible number of combinations based on these 11 style-elements is: 7 \* 2^10 = 7168.

### Community-specific variations

On top of the overall patterns based on the analysis all functions, the community-specific variations are also studied. In this part of the study, we ask the question: do local patterns of PSV exist in programming communities? To this end, we constructed a dependency graph of CRAN packages by defining a package as a node and a import/suggest relationship as a directed edge. Communities in this dependency graph were extracted using the Walktrap Community Detection Algorithm (Pons & Latapy, 2005) provided by the [igraph package](https://igraph.org/r/).

The 18 largest communities were extracted to study local patterns in PSV.

Results
-------

About the authors
-----------------

References
----------

Bååth, R. (2012). The state of naming conventions in R. The R journal, 4(2), 74-75.

Pons, P., & Latapy, M. (2005, October). Computing communities in large networks using random walks. In International symposium on computer and information sciences (pp. 284-293). Springer, Berlin, Heidelberg.
