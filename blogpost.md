A Computational Analysis of the Dynamics of R Style Based on 94 Million Lines of Code from All CRAN Packages in the Past 20 Years
================
Chia-Yi Yen, Mia Huai-Wen Chang, Chung-hong Chan
04-07-2019

Please cite this as: Yen, C.Y., Chang, M.H.W., Chan, C.H. (2019) *A Computational Analysis of the Dynamics of R Style Based on 94 Million Lines of Code from All CRAN Packages in the Past 20 Years.* Paper presented at the useR! 2019 conference, Toulouse, France.

The full paper is pending.

Five-sentence Summary
---------------------

-   There are so many programming style variations. R is particularly problematic.
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

One area that can highlight this flexible is the naming conventions. According to the previous research by [Bååth (2012)](https://journal.r-project.org/archive/2012-2/RJournal_2012-2_Baaaath.pdf), there are at least 6 styles and none of the 6 has dominated the scene. There are still some other style-elements that R programmers have freedom to adopt, e.g. whether or not to add spaces around infix operators, use double quotation marks or single quotation marks to denote strings, etc. On one hand, these variations provide programmers with freedom. On the other hand, these variations can confuse new programmers and can have dire effects on programe comprehension. Also, incompatibility between programming styles might also affect reuse, maintainability (Elish & Offutt, 2002) and open source collabration (Wang & Hahn, 2017).

Various efforts to standardize the programming style (e.g. [Google's R Style Guide](https://google.github.io/styleguide/Rguide.xml), The [Tidyverse Style Guide](https://style.tidyverse.org/), [Bioconductor Coding Style](https://www.bioconductor.org/developers/how-to/coding-style/), etc.) are available. These style guides are based on normative assessment of code quality, e.g. style-elements that improve programe comprehension (Oman & Cook, 1990). However, we argue that we should first study the current situation, and preferably, the historical development, of programming style variations (PSV) to supplement these standardization efforts. We have undertaken such a task, so that the larger R community can have a baseline to evaluate the effectiveness of those standardization efforts. Also, we can have better understanding on the factors driving increase and decrease in PSV historically, such that more effective efforts can be formulated.

Analysis
--------

### Data Source

In January 2019, we cloned a local mirror of CRAN using the rsync method suggested by the [CRAN Mirror HOWTO](https://cran.r-project.org/mirror-howto.html). In our local mirror, it contains all package as tarball files (.tar.gz). By all package, we mean all packages actively listed online on the CRAN websites and all packages delisted for whatever reasons e.g. no maintainer, etc. In this analysis, we include all packages, including those delisted.

In order to faciliate the analysis, we have developed the [baaugwo package](https://github.com/chainsawriot/baaugwo) to extract all R sourcecode and metadata from these tarballs. In this study, only the source code from the */R* directory of each tarball file is included. We have also archived the meta data from the *DESCRIPTION* and *NAMESPACE* files from the tarballs.

In order to cancel out the effect of multiple submissions in a year by one particular package, we have applied the *"one-submission-per-year"* rule to randomly selected only one submission from a year for each package. Unless explicitly notice, we present below the analysis of this "one-submission-per-year" sample. Similarly, unless explicitily notice, the unit of the analysis is **exported function**. The study period of this study is from 1998 to 2018.

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

The last style-element is the line-length. For each R file, we counted the distribution of line-length. In this analysis, the unit of it is line.

By not considering line-length, we have studied 10 binary style-elements and one multinomial style-element with 7 categories. Therefore, the possible number of combinations based on these 11 style-elements is: 7 \* 2^10 = 7168.

### Community-specific variations

On top of the overall patterns based on the analysis all functions, the community-specific variations are also studied. In this part of the study, we ask the question: do local patterns of PSV exist in programming communities? To this end, we constructed a dependency graph of CRAN packages by defining a package as a node and a import/suggest relationship as a directed edge. Communities in this dependency graph were extracted using the Walktrap Community Detection Algorithm (Pons & Latapy, 2005) provided by the [igraph package](https://igraph.org/r/).

The 18 largest communities were extracted to study local patterns in PSV.

Results
-------

We studied more than 94 Million lines of code from 15530 unique packages. In total, 1898142 exported functions were studied. Figure 1 displays the evolution of the 10 binary style-elements from 1998 to 2008. Some style-elements have a very clear trends towards an majority-vs-minority pattern, e.g. fx\_closecurly, fx\_semi, fx\_t\_f and fx\_tab. Some styles-elements are instead trending towards a divergence from a previously majority-vs-minority pattern, e.g. fx\_assign, fx\_commas, fx\_infix, fx\_integer, fx\_opencurly and fx\_singleq. There are two style-elements that deserve specical scrutiny. Firstly, the variation in fx\_assign is a clear example illustrating the effect of introducing a new language element by the R Development Core Team. The introduction of the language feature (= as assignment operator) in R 1.4 (Chambers, 2001) conicides with the taking off in popularity of such style-element since 2001. Up to now, around 20% of exported functions use such style.

Secondly, the popularity of fx\_opencurly shows how a previously established majority style (~80% in late 90s) slowly reduced into a minority, but still very prominant, style (~30% in late 10s).

![Evolution in popularities of 10 binary style-elements from 1998 to 2018.](blogpost_files/figure-markdown_github/Figure%201-1.png)

Similarly, the evolution of different naming conventions is shown in figure 2. This analysis can best be used to illustrate the effect of style-guides. According to Bååth(2012), dotted style is very specific to R programming. This style is the most dominant style in the early days of CRAN. However, multiple style guides advise against the use of dotted style universially and thus a significant decline trend is observed. snake\_case and UpperCamelCase are the styles endorsed by the Tidyverse Style Guide and the Google's R Style Guide respectively. These two styles see an increasing trend since the 10s, although the growth of snake\_case is relatively more impressive. To our surprise, lowerCamelCase, a style not endorsed by any style-guide, is currently the most popular naming convention (22.5% in 2018). However, its reign might soon be dethroned by snake\_case (21.5% in 2018) in the near future.

![Evolution in popularities of 7 naming conventions from 1998 to 2018.](blogpost_files/figure-markdown_github/Figure%202-1.png)

The evolution of line lengths is tricky to be visualised in 2-D space. We have prepared an animation to visualise the change in line distribution over the span of 20 years. In this paper, figure 3 shows the snapshot of the change in line length distribution in the range of 40 characters to 100 characters. In general, the programmers of newer packages write with lesser number of characters per line. Similar to previous analyses with Python programs (VanderPlas, 2017), artificial peaks corresponding to recommendations from either style-guides, linters and editor settings are also observed in our analysis. In the most 2018, the artificial peak of 80 characters (recommended by most of the style-guides and linters such as lintr) is more pronounced for lines with comments but not those with actual code.

![Change in line length distribution: 2003, 2008, 2013 and 2018.](blogpost_files/figure-markdown_github/Figure%203-1.png)

### Communitiy-based variations

Using the aforementioned community detection algorithm of the dependency graph, 19 large communities were extracted. These communities are named by their applications. Table 1 lists the details of these communities.

Using naming convention as an example, there are local patterns in PSV (Figure 4). For example, snake\_case is the most popular naming convention in the "RStudio-related" communitiy as expected because it is the naming convention endorsed by the Tidyverse Style-guide. However, no function exported by the packages from "Time, Date, and Money" community uses such convention.

![Community-specific distribution of naming conventions among 18 large communities](blogpost_files/figure-markdown_github/Figure%204-1.png)

For the binary style-elements, local patterns are also observed (Figure 5). The most salient pattern is the "Java" and "Sparse Matrix" communities exceptional high level of tab-indentation usage, probably due to influence from Java or Matlab. Also, the exceptional high level in usage of open curly on its own line for the "Graphics" is interesting too.

![Community-specific distribution of naming conventions among 18 large communities](blogpost_files/figure-markdown_github/Figure%205-1.png)

Discussion
----------

In this study, we study the PSV in CRAN packages across two dimensions: 1) temporal variations: the longitudinal changes in popularities of various style-elements, and 2) community-based variations: the variations among communities of packages. From our analysis, we suggest three possible factors that might drive PSV: effect of style-guides (trending of naming conventions endorsed by RStudio and Google), effect of introducing a new language feature (trending of = usage as assignments) and effect of editors (the domainance of 80-character line limit).

From a policy recommendation standpoint, our study provides important insight for the R Development Core Team and other stakeholders who care about PSV. Firstly, introduction of new language can have very longlasting effect on PSV. "Assignments with the = operator" is a feature that introduced by the R Development Core Team to *"increase compatibility with S-Plus (as well as with C, Java, and many other languages)."* (Chamber, 2001) This might be a good intention but it has an unintended consequence of introducing a very persistent PSV that two major style-guides (Tidyverse and Google) consider to be a bad style.

Secondly, although style-guides, linters and editors are important homogenizers of PSV, we observe very strong path dependency in PSV. As indicated by the local patterns we found in some communities, some package developers are very resistent to these homogenizers and maintain their own styles. Having said so, we are not accusing those developers for not following the latest trends in programming styles. Instead, they follow one of the golden rules of programming: "if it ain't broke don't fix it". Again, from a policy recommendation standpoint, the existence of local patterns in PSV suggests there are many blind spots to the previous efforts in addressing PSV. Style-guide owners may consider community outreach to promote their endorsed styles.

Our analysis also opens up an open question: should R adopt an official style-guide akin the PEP-8 (van Rossum, et al. 2001) from Python Software Foundation? There are of course pros and cons of adopting such official style-guide. As written by Christiansen (1998), *"Style can easily become a religious issue."* It is not our intention to meddle in this "religious issue". If such an effort would be undertaken by someone else, we suggest the following **consensus-based style**. We must stress here that this **consensus-based style** is the most popular style from our analysis. It has no gurantee of improving clarity or comprehensibility. The following is an example of a function written in such consensus-based style.

``` r
softplusFunc <- function(value, leaky = FALSE) {
    if (leaky) {
        warnings("using leaky RELU!")
        return(ifelse(value > 0, value, value * 0.01))
    }
    return(log(1 + exp(value)))
}
```

In essense,

-   Use lowerCamel or snake case
-   Use &lt;- to assign, don't use =
-   Add a space after commas
-   Don't use T / F, use TRUE / FALSE
-   Put open curly bracket on same line then a newline
-   Use double quotation mark for strings
-   Add spaces around infix operators
-   Don't terminate lines with semicolon
-   Be liberal about typing integers
-   Put close curly bracket on its own line
-   Don't use tab to intend

Although enforcing a consistent style can improve open source collaboration (Wang & Hahn, 2017), one must also bear in mind that these rules need to be adjusted sometimes. For example, using spaces instead of tabs for indentation can make code not assessible to visually impaired programmers (Mosal, 2019).

About the authors
-----------------

Ms **Chia-Yi Yen** is currently a finance PhD student at the Mannheim Business School. Prior to her doctoral studies, she worked as a financial engineer in hedge fund industry and a data science consultant for financial institutions. Besides, she is also listed as one of the inventors of three Taiwanese patents (under review) of innovative Fintech applications, a translator of two children’s novels on computer science, the co-founder of R-Ladies Taipei — a local community helping women learn data science and R language — and one of the Microsoft Most Valuable Professionals from January 2017 to June 2019.

Dr **Chung-hong Chan** is a research associate at the Institue of Media and Communication Studies and project staff at the Mannheim Center for European Social Research, the University of Mannheim. He earned his doctorate in communication studies at the University of Hong Kong and studied biostatistics and epidemiology at the Chinese University of Hong Kong. Previously, he worked in a hospital as a statistician for 10 years. His research interests are cyberbalkanization, polarization, platform interventions (e.g. online censorship), text mining, social network analysis and meta-analysis/metaregression.

References
----------

Bioconductor. (2015) Coding Style. <http://bioconductor.org/developers/how-to/coding-style/>

Bååth, R. (2012). The state of naming conventions in R. The R journal, 4(2), 74-75.

Chambers, J. (2001). Assignments with the = Operator. <http://developer.r-project.org/equalAssign.html>

Christiansen, T. (1998). Perl Style: Everyone Has an Opinion. <https://www.perl.com/doc/FMTEYEWTK/style/slide1.html/>

Csardi, G., & Nepusz, T. (2006). The igraph software package for complex network research. InterJournal, Complex Systems, 1695(5), 1-9.

Elish, M. O., & Offutt, J. (2002). The adherence of open source java programmers to standard coding practices.

Google's R Style Guide. <https://google.github.io/styleguide/Rguide.xml>

Hester, J., Angly, F. (2019). A 'Linter' for R Code. <https://github.com/jimhester/lintr/>

Mosal, C. (2019). Nobody talks about the real reason to use Tabs over Spaces. <https://www.reddit.com/r/javascript/comments/c8drjo/nobody_talks_about_the_real_reason_to_use_tabs/>

Oman, P. W., & Cook, C. R. (1990). A taxonomy for programming style. Proceedings of the 1990 ACM Annual Conference on Cooperation - CSC ’90. <doi:10.1145/100348.100385>

Pons, P., & Latapy, M. (2005, October). Computing communities in large networks using random walks. In International symposium on computer and information sciences (pp. 284-293). Springer, Berlin, Heidelberg.

Wickham, H. (2017). The tidyverse style guide. <https://style.tidyverse.org/>

van Rossum, G., Warsaw, B., Coghlan, N. (2001). PEP 8 -- Style Guide for Python Code. <https://www.python.org/dev/peps/pep-0008/>

VanderPlas, J. (2017). Exploring Line Lengths in Python Packages. <https://jakevdp.github.io/blog/2017/11/09/exploring-line-lengths-in-python-packages/>

Wang, Z., & Hahn, J. (2017). The Effects of Programming Style on Open Source Collaboration.
