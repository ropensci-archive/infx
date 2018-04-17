
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- build with rmarkdown::render("README.Rmd") -->
[infx](https://nbenn.github.io/infx)
====================================

[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://img.shields.io/badge/lifecycle-maturing-blue.svg) [![Travis-CI Build Status](https://travis-ci.org/nbenn/infx.svg?branch=master)](https://travis-ci.org/nbenn/infx) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/nbenn/infx?branch=master&svg=true)](https://ci.appveyor.com/project/nbenn/infx) [![Coverage status](https://codecov.io/gh/nbenn/infx/branch/master/graph/badge.svg)](https://codecov.io/github/nbenn/infx?branch=master)

Access to [InfectX](http://www.infectx.ch)/[TargetInfectX](https://www.targetinfectx.ch) screening data from R. A browser-based view of the data is available [here](http://www.infectx.ch/databrowser).

Installation
------------

You can install [infx](https://nbenn.github.io/infx) from Github with:

``` r
# install.packages("devtools")
devtools::install_github("nbenn/infx", build_vignettes = TRUE)
```

InfectX
-------

[InfectX](http://www.infectx.ch) and its successor project [TargetInfectX](https://www.targetinfectx.ch) are large-scale high throughput screening experiments focused on the human infectome of a set of viral and bacterial pathogens. In order to identify host-provided components involved in pathogen entry and host colonization, several RNA interference screens were carried out on HeLa cells, using siRNA libraries from vendors including Dharmacon, Quiagen and Ambion. Of the many performed screens, currently the data of kinome-wide screens for five bacterial pathogens (*Bartonella henselae*, *Brucella abortus*, *Listeria monocytogenes*, *Salmonella* typhimurium, and *Shigella flexneri*) and three viruses (Adenovirus, Rhinovirus, and *Vaccinia virus*) is publicly available[1]. Additionally, several genome-wide screens will follow suit in the coming months.

All collected data, including raw imaging data, [CellProfiler](http://cellprofiler.org) derived feature data and infection scoring at single cell resolution, alongside extensive metadata, is hosted by the laboratory information management system [openBIS](https://openbis.elnlims.ch). This R package provides access to the openBIS [JSON-RPC API](https://wiki-bsse.ethz.ch/display/openBISDoc1304/openBIS+JSON+API), enabling listing of data organization objects, searching for and downloading of data sets.

OpenBIS
-------

Only a brief introduction on how to work with openBIS is given here. For more in-depth information on how data is organized in openBIS and how it can be accessed using this package, please refer to the vignette ["Introduction to infx"](https://nbenn.github.io/infx/articles/infx-intro.html). For an extensive look at what parts of the API are currently implemented and how to extend the package to support further functionality, have a look at the vignettes ["OpenBIS API coverage"](https://nbenn.github.io/infx/articles/openbis-api.html) and ["JSON object handling"](https://nbenn.github.io/infx/articles/json-class.html). Documentation of exported functions is available from within the R help system or from [here](https://nbenn.github.io/infx/reference/index.html).

For every API call, a valid login token is required. Tokens can be created using [`login_openbis()`](https://nbenn.github.io/infx/reference/login.html) and tested for validity with [`is_token_valid()`](https://nbenn.github.io/infx/reference/login.html).

``` r
tok <- login_openbis(user = "rdgr2014",
                     pwd = "IXPubReview")

is_token_valid(tok)
#> [1] TRUE
```

Using the valid login token, openBIS can now be queried, for example for a list of all projects that are available to the given user, using [`list_projects()`](https://nbenn.github.io/infx/reference/list_projects.html).

``` r
projects <- list_projects(tok)
projects[[1]]
#> █─Project 
#> ├─permId = 20130710131815818-2788266 
#> ├─spaceCode = INFECTX_PUBLISHED 
#> ├─code = ADENO_TEAM 
#> ├─description =  
#> ├─registrationDetails = █─EntityRegistrationDetails 
#> │                       └─... 
#> └─id = 39
```

Finally, the login token should be destroyed, using [`logout_openbis()`](https://nbenn.github.io/infx/reference/login.html).

``` r
logout_openbis(tok)
is_token_valid(tok)
#> [1] FALSE
```

While this client has been thoroughly tested with the openBIS instance hosted by InfectX and certain aspects are geared towards high content screening application of openBIS, it is in no way limited to usage with InfectX data. Any method that issues an API call should be able to accept the `host_url` argument and use it for constructing the API endpoint url. As a small example, available projects are listed for the demo openBIS instance offered by the development team.

``` r
tok <- login_openbis("test_observer", "test_observer",
                     host_url = "https://openbis.elnlims.ch")

projects <- list_projects(tok, host_url = "https://openbis.elnlims.ch")
projects[[1]]
#> █─Project 
#> ├─permId = 20150126115738287-33 
#> ├─spaceCode = MATERIALS 
#> ├─code = BACTERIA 
#> ├─description =  
#> ├─registrationDetails = █─EntityRegistrationDetails 
#> │                       └─... 
#> └─id = 3

logout_openbis(tok, host_url = "https://openbis.elnlims.ch")
```

Acknowledgments
---------------

This work is partially funded by [SystemsX.ch](http://www.systemsx.ch), the Swiss Initiative for Systems Biology via grants 51RT-0\_126008 and 51RTP0\_151029 for the Research and Technology Development (RTD) projects [InfectX](https://infectx.ch) and [TargetInfectX](https://www.targetinfectx.ch) respectively. Further funding is provided by the [Seminar for Statistics](https://www.math.ethz.ch/sfs) at ETH Zurich.

[1] [*BMC Genomics* 2014 **15**:1162](https://doi.org/10.1186/1471-2164-15-1162)
