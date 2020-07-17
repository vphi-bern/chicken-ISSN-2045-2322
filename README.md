# chicken-ISSN-2045-2322
R-Shiny app for research presented in the research paper "Finding hens in a haystack: Consistency of movement patterns within and across individual laying hens maintained in large groups", DOI "10.1038/s41598-018-29962-x"

### Abstract
We sought to objectively quantify and compare the recorded movement and location patterns of laying hens within a commercial system. Using a custom tracking system, we monitored the location within five zones of a commercial aviary for 13 hens within a flock of 225 animals for a contiguous period of 11 days. Most hens manifested a hen-specific pattern that was (visually) highly consistent across days, though, within that consistency, manifested stark differences between hens. Three different methods were used to classify individual daily datasets into groups based on their similarity: (i) Linear Discriminant Analysis based on six summary variables (transitions into each zone) and total transitions; (ii) Hierarchical Clustering, a na&#239;ve clustering analysis technique, applied to summary variables and iii) Hierarchical Clustering applied to dissimilarity matrices produced by Dynamic Time Warping. The three methods correctly classified more than 85% of the hen days and provided a unique means to assess behaviour of a system indicating a considerable degree of complexity and structure. We believe the current effort is the first to document these location and movement patterns within a large, complex commercial system with a large potential to influence the assessment of animal welfare, health, and productivity.

### How to cite
Rufener, C., [Berezowski, J.](http://www.vphi.ch/about_us/team/dr_berezowski_john/index_eng.html), [Maximiano Sousa, F.](http://www.vphi.ch/about_us/team/maximiano_alves_de_sousa_filipe_miguel/index_eng.html), Abreu, Y., Asher, L., [Toscano, M.J.](https://www.tierschutz.vetsuisse.unibe.ch/about_us/personnel/dr_toscano_michael_j/index_eng.html), 2018. Finding hens in a haystack: consistency of movement patterns within and across individual laying hens maintained in large groups. Sci. Rep. 8. doi: [10.1038/s41598-018-29962-x](https://doi.org/10.1038/s41598-018-29962-x)

### About the app
An online version of this app you can see it [here](https://vphi-bern.shinyapps.io/chickens/).

This app was built using [R](http://www.r-project.org/) and [Shiny](http://shiny.rstudio.com/). All graphs and analysis were made with [dplyr](https://cran.rstudio.com/web/packages/dplyr/index.html), [highcharter](https://cran.r-project.org/web/packages/highcharter/index.html), [sparkline](https://cran.r-project.org/web/packages/sparkline/index.html) and [visNetwork](https://cran.r-project.org/web/packages/visNetwork/index.html) packages.

### How to run it
```R
#### First make sure you have all the necessary packages installed
install.packages(c("shiny", "shinydashboard", "dygraphs", "shinyWidgets", "highcharter", "dplyr", "xts", "purrr", "htmltools", "stringr", "shinyjs", "formattable", "sparkline", "lubridate", "visNetwork", "viridis"))

library(shiny)

#### Easiest way is to use runGitHub
runGitHub("chicken-ISSN-2045-2322", "vphi-bern")

#### Run a tar or zip file directly
runUrl("https://github.com/vphi-bern/chicken-ISSN-2045-2322/archive/master.zip")
```

Or you can clone the git repository, then use `runApp()`:
```R
# First clone the repository with git. If you have cloned it into
# ~/chicken-ISSN-2045-2322, first go to that directory, then use runApp().
setwd("~/chicken-ISSN-2045-2322")
runApp()
```

### [Veterinary Public Health Institute (VPHI)](http://www.vphi.ch), [Universit&#228;t Bern](https://www.unibe.ch/index_eng.html), &#169; All rights reserved
