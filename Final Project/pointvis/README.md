# pointvis - Package installation details (how-to)
This folder contains the pacakge 'pointvis', which presents three point data visualization functions.
To install the package for R please type in:
    devtools::install_github("martabc/pointvis")
in R or RStudio, and all necessary files will download locally. To load the library once install, please type:
    library(pointvis)
    
Only then will you be able to use any of the three functions (namely, displayData(), showClusters(), and heatmapTrend() ).
Depending on the version of R installed, some of the libraries may not auto install. If that's the case and you receive a warning upon loading the library, you may also try to install the necessary, missing package manually by typing something such as is suggested below (more information at https://support.rstudio.com/hc/en-us/articles/219949047-Installing-older-versions-of-packages):
    require(devtools)
    install_version("package", version = "x.x.x", repos = "http://cran.us.r-project.org")

Or, alternatively, you may search for the development version of the package in question via Google or GitHub, and download again via devtools with something such as follows (this example requires that you know the original username or account name of the user publishing the package):
    devtools::install_github('bhaskarvk/leaflet.extras')
    
From testing the installation of the pacakge, I only predict two of them to be potentially problematic based on current R and RStudio versions:  'leaflet.extras' and 'rMaps'. To install rMaps you may type:
    devtools::install_github('ramnathv/rMaps')
   

Hope this helps!
