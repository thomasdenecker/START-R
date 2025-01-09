<p align="center"><img src="docs/Images/Logo_START_R.svg" alt="logo" height="150px"></p>

------

[![DOI](https://zenodo.org/badge/127922557.svg)](https://zenodo.org/badge/latestdoi/127922557)

Visit our website : https://thomasdenecker.github.io/START-R/

## Requirements


We use Docker to develop and manage START-R. We invite you to verify that the
following requirements are correctly satisfied before trying to bootstrap the
application:

* [Docker 1.12.6+](https://docs.docker.com/engine/installation/)

> We recommend you to follow Docker's official documentations to install
required docker tools (see links above).To help you, explanatory videos for each
operating system are available [here](https://www.bretfisher.com/installdocker/)

To help you with the installation, you will find below videos on youtube for each system:
- [Mac OS X](https://www.youtube.com/watch?v=mbSsh40_8WM)
- [Windows 10](https://www.youtube.com/watch?v=_9AWYlt86B8)
- [Linux](https://www.youtube.com/watch?v=8Iu5uqby9PY)

**Docker must be on for the duration of START-R use.**

**Important**

Note that the size of the RAM that should be allocated to the Docker depends on
the size of the studied organism genome. START-R can work with data from several
organims. For the human genome, we strongly recommend an increase in allocated memory for Docker.
Otherwise, the risk is an early termination of the analysis that will be incomplete.

A workstation or a laboratory server with 16GB of RAM is therefore well dimensioned.
To increase the allocated memory, go here for
- [Mac OS X](https://docs.docker.com/docker-for-mac/#memory)
- [Windows 10](https://docs.docker.com/docker-for-windows/#advanced)
- [Linux](https://docs.docker.com/config/containers/resource_constraints/#limit-a-containers-access-to-memory)

## Quick start

Have you read the "Requirements" section above?

### START-R project installation

Download the zip file ([here](https://github.com/thomasdenecker/START-R/archive/master.zip)), extract this file and copy the obtained folder where you want on your computer. Note that if you move the folder, the installation procedure will have to be redone.

**Reminder** : Docker must always be switched on for any installation and use of START-R !

#### Windows installation 

**IMPORTANT** : START-R needs Docker. It will only be possible to install on **Windows 10**.

In this folder, you will find a file named INSTALLATION_WINDOWS.bat. By double clicking on it, the installation will begin. This may take a little time depending on the quality of your internet connection. When the installation is completed, a new file will appear. They allow to launch the START-R applications.

#### Mac OsX installation

**In command line**

[Open a terminal](https://www.youtube.com/watch?v=QROX039ckO8) and run these commands:

```
git clone https://github.com/thomasdenecker/START-R.git
cd START-R
sudo ./INSTALLATION_MAC.sh
```

The installation will begin. This may take a little time depending on the quality of your internet connection. When the installation is completed, a new file will appear. They allow to launch the START-R applications. Once the installation is complete, use this command to launch START-R analyzer:
```
./START-R_analyzer.sh
```

and this command to launch START-R viewer
```
./START-R_viewer.sh
```

**NOTE**

You can also double click the file START-R_analyzer.sh and START-R_viewer.sh. In this situation a small manipulation is required (only once). In the Finder, right-click the file START-R_analyzer.sh (idem for START-R_viewer.sh) and select "Open with" and then "Other...".

You can select the application you want the file to be execute with. In this case it should be the Terminal. To be able to select the Terminal, you have to switch from "Recommended Applications" to "All Applications"  (the Terminal.app application can be found in the Utilities folder).

Check "Always Open With" and after clicking OK you should be able to execute you SHELL script by simply double-clicking it.

#### Linux installation

**In command line**

[Open a terminal](https://linuxconfig.org/how-to-open-a-terminal-on-ubuntu-bionic-beaver-18-04-linux) and run these commands:

```
git clone https://github.com/thomasdenecker/START-R.git
cd START-R
sudo ./INSTALLATION_LINUX.sh
```
Once the installation is complete, use this command to launch START-R analyzer:
```
sudo ./START-R_analyzer.sh
```

and this command to launch START-R viewer
```
sudo ./START-R_viewer.sh
```

### START-R application utilisation

Double click on START-R file (Windows / MacOS X) or launch the command line (Linux) and open your internet browser, typing the following url: http://localhost:3838/ for START-R analyzer and http://localhost:3839/ for START-R viewer and it should work.

**NOTE** (MAC users) : You may need to repeat the same manipulation as for the installation file (only once).

If you want to test the START-R suite on specific datasets for a differential analysis, please use the GEO datasets [GSM2111308](https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSM2111308) for U2OS cells and [GSM2111313](https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSM2111313) for K562 cells.

### START-R analyzer results

When an analysis is done with START-R analyzer, all results for this analysis are available in the folder START-R_analyzer/Outputs. The folder name begins with the date of analysis and successive numbers (for example, the first analysis will be named 20190611_1, the second analysis 20190611_2 ...)

## How to use 

To help you in the use of START-R, we have written a [wiki](https://github.com/thomasdenecker/START-R/wiki). If you have any questions or problems, do not hesitate to post an [issue](https://github.com/thomasdenecker/START-R/issues/new/). 

## START-R Viewer: display several profiles

- Collect replication timing profiles (same chromosome: ".SRV" files) in a same folder

- Rename files (example: "chr20_exp1.SRV", "chr20_exp2.SRV", "chr20_exp3.SRV", ...) to avoid same names of files

- In START-R Viewer, go to the folder including files and click on profiles of interest for a same chromosome thanks to the "ctrl" button to select several profiles

- The application is limited to the display of ten profiles, that is to say, ten individual profiles or five differential analyses (5 x 2 profiles = 10 profiles)

- Show comparable profiles on the same graph (example: same type of experiment, same scale, same normalization, ...)

- Don't show a profile not derived from a differential analysis with two profiles compared as part of a differential analysis

## Development

### Launch in debug mode

During development, you will probably need to get all messages (errors, warnings and notifications) in the R terminal. The following command launches the application and generates a log file in the application folder. To find the path to the application, you can look in the launch file.

START-R analyzer
```
docker run -ti --rm -p 3838:3838 -v YOUR_APPLICATION_PATH:/var/log/shiny-server -v YOUR_APPLICATION_PATH/START-R_analyzer:/srv/shiny-server tdenecker/start-r
```

START-R viewer
```
docker run -ti --rm -p 3839:3838 -v YOUR_APPLICATION_PATH:/var/log/shiny-server -v YOUR_APPLICATION_PATH/START-R_viewer:/srv/shiny-server tdenecker/start-r
```

### Connect to a R session

```
docker run -ti --rm -p 3839:3838 -v YOUR_APPLICATION_PATH:/srv/shiny-server  tdenecker/start-r R
```

**Warning**: nothing is saved in this session (package installation, ...)

### Remove folder (Only for linux user) 

To delete an analysis folder, you must use the following command :
```
sudo rm -rf dirName
```
## References
START-R use R packages. You will find below the list of packages and the installed versions in the Docker image: 

- Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie and Jonathan
McPherson (2017). shiny: Web Application Framework for R. R package
version 1.0.5. https://CRAN.R-project.org/package=shiny

- Thomas Lin Pedersen (2016). shinyFiles: A Server-Side File System
Viewer for Shiny. R package version 0.6.2.
https://CRAN.R-project.org/package=shinyFiles

- Dean Attali (2018). shinyjs: Easily Improve the User Experience of
Your Shiny Apps in Seconds. R package version 1.0.
https://CRAN.R-project.org/package=shinyjs

- Winston Chang (2016). shinythemes: Themes for Shiny. R package
version 1.1.1. https://CRAN.R-project.org/package=shinythemes

- Carson Sievert, Chris Parmer, Toby Hocking, Scott Chamberlain,
Karthik Ram, Marianne Corvellec and Pedro Despouy (2017). plotly:
Create Interactive Web Graphics via 'plotly.js'. R package version
4.7.1. https://CRAN.R-project.org/package=plotly

- Dean Attali (2017). colourpicker: A Colour Picker Tool for Shiny and
for Selecting Colours in Plots. R package version 1.0.
https://CRAN.R-project.org/package=colourpicker

- Ritchie, M.E., Phipson, B., Wu, D., Hu, Y., Law, C.W., Shi, W., and
Smyth, G.K. (2015). limma powers differential expression analyses for
RNA-sequencing and microarray studies. Nucleic Acids Research 43(7),
e47.

- Venkatraman E. Seshan and Adam Olshen (). DNAcopy: DNA copy number
data analysis. R package version 1.44.0.

- SNPchip: R classes and methods for SNP array data R.B. Scharpf and G.
Parmigiani and J. Pevsner and I. Ruczinski 2007, Bioinformatics, Vol.
23, 627-628

- Hans W. Borchers (2018). pracma: Practical Numerical Math Functions.
R package version 2.1.4. https://CRAN.R-project.org/package=pracma

- Matt Dowle and Arun Srinivasan (2017). data.table: Extension of
'data.frame'. R package version 1.10.4-3.
https://CRAN.R-project.org/package=data.table

- RStudio and Inc. (2017). htmltools: Tools for HTML. R package version
0.3.6. https://CRAN.R-project.org/package=htmltools

- Marek Walesiak and Andrzej Dudek (2017). clusterSim: Searching for
Optimal Clustering Procedure for a Data Set. R package version
0.47-1. https://CRAN.R-project.org/package=clusterSim

- John Fox and Sanford Weisberg (2011). An {R} Companion to Applied
Regression, Second Edition. Thousand Oaks CA: Sage. URL:
http://socserv.socsci.mcmaster.ca/jfox/Books/Companion

- R Core Team (2015). R: A language and environment for statistical
computing. R Foundation for Statistical Computing, Vienna, Austria.
URL https://www.R-project.org/.

Note: You will find in the application help in the choice of methods based on these packages. These helps are directly extracted from the packages. 

## Citation
If you use START-R project, please cite our paper :

**Efficient, quick and easy-to-use DNA replication timing analysis with START-R suite**

Djihad Hadjadj, Thomas Denecker, Eva Guérin, Su-Jung Kim, Fabien Fauchereau, Giuseppe Baldacci, Chrystelle Maric, Jean-Charles Cadoret

NAR Genomics and Bioinformatics, Volume 2, Issue 2, June 2020, lqaa045, https://doi.org/10.1093/nargab/lqaa045

## Contributing

Please, see the [CONTRIBUTING](CONTRIBUTING.md) file.

## Contributor Code of Conduct

Please note that this project is released with a [Contributor Code of
Conduct](http://contributor-covenant.org/). By participating in this project you
agree to abide by its terms. See [CODE_OF_CONDUCT](CODE_OF_CONDUCT.md) file.

## License

START R is released under the BSD-3 License. See the bundled [LICENSE](LICENSE)
file for details.
