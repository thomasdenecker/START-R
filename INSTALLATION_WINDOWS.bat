
docker pull tdenecker/start-r
echo docker run --rm -p 3838:3838 -v %CD%\START-R_analyzer:/srv/shiny-server tdenecker/start-r >> START-R_analyzer.bat
echo docker run --rm -p 3839:3838 -v %CD%\START-R_viewer:/srv/shiny-server tdenecker/start-r >> START-R_viewer.bat

mkdir %CD%\START-R_analyzer\Outputs
