docker pull tdenecker/start-r

echo FOR /F "tokens=*" %%%%g IN ('docker ps -a -q') do (docker stop %%%%g) > START-R_analyzer.bat
echo docker run --rm -p 3838:3838 -v %CD%\START-R:/srv/shiny-server newdocker >> START-R_analyzer.bat

echo FOR /F "tokens=*" %%%%g IN ('docker ps -a -q') do (docker stop %%%%g) > START-R_viewer.bat
echo docker run --rm -p 3838:3838 -v %CD%\START-R_viewer:/srv/shiny-server newdocker >> START-R_viewer.bat
