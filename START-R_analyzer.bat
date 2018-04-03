FOR /F "tokens=*" %%g IN ('docker ps -a -q') do (docker stop %%g) 
docker run --rm -p 3838:3838 -v C:\Users\thomas\Desktop\START-R\START-R:/srv/shiny-server newdocker 
