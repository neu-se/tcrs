FROM rocker/shiny-verse:4

RUN Rscript -e "install.packages('qualtRics')"
RUN Rscript -e "install.packages('HH')"
RUN Rscript -e "install.packages('ggthemes')"
RUN Rscript -e "install.packages('googlesheets4')"

COPY server.R /srv/shiny-server/
COPY ui.R /srv/shiny-server/