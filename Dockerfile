FROM rocker/shiny-verse:4

RUN Rscript -e "install.packages('qualtRics')"
RUN Rscript -e "install.packages('HH')"
RUN Rscript -e "install.packages('ggthemes')"

COPY server.R /srv/shiny-server/
COPY ui.R /srv/shiny-server/