FROM ubuntu:16.04

# Preparing to install R
RUN \
  apt-get update && \
  apt-get install sudo -y && \
  sudo -s && \
  echo "deb http://cran.rstudio.com/bin/linux/ubuntu xenial/" | tee -a /etc/apt/sources.list && \
  gpg --keyserver keyserver.ubuntu.com --recv-key E084DAB9 && \
  gpg -a --export E084DAB9 | apt-key add - && \
  apt-get update

# Install everything that is needed for R to run our scripts and additionally installing packages inside R
RUN \
  apt-get install -y r-base r-base-dev libcurl4-openssl-dev libssl-dev libgsl-dev r-cran-rgl && \
  Rscript -e 'install.packages("xts", repos = "http://cran.us.r-project.org")' && \
  Rscript -e 'install.packages("imputeTS", repos = "http://cran.us.r-project.org")' && \
  Rscript -e 'install.packages("tseries", repos = "http://cran.us.r-project.org")' && \
  Rscript -e 'install.packages("forecast", repos = "http://cran.us.r-project.org")' && \
  Rscript -e 'install.packages("arfima", repos = "http://cran.us.r-project.org")' && \
  Rscript -e 'install.packages("rugarch", repos = "http://cran.us.r-project.org")' && \
  Rscript -e 'install.packages("tsoutliers", repos = "http://cran.us.r-project.org")' && \
  Rscript -e 'install.packages("reshape2", repos = "http://cran.us.r-project.org")' && \
  Rscript -e 'install.packages("Rssa", repos = "http://cran.us.r-project.org")' && \
  Rscript -e 'install.packages("e1071", repos = "http://cran.us.r-project.org")' && \
  Rscript -e 'install.packages("influxdbr", repos = "http://cran.us.r-project.org")' && \
  Rscript -e 'install.packages("mongolite", repos = "http://cran.us.r-project.org")'

# Making necessary folders
RUN \
  mkdir /home/ForecastingService && \
  cd /home/ForecastingService

WORKDIR "/home/ForecastingService"

# Copying the necessary data from the local folderto the image
COPY ForecastingService.R .
COPY PredictionCore.R .
COPY DataPreprocessing.R .
COPY ForecastMetric.R .
COPY ARIMAForecast.R .
COPY ETSForecast.R .
COPY LinearRegressionForecast.R .
COPY SSAForecast.R .
COPY SVMForecast.R .
COPY test.csv .

CMD ["bash"]

# Testing with commands in CLI:
# [no InfluxDB - results in a .RData file in the same folder] Rscript ForecastingService.R --target=test.csv --starttime=1518524056 --type=BATCH
# [with InfluxDB - results in a database in Influx] Rscript ForecastingService.R --target=test.csv --starttime=1518524056 --type=SINGLE --client=client1 --predsteps=10 --influx.dbhost=localhost:8086 --influx.dbuser=root --influx.dbpassword=root --mongo.dbhost=localhost --mongo.dbuser=admin --mongo.dbpassword=admin
