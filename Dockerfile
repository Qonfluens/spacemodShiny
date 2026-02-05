# Étape 1 : image de base
FROM rocker/shiny:latest

# Étape 2 : installer les dépendances système
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libudunits2-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Étape 2b : connexion GitHub
RUN echo "options(download.file.method = 'libcurl', download.file.extra = '-L')" >> /usr/local/lib/R/etc/Rprofile.site

# Étape 3 : Installer spacemodR
RUN R -e "install.packages('remotes'); remotes::install_github('Qonfluens/spacemodR', upgrade = 'never')"

# Étape 4 : Installer les dépendances du projet Shiny
COPY DESCRIPTION NAMESPACE /app/
WORKDIR /app
RUN R -e "remotes::install_deps(dependencies = TRUE, upgrade = 'never')"

# Étape 5 : Copier le reste du code et installer l'app
COPY . /app
RUN R CMD INSTALL .

# Étape 6 : exposer le port et lancer
EXPOSE 3838
CMD ["R", "-e", "spacemodShiny::run_app(host = '0.0.0.0', port = 3838)"]
