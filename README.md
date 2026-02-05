# spacemodShiny





## Deployment with Docker

The application can be easily deployed using Docker. A build-and-run script is provided:

```bash
./docker-run.sh
```

or

```bash
docker build -t spacemodshiny .
# look within it
docker run -it spacemodshiny /bin/bash
# run local browser
docker run -d -p 3838:3838 spacemodshiny
```

```bash
docker ps
docker logs -f <CONTAINER-ID>
```

Push to Scaleway registry:

```bash
docker login rg.fr-par.scw.cloud/shiny-rgtry
docker tag spacemodshiny rg.fr-par.scw.cloud/shiny-rgtry/spacemodshiny:latest
docker push rg.fr-par.scw.cloud/shiny-rgtry/spacemodshiny:latest
```
