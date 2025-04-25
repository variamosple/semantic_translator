# Variamos Semantic Translator

## Requirements
- Python 3.12
- Docker (Latest version if possible).

## Instructions

To run the Semantic Translator you can run the following docker commands:
```bash
docker build -t semantic_translator . # Creates the docker image
docker run -d -p 5001:5001 --name semantic_translator semantic_translator # Creates the container running the docker image
```

The project will run on the following URL:
- http://localhost:5001/query


Once you have run these commands and you have the Semantic Translator project running, if you want to make changes in the project to be reflected in the docker container you can run the following command:
```bash
docker stop semantic_translator && docker rm semantic_translator && docker build -t semantic_translator . && docker run -d -p 5001:5001 --name semantic_translator semantic_translator
```

This command will stop the current container, remove the previous image and will recreate new ones based on the cached libraries already installed on the previous image. It should take less time that the first time you ran the build command.

