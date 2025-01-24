# Usa una imagen base de Python
FROM python:3.9-slim

# Instala las dependencias del sistema necesarias
RUN apt-get update && apt-get install -y \
    build-essential libffi-dev libssl-dev curl procps \
    && apt-get clean

# Establece el directorio de trabajo
WORKDIR /aplicacion

# Copia los archivos del proyecto al contenedor
COPY . /aplicacion

# Instala las dependencias de Python
RUN pip install Flask requests flask-restx

# Exponer el puerto en el que se ejecutará la aplicación
EXPOSE 5000

# Comando para iniciar la aplicación
CMD ["python", "flaskAPI.py"]

