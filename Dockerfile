FROM minizinc/minizinc

WORKDIR /usr/src/app

RUN apt-get -y upgrade

RUN apt-get update && apt-get upgrade -y

RUN apt-get install software-properties-common python3 python3-pip nginx -y

RUN apt-add-repository ppa:swi-prolog/stable

RUN apt-get update

RUN apt-get install swi-prolog -y

# Creating virtual environment to avoid compatibility errors with newer Docker versions.
RUN apt install python3.12-venv -y

RUN python3 -m venv venv

ENV PATH="./venv/bin:$PATH"

COPY requirements.txt .
COPY .env .

RUN pip install --default-timeout=1000 -r requirements.txt

COPY . .

EXPOSE 5000
CMD ["./venv/bin/gunicorn", "--bind=0.0.0.0:5000", "--workers=16", "app:app" , "--timeout 0" ]
# CMD ["./venv/bin/flask", "run", "--port=5000", "--host=0.0.0.0"]
