FROM minizinc/minizinc

WORKDIR /usr/src/app

RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get install software-properties-common python3 python3-pip nginx -y && \
    apt-add-repository ppa:swi-prolog/stable && \
    apt-get update && \
    apt-get install swi-prolog python3.12-venv -y && \
    python3 -m venv venv

ENV PATH="./venv/bin:$PATH"

COPY requirements.txt .
RUN pip install --upgrade pip && \
    pip install -r requirements.txt

COPY . .

EXPOSE 5000
CMD ["./venv/bin/gunicorn", "--bind=0.0.0.0:5000", "--workers=4", "--timeout=0", "app:app"]
