FROM minizinc/minizinc



WORKDIR /usr/src/app

RUN apt-get update && apt-get upgrade -y

RUN apt-get install software-properties-common python3 python3-pip -y

RUN apt-add-repository ppa:swi-prolog/stable

RUN apt-get update

RUN apt-get install swi-prolog -y

RUN pip install flask minizinc swiplserver



COPY . .

EXPOSE 5000
CMD ["flask", "run", "--host=0.0.0.0"]
