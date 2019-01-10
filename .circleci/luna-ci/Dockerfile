FROM fpco/stack-build:lts-12.16
RUN apt-get update -y &&\
    apt-get install build-essential software-properties-common -y &&\
    add-apt-repository ppa:ubuntu-toolchain-r/test -y &&\
    apt-get update -y &&\
    apt-get install gcc-8 g++-8 -y

    # update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-8 60 --slave /usr/bin/g++ g++ /usr/bin/g++-8