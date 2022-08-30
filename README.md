# ShIVA – A user-friendly and interactive interface giving biologists control over their single-cell RNA-seq data 

Rudy Aussel^1^, Muhammad Asif^2^, Sabrina Chenag^1^, Sébastien Jaeger^1,2^, Pierre Milpied^1,2^ and Lionel Spinelli^1,2,&^ 

1 Centre d’Immunologie de Marseille-Luminy (CIML), Aix Marseille Université, INSERM, CNRS, Marseille, France

2 Turing Centre For Living Systems (CENTURI), Marseille, France

& E-mail:  spinelli_at_ciml.univ-mrs.fr

## Abstract

Single-cell technologies have revolutionised biological research and applications. As they continue to advance, from multi-modal to spatial capture, analysing this data is becoming increasingly complex. For biologists lacking expert data analysis resources, the problem is increasingly crucial, even for the simplest transcriptomic data. While tools to simplify the analysis of these data exist, many still require a good understanding of the analysis workflow to be used.
We propose ShIVA, an interface for the analysis of single-cell RNA-seq and CITE-seq data specifically dedicated to biologists. Intuitive, iterative, documented by video tutorials, ShIVA allows the biologist to follow a robust and reproducible analysis process, to fully explore and quantify his or her dataset, to produce useful figures and tables and to export his or her work to allow more complex analysis performed by experts. 

## Installation

ShIVA can easily be installed and run using Docker. Follow the procedure:

### On Windows:

1. Install Docker on your system : [Install Docker](https://docs.docker.com/desktop/install/windows-install/)

2. Create a folder for the ShIVA output. For instance, if you can create a folder named "shiva" in your Documents. To do so, go to the Documents folder and create a folder with name "shiva" in it. Remeber the path of this folder. We will call it PATH_TO_OUTPUT below.

3. To launch ShIVA, you have two solutions:

  + Use the Docker Dashboard (see the [tutorial](https://docs.docker.com/get-started/) ) to set the following parameter:
  
       image name : cb2m/shiva
       
       port: 3838
       
       volume: /root/Shiva/Output:PATH_TO_OUTPUT
       


  + Use the command line:

* open a command prompt (see [here](https://www.howtogeek.com/235101/10-ways-to-open-the-command-prompt-in-windows-10/))
* type the command 

'''{bash}
docker run --name shiva -d -p 3838:3838 -e USER=$(whoami) -v /root/Shiva/Output:/home/$USER/ShIVA/ cb2m/shiva
'''

### On Linux:

1. Install Docker on your system : [Install Docker](https://docs.docker.com/engine/install/)
2. Create a folder for the ShIVA output. For instance, if you want to locate the ShIVA output on /home/$USER/ShIVA/, use the command:

'''{bash}
mkdir -p /home/$USER/ShIVA/
'''

The folder path will be called PATH_TO_OUTPUT below

3. Run the command, changing if required the path of the ShIVA output folder (PATH_TO_OUTPUT) to the chosen value: 

'''{bash}
docker run --name shiva -d -p 3838:3838 -e USER=$(whoami) -v /root/Shiva/Output:/PATH_TO_OUTPUT cb2m/shiva
'''

for instance : 

'''{bash}
docker run --name shiva -d -p 3838:3838 -e USER=$(whoami) -v /root/Shiva/Output:/home/$USER/ShIVA/ cb2m/shiva
'''

## Tutorials

A complete series of tutorial videos are available on the CB2M YouTube channel : [CB2M YouTube Channel](https://www.youtube.com/channel/UCJJ3Svi8AY6XGx4Y9r9G3Iw).

