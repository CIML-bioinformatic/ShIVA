# ShIVA – A user-friendly and interactive interface giving biologists control over their single-cell RNA-seq data 

Rudy Aussel <sup>1</sup>, Muhammad Asif <sup>2</sup>, Sabrina Chenag <sup>1</sup>, Sébastien Jaeger <sup>1,2</sup>, Pierre Milpied <sup>1,2</sup> and Lionel Spinelli <sup>1,2,&</sup> 

<sup>1</sup> Centre d’Immunologie de Marseille-Luminy (CIML), Aix Marseille Université, INSERM, CNRS, Marseille, France
<sup>2</sup> Turing Centre For Living Systems (CENTURI), Marseille, France
<sup>&</sup> E-mail:  spinelli_at_ciml.univ-mrs.fr

## Abstract

Single-cell technologies have revolutionised biological research and applications. As they continue to advance, from multi-modal to spatial capture, analysing this data is becoming increasingly complex. For biologists lacking expert data analysis resources, the problem is increasingly crucial, even for the simplest transcriptomic data. While tools to simplify the analysis of these data exist, many still require a good understanding of the analysis workflow to be used.
We propose ShIVA, an interface for the analysis of single-cell RNA-seq and CITE-seq data specifically dedicated to biologists. Intuitive, iterative, documented by video tutorials, ShIVA allows the biologist to follow a robust and reproducible analysis process, to fully explore and quantify his or her dataset, to produce useful figures and tables and to export his or her work to allow more complex analysis performed by experts. 

## Tutorials

A complete series of tutorial videos is available on the CB2M YouTube channel : [CB2M YouTube Channel](https://www.youtube.com/channel/UCJJ3Svi8AY6XGx4Y9r9G3Iw/playlists).

## Installation

ShIVA can easily be installed and run using Docker. You can install it on your own worskstation or laptop. Since single-cell data analysis often require a large amount of RAM, we recommend to use a computer with at least 16 to 32 GB of RAM.

ShIVA can also be installed on any machine and remotely accessed. It means that you can ask to your IT service to install ShIVA on a powerful enough server and access to the ShIVA interface from your workstation or laptop.

### Install ShIVA on you own workstation or laptop

Follow the procedure:

#### On Windows:

1. Install Docker on your system : [Install Docker](https://docs.docker.com/desktop/install/windows-install/)

2. Create a folder for the ShIVA output. For instance, you can create a folder named "shiva" in your Documents folder. To do so, go to the Documents folder and create a folder with name "shiva" in it. Remember the path of this folder (just use CTRL+C on the folder to copy its path). We will call it PATH_TO_OUTPUT below. For instance, you can create the folder "C:\Users\myself\Documents\shiva".

3. To launch ShIVA, 
  + open a command prompt (see [here](https://www.howtogeek.com/235101/10-ways-to-open-the-command-prompt-in-windows-10/))
  + type the following command, replacing the string PATH_TO_OUTPUT by the path of your chosen folder :

```
docker run --rm --name shiva -d -p 3838:3838 -e USER=$(whoami) -v "PATH_TO_OUTPUT":/root/Shiva/Output cb2m/shiva
```

For instance, with the example above, it becomes :

```
docker run --rm --name shiva -d -p 3838:3838 -e USER=$(whoami) -v "C:\Users\myself\Documents\shiva":/root/Shiva/Output cb2m/shiva
```

4. Now, you just have to open your favorite internet browser and type : "http://127.0.0.1:3838". The ShIVA interface will appear in the browser.


#### On MacOS:

1. Install Docker on your system : [Install Docker](https://docs.docker.com/desktop/install/mac-install/)

2. Create a folder for the ShIVA output. For instance, if you want to locate the ShIVA output on /USers/$USER/ShIVA/, use the command (where $USER is the name of your account) :

```
mkdir -p /home/$USER/shiva/
```

This folder path will be called PATH_TO_OUTPUT below.

3. Run the following command, changing the path of the ShIVA output folder (PATH_TO_OUTPUT) to the chosen value: 

```
docker run --rm --name shiva -d -p 3838:3838 -e USER=$(whoami) -v PATH_TO_OUTPUT:/root/Shiva/Output cb2m/shiva
```

for instance, with the example above : 

```
docker run --rm --name shiva -d -p 3838:3838 -e USER=$(whoami) -v /home/$USER/shiva/:/root/Shiva/Output cb2m/shiva
```

4. Now, you just have to open your favorite internet browser and type : "http://127.0.0.1:3838". The ShIVA interface will appear in the browser.


#### On Linux:

1. Install Docker on your system : [Install Docker](https://docs.docker.com/desktop/install/linux-install/)

2. Create a folder for the ShIVA output. For instance, if you want to locate the ShIVA output on /home/$USER/ShIVA/, use the command (where $USER is the name of your account) :

```
mkdir -p /home/$USER/shiva/
```

This folder path will be called PATH_TO_OUTPUT below.

3. Run the following command, changing the path of the ShIVA output folder (PATH_TO_OUTPUT) to the chosen value: 

```
docker run --rm --name shiva -d -p 3838:3838 -e USER=$(whoami) -v PATH_TO_OUTPUT:/root/Shiva/Output cb2m/shiva
```

for instance, with the example above : 

```
docker run --rm --name shiva -d -p 3838:3838 -e USER=$(whoami) -v /home/$USER/shiva/:/root/Shiva/Output cb2m/shiva
```

4. Now, you just have to open your favorite internet browser and type : "http://127.0.0.1:3838". The ShIVA interface will appear in the browser.

### Install ShIVA on a remote server

ShIVA is a Shiny Application. We recommend using ShinyProxy to deploy ShIVA on a server (see [ShinyProxy](https://www.shinyproxy.io/)).

ShinyProxy is a very efficient way to deploy Shiny apps in an shared context. It has built-in functionality for simple or LDAP authentication and authorization, makes securing Shiny traffic (over TLS) a breeze and has no limits on concurrent usage of a Shiny app.

The ShinyProxy configuration is performed through a config file located in /ets/shinyproxy/application.yml. You can find an example of this file in the Documentation/ShinyProxy folder.

Once ShIVA is deployed on a server, it can be accessed from any machine through a web browser using the http://<SERVER_NAME>:<PORT> URL.


