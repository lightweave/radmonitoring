# radmonitoring
This project started for Monte-Carlo simulation of telescope for space radiation conditions monitoring.
It based on Geant4 libraries and R packages.

# HOW to RUN analysis with r:

Install R(manual from https://www.vultr.com/docs/how-to-install-rstudio-server-on-centos-7 ):

sudo yum install R -y

Enter the R shell:

  sudo -i R

Install the package you need as below, and more packages can be installed in the same fashion:

  install.packages('txtplot')
quit the R shell:

  q()

Run analysis script:

  Rscript a.R

or

  R CMD BATCH a.R

Check the output:

  cat a.Rout


# Publications:
This project is based on previous work:

I. A. Zolotarev, L. S. Novikov, V. I. Osedlo, V. I. Tulupov, and N. P. Chirskaya. Numerical simulation of metrological characteristics of cosmic radiation detectors. Inorganic Materials: Applied Research, 8(2):222–228, 2017. [ DOI 10.1134/S2075113317020241]

# Abstract (Cospar thesis):


This work presents the technique and results of computer simulation of the interaction of cosmic radiation with detectors. The program code based on Geant4 libraries is designed, to simulate the operation of a telescopic detector of ionizing radiation, consisting of several sequentially located semiconductor and scintillation detectors. Using the Geant4 toolkit, we simulate signals from electrons and protons of various energies in the detecting regions of the device, taking into account the generation of secondary particles. Separation of signals from primary particles passing through the entrance window of the telescope and through the walls of the housing is carried out. The obtained data are used to simulate the necessary throughput of the device electronics in the Matlab Simulink environment. The characteristics of the sensitivity and accuracy of the spectrum determination are performed using an original algorithm. This algorithm realizes quantile regression to obtain information on the spectra of cosmic radiation, based on the energy losses of particles in the detectors of the instrument.



The combination of discussed software packages allows to optimize the parameters of the geometry and logic circuits of the device. The source files of the program codes will be available in the public domain.

 Application of the results obtained using the presented software codes at the design and production stages of spacecraft will significantly increase their reliability and extend the service life under the space radiation exposure.
 
# Acknowledgments:
Current research was funded by RFBR according to the project №18-32-00727-mol-a.
