# radmonitoring
This project started for Monte-Carlo simulation of telescope for space radiation conditions monitoring.
It based on Geant4 libraries and R packages.

Abstract:
This work presents the technique and results of computer simulation of the interaction of cosmic radiation with detectors. 
The program code based on Geant4 libraries is designed, to simulate the operation of a telescopic detector of ionizing radiation,
consisting of several sequentially located semiconductor and scintillation detectors. Using the Geant4 toolkit, 
we simulate signals from electrons and protons of various energies in the detecting regions of the device, taking into account 
the generation of secondary particles. Separation of signals from primary particles passing through the entrance window of the telescope
and through the walls of the housing is carried out.  The characteristics of the sensitivity and accuracy
of the spectrum determination are performed using an original algorithm. This algorithm realizes quantile regression to obtain
information on the spectra of cosmic radiation, based on the energy losses of particles in the detectors of the instrument.
