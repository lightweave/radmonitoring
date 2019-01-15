//
// ********************************************************************
// * License and Disclaimer                                           *
// *                                                                  *
// * The  Geant4 software  is  copyright of the Copyright Holders  of *
// * the Geant4 Collaboration.  It is provided  under  the terms  and *
// * conditions of the Geant4 Software License,  included in the file *
// * LICENSE and available at  http://cern.ch/geant4/license .  These *
// * include a list of copyright holders.                             *
// *                                                                  *
// * Neither the authors of this software system, nor their employing *
// * institutes,nor the agencies providing financial support for this *
// * work  make  any representation or  warranty, express or implied, *
// * regarding  this  software system or assume any liability for its *
// * use.  Please see the license in the file  LICENSE  and URL above *
// * for the full disclaimer and the limitation of liability.         *
// *                                                                  *
// * This  code  implementation is the result of  the  scientific and *
// * technical work of the GEANT4 collaboration.                      *
// * By using,  copying,  modifying or  distributing the software (or *
// * any work based  on the software)  you  agree  to acknowledge its *
// * use  in  resulting  scientific  publications,  and indicate your *
// * acceptance of all terms of the Geant4 Software license.          *
// ********************************************************************
//
// -------------------------------------------------------------------
// $Id: SteppingAction.cc,v 1.1 2012-09-21 mraine Exp $
// -------------------------------------------------------------------

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo....

#include "Analysis.hh"

#include "SteppingAction.hh"
#include "RunAction.hh"
#include "EventAction.hh"
#include "DetectorConstruction.hh"
#include "PrimaryGeneratorAction.hh"

#include "G4SystemOfUnits.hh"
#include "G4SteppingManager.hh"
#include "G4VTouchable.hh"
#include "G4VPhysicalVolume.hh"
#include "stddef.h"


//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo....

SteppingAction::SteppingAction(RunAction* RuAct,EventAction* event)
: G4UserSteppingAction(), runAction(RuAct), eventAction(event)
{
}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo....

SteppingAction::~SteppingAction()
{}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo....

void SteppingAction::UserSteppingAction(const G4Step* step)
{ 
     G4double flagParticle=0.;
     G4double energydep = 0.;
     G4double kinenergy = 0.;
     G4double PartType = 0.;
     G4double momentumx = 0.;
     G4double momentumy = 0.;
     G4double momentumz= 0.;
     G4double momentum2 = 0.;
     G4double angz = 0.;
     G4int colimflag = 0;
     G4double edep = 0.;
     G4int detNo = -1.;
     G4double pi  = 3.14159265358979323846;

// In which volume point is located

  G4VPhysicalVolume* volume = step->GetPreStepPoint()->GetPhysicalVolume();
  G4String name = volume->GetName();
// Kill outgoing particles
 //  if (name == "Shield") step->GetTrack()->SetTrackStatus(fStopAndKill);

  const G4Track* track = step->GetTrack();
  if ( track->GetParentID() == 0 )
      {
      PartType = 1.; //Primary
      // Check if the particle just borned and is not a secondary
      if ( track->GetCurrentStepNumber() == 1)
         {

      energydep = step->GetTotalEnergyDeposit();
      kinenergy = step->GetTrack()->GetKineticEnergy();
      kinenergy = energydep+kinenergy;
      eventAction->AddInitSpectrum(kinenergy); //primary energy in current event
      //G4cout << "energy =  " << "  " << kinenergy << G4endl;

// Start angle of primary particle
      G4ThreeVector momentum = track->GetMomentumDirection();
      momentumx = momentum.x();
      momentumy = momentum.y();
      momentumz = momentum.z();
      momentum2 = sqrt(pow(momentumx,2)+pow(momentumy,2)+pow(momentumz,2));
      angz = acos(momentumz/momentum2)*180./pi;
      eventAction->SetStartAngle(angz);
//      G4cout << "angle =  " << "  " << angz << G4endl;
         }
  // Partice came from collimator
     if (name == "VacDet_PV" && (eventAction->GetStartAngle()) >= 0. // particle came from forward side
             && (eventAction->GetStartAngle()) <= 90. )
         {
     colimflag = 1;
     eventAction->SetCollimator(colimflag);
         }

      }
   // If the secondary - find the kind of particle and its energy loss in detector
  else
     {
        if (step->GetTrack()->GetDynamicParticle()->GetDefinition() ->GetParticleName() == "e-")      	  flagParticle = 1;
        if (step->GetTrack()->GetDynamicParticle()->GetDefinition() ->GetParticleName() == "proton")  	  flagParticle = 2;
        if (step->GetTrack()->GetDynamicParticle()->GetDefinition() ->GetParticleName() == "GenericIon")    flagParticle = 3;
        if (step->GetTrack()->GetDynamicParticle()->GetDefinition() ->GetParticleName() == "gamma")    flagParticle = 4;
        if (step->GetTrack()->GetDynamicParticle()->GetDefinition() ->GetParticleName() == "neutron")    flagParticle = 5;
    }
        edep =0.;
        edep = step->GetTotalEnergyDeposit();

 // Detector number with signal
    if (name == "Det1_PV" && edep > 0.) detNo = 1;
    if (name == "Det2_PV" && edep > 0.) detNo = 2;
    if (name == "Det3_PV" && edep > 0.) detNo = 3;
    if (name == "Det4_PV" && edep > 0.) detNo = 4;

    if (detNo>0)
    {
        eventAction->AddDetEdep(detNo-1,edep);
        eventAction->AddTotalEdep(edep);
        if (PartType == 1.) // primary particle
            {
            eventAction->AddDetEPrim(detNo-1,edep);
            }
        if (flagParticle == 4.) // gamma enery loss
            {
            eventAction->AddDetEGamm(detNo-1,edep);
            }
        if (flagParticle != 1. && PartType != 1. && flagParticle != 2.) // not electron, proton or primary particle
             {
            eventAction->AddDetESec(detNo-1,edep);
             }
   }

}    
