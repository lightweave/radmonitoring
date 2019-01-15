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
// $Id: DetectorConstruction.cc,v 1.1 2012-09-20 mraine Exp $
// -------------------------------------------------------------------

#include "DetectorConstruction.hh"
#include "G4SystemOfUnits.hh"
#include "G4Region.hh"
#include "G4ProductionCuts.hh"
#include "G4Tubs.hh"
#include "G4Box.hh"
#include "G4Cons.hh"

#include "G4LogicalBorderSurface.hh"
#include "G4LogicalSkinSurface.hh"
#include "G4OpticalSurface.hh"
#include "G4PhysicalConstants.hh"
#include "G4OpBoundaryProcess.hh"
#include "G4UnionSolid.hh"

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo....

DetectorConstruction::DetectorConstruction()
:fPhysiWorld(NULL), fLogicWorld(NULL), fSolidWorld(NULL)
{}  

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo....

DetectorConstruction::~DetectorConstruction()
{}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo....

G4VPhysicalVolume* DetectorConstruction::Construct()

{
  DefineMaterials();
  return ConstructDetector();
}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo....

void DetectorConstruction::DefineMaterials()
{ 
  G4double a, z, density;
  G4int nelements;
  G4int number_of_atoms;
  G4double fractionmass;
  G4int ncomponents;
  G4bool isotopes = false;
  // Silicon is defined from NIST material database
  G4NistManager * man = G4NistManager::Instance();
 // G4Material * Si = man->FindOrBuildMaterial("G4_Si");

    // Default materials in setup.
  fSiMaterial = man->FindOrBuildMaterial("G4_Si");

  fAlMaterial = man->FindOrBuildMaterial("G4_Al");
  // vacuum

  G4Element* N = new G4Element("Nitrogen", "N", z=7 , a=14.01*g/mole);
  G4Element* O = new G4Element("Oxygen"  , "O", z=8 , a=16.00*g/mole);
    G4Element* C = new G4Element("Carbon", "C", z=6., a=12.01*g/mole);
    G4Element* Cu  = new G4Element("Copper"  , "Cu" , z= 29., a= 63.54*g/mole);
    G4Element* Mg  = new G4Element("Magnesium"  ,"Mg" , z= 12., a= 24.305*g/mole);
    G4Element* Mn  = new G4Element("Manganese"  ,"Mn" , z= 25., a= 54.94*g/mole);
    G4Element* Al  = new G4Element("Aluminium"  ,"Al" , z= 13., a= 26.98*g/mole);
    G4Element* H = new G4Element("Hydrogen", "H", z=1 , a=1.01*g/mole);
    G4Element* Si = new G4Element("Silicium", "Si", z=14 , a=28.086*g/mole);

   /*  man->FindOrBuildMaterial("G4_Si", isotopes);
     man->FindOrBuildMaterial("G4_Fe", isotopes);
     man->FindOrBuildMaterial("G4_Cu", isotopes);
     man->FindOrBuildMaterial("G4_Ge", isotopes);
     man->FindOrBuildMaterial("G4_Mo", isotopes);
     man->FindOrBuildMaterial("G4_Ta", isotopes);
     man->FindOrBuildMaterial("G4_W" , isotopes);
     man->FindOrBuildMaterial("G4_Au", isotopes);
     man->FindOrBuildMaterial("G4_Pb", isotopes);
     man->FindOrBuildMaterial("G4_PLEXIGLASS", isotopes);
     man->FindOrBuildMaterial("G4_SODIUM_IODIDE", isotopes);
     man->FindOrBuildMaterial("G4_C", isotopes);
     man->FindOrBuildMaterial("G4_CESIUM_IODIDE", isotopes);
*/
          air = new G4Material("Air", density=1.29*mg/cm3, nelements=2);
          air->AddElement(N, 70.*perCent);
          air->AddElement(O, 30.*perCent);

          // vacuum
          beam =
          new G4Material("Vacuum", density= 1.e-8*g/cm3, nelements=1,
                                 kStateGas, STP_Temperature, 2.e-5*bar);
          beam->AddMaterial(air, fractionmass=1.);

    G4Material* Dural =
    new G4Material("Dural"  , density= 2.700*g/cm3, ncomponents=4);
    Dural->AddElement(Al, fractionmass=0.936);
    Dural->AddElement(Cu, fractionmass=0.044);
    Dural->AddElement(Mg, fractionmass=0.015);
    Dural->AddElement(Mn, fractionmass=0.005);
// Used in geometry materials
    gapMaterial = beam;
    DuralMaterial = Dural;
//  	DuralMaterial = man->FindOrBuildMaterial("G4_BRASS", isotopes);
    Shell_material = man->FindOrBuildMaterial("G4_W", isotopes);
    Cone_dielectric = man->FindOrBuildMaterial("G4_PLEXIGLASS", isotopes);
    ScintMaterial = man->FindOrBuildMaterial("G4_CESIUM_IODIDE", isotopes);
   // Chose main material of detector's walls
    Wall_material = Dural;

    G4cout << *(G4Material::GetMaterialTable()) << G4endl;
}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo....
G4VPhysicalVolume* DetectorConstruction::ConstructDetector()
{

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo....

  // WORLD VOLUME
  
  fWorldSizeX  = 25.*cm;
  fWorldSizeY  = 25.*cm;
  fWorldSizeZ  = 25.*cm;

  //World volume size
//    MainZ = 95.*mm;
  MainZ = 0.*mm;
  MainR = 63.*mm;  //!!
  // First detector - Si
    Det1Z = 0.04*mm;
    Det1R = 4.*mm;
    //
    FoilGapZ = 4*mm;
  // vacuum between foil and detector D1
    Gap1Z = 17.68*mm;
    Gap1R = 4.0*mm;

  // vacuum between detectors D1 and D2
    Gap2Z = 5.0*mm;
    Gap2R = 4.*mm;
      // vacuum between detectors D2 and D3
    Gap3Z = 5.0*mm;
    Gap3R = 7.5*mm;
  // vacuum between detectors D3 and D4 (lower)
    Gap4Z = 5.0*mm;
    Gap4R = 7.5*mm;

  // distance between detectors D3 and D4 (plex)
   // Gap5Z = 6.*mm;
  //		Gap5R = 7.*mm;

  // Outer foil, equivalent to 20 um of Si
    FoilZ = 20.*um;
    FoilR = 4.0*mm;
  // фотодиод за 3-м детектором
    FotoZ = 300.*um;
    FotoR = 7.5*mm;

  // Second detector - Si
    Det2Z = 0.5*mm;
    Det2R = 4.*mm;

  // Third detector - scint CsJ
      Det3Z = 10.*mm;
      Det3R = 7.5*mm;

  // Fourth detector - Si
     Det4Z = 1.*mm;
     Det4R = 10.*mm;

  // default parameter values of the absorbers
    pDet1   = 0;
    pDet2   = 0;
    pDet3   = 0;
    pDet4   = 0;

    // parameters of upper cone// обновила

    ConeRmin1 = 12.8*mm ;
    ConeRmax1 = 22.*mm;
    ConeRmin2 = 4.0*mm;
    ConeRmax2 = 22.*mm;
    ConeDz = 28.0*mm;

  // first part of shell- metal
    UpperTube1Z = 74.7*mm;
    UpperTube1Rmin = 0.*mm;
    UpperTube1Rmax = 22.*mm;
  // third (inner ) part of shell - plex
    UpperTube2Z = 21.7*mm;
    UpperTube2Rmin = 0.*mm;
    UpperTube2Rmax =11.5*mm;
  // first (outer) part of lower shell - metal
    LowTube1Z = 68.*mm;
    LowTube1Rmin = 22.*mm;
    LowTube1Rmax = 27.5*mm;
  // second part of lower tube - plex
    LowTube2Z = 38.*mm;
    LowTube2Rmin = 0.*mm;
    LowTube2Rmax = 10.5*mm;
  // third (outer) part of lower shell - metal
    LowTube3Z = 68.*mm;
    LowTube3Rmin = 27.5*mm;
    LowTube3Rmax = 31.5*mm;
  // Вольфрамовая защита вокруг D3 и D4, частично замещает плекс
    W_shield1Z = 38.*mm;
    W_shield1Rmin = 10.5*mm;
    W_shield1Rmax = 18.5*mm;
  //Вольфрамовая шайба в нижней части
    W_shield2Z = 5.*mm;
    W_shield2R = 18.5*mm;
  //Вольфрамовая шайба в верхней части. Разбита на 2 шайбы. Внешняя - дочерняя от UpperTube1, внутренняя - от UpperTube2
    W_shield3Z = 5.*mm;
    W_shield3Rmin = 4.0*mm;
    W_shield3Rmax = 18.5*mm;
    W_shield3Rmid = UpperTube2Rmax;

  ////////////////////////////////
  fSolidWorld = new G4Box("World",				     //its name
			   fWorldSizeX/2,fWorldSizeY/2,fWorldSizeZ/2);  //its size
  

  fLogicWorld = new G4LogicalVolume(fSolidWorld,	//its solid
                   beam,		//its material
				   "World");		//its name
  
  fPhysiWorld = new G4PVPlacement(0,			//no rotation
  				 G4ThreeVector(),	//at (0,0,0)
                                 "World",		//its name
                                 fLogicWorld,		//its logical volume
                                 0,			//its mother  volume
                                 false,			//no boolean operation
                                 0);			//copy number
  // Shield volume
  G4double ShieldR =  19.*cm;

  G4Sphere* ShieldSolid = new G4Sphere("Shield",				     //its name
                ShieldR-1.*mm,ShieldR,0.0*deg, 360.0*deg,0.0*deg, 180.0*deg);   //its size


  ShieldLogic = new G4LogicalVolume(ShieldSolid,       //its solid
                             fSiMaterial,	//its material
                             "Shield");		//its name
/*
  ShieldPhysi = new G4PVPlacement(0,			                               //no rotation
            G4ThreeVector(0.,0.,0.),	                               //at (0,0,0)
            ShieldLogic,	//its logical volume
            "Shield",		//its name
            fLogicWorld,		//its mother  volume
            false,		//no boolean operation
            0);			//copy number
*/
//

   /// Upper cone part - коллиматор
    G4VSolid*
     sCone = new G4Cons("Al_Cone",
                   ConeRmin1,
                   ConeRmax1,
                   ConeRmin2,
                   ConeRmax2,
                   ConeDz/2.,//half lenght
                   0.0*deg,
                   360.0*deg);

    G4LogicalVolume*
      lCone = new G4LogicalVolume(sCone,			//solid
              Wall_material,		//material
                            "Al_Cone");		//name
    G4VPhysicalVolume*
      pCone = new G4PVPlacement(0,				//no rotation
                                 G4ThreeVector(0.,0.,MainZ/2.0+ConeDz/2.),		//position
                                 lCone,			//logical volume
                                "Al_Cone",			//name
                                 fLogicWorld,				//mother  volume
                                 false,			//no boolean operation
                                 0);			//copy number
  /// вакуумный детектор в растворе коллиматора
      G4VSolid*
       sVacDet = new G4Cons("VacDet",
                     0.,
                     ConeRmin1,
                     0.,
                     ConeRmin2,
                     ConeDz/2.,//half lenght
                     0.0*deg,
                     360.0*deg);

      G4LogicalVolume*
        lVacDet = new G4LogicalVolume(sVacDet,			//solid
                gapMaterial,		//material
                            "VacDet");		//name

      pVacDet = new G4PVPlacement(0,				//no rotation
                                   G4ThreeVector(0.,0.,MainZ/2.0+ConeDz/2.),		//position
                                   lVacDet,			//logical volume
                                  "VacDet",			//name
                                   fLogicWorld,				//mother  volume
                                   false,			//no boolean operation
                                   0);			//copy number


  /// внутренняя часть металлического корпуса, остальное (кроме внешнего металла)
  //    помещается в него дочерними объемами
       G4VSolid*
        sUpperTube1 = new G4Tubs("UpperTube1",
                UpperTube1Rmin,				// min radius
                UpperTube1Rmax,		// max radius
                UpperTube1Z/2.,		// Height
                0.0*deg,			// start angle
                360.0*deg);		// segment size

       G4LogicalVolume*
         lUpperTube1 = new G4LogicalVolume(sUpperTube1,			//solid
                 Wall_material,		//material
                            "UpperTube1");		//name

       G4VPhysicalVolume*
       pUpperTube1 = new G4PVPlacement(0,				//no rotation
                                    G4ThreeVector(0.,0.,MainZ/2.0
                                                  +ConeDz+UpperTube1Z/2.),		//position
                                    lUpperTube1,			//logical volume
                                   "UpperTube1",			//name
                                    fLogicWorld,				//mother  volume
                                    false,			//no boolean operation
                                    0);			//copy number

  /// Первая часть металлического корпуса - толстая часть, там где диаметр 57 мм


   G4VSolid*
    sLowTube1 = new G4Tubs("LowTube1",
            LowTube1Rmin,				// min radius
            LowTube1Rmax,		// max radius
            LowTube1Z/2.,		// Height
            0.0*deg,			// start angle
            360.0*deg);		// segment size

   G4LogicalVolume*
     lLowTube1 = new G4LogicalVolume(sLowTube1,			//solid
             Wall_material,		//material
                        "LowTube1");		//name

   G4VPhysicalVolume*
   pLowTube1 = new G4PVPlacement(0,				//no rotation
                                G4ThreeVector(0.,0.,MainZ/2.0+ConeDz+UpperTube1Z-LowTube1Z/2.),		//position
                                lLowTube1,			//logical volume
                               "LowTube1",			//name
                                fLogicWorld,				//mother  volume
                                false,			//no boolean operation
                                0);			//copy number
   /// Самая внешняя часть металлического корпуса - толстая часть, там где диаметр 57 мм
   // Вольфрам, 3 мм - Варинат C
  // В варианте с внутренней вольфрамовой вставкой  (Вариант D) меняем на дюраль

 /*   G4VSolid*
     sLowTube3 = new G4Tubs("LowTube3",
            LowTube3Rmin,				// min radius
            LowTube3Rmax,		// max radius
            LowTube3Z/2.,		// Height
            0.0*deg,			// start angle
            360.0*deg);		// segment size

    G4LogicalVolume*
      lLowTube3 = new G4LogicalVolume(sLowTube3,			//solid
  // 		   Shell_material,		//material - вольфрамовый слой снаружи - Вариант C
            DuralMaterial,        //вариант с внутренней вольфрамовой вставкой (D)
                        "LowTube3");		//name

    G4VPhysicalVolume*
    pLowTube3 = new G4PVPlacement(0,				//no rotation
                                G4ThreeVector(0.,0.,MainZ/2.0+ConeDz+10.*mm+LowTube3Z/2.),		//position
                                lLowTube3,			//logical volume
                               "LowTube3",			//name
                                fLogicWorld,				//mother  volume
                                false,			//no boolean operation
                                0);			//copy number
*/
  /// внутренний корпус - оргстекло, дочерний от uppertube1
        G4VSolid*
         sUpperTube2 = new G4Tubs("UpperTube2",
                UpperTube2Rmin,				// min radius
                 UpperTube2Rmax,		// max radius
                 UpperTube2Z/2.,		// Height
                 0.0*deg,			// start angle
                 360.0*deg);		// segment size

        G4LogicalVolume*
          lUpperTube2 = new G4LogicalVolume(sUpperTube2,			//solid
                  Cone_dielectric,		//material
                                "UpperTube2");		//name

        G4VPhysicalVolume*
        pUpperTube2 = new G4PVPlacement(0,				//no rotation
                                     G4ThreeVector(0.,0.,-UpperTube1Z/2.0+UpperTube2Z/2.),		//position
                                     lUpperTube2,			//logical volume
                                    "UpperTube2",			//name
                                     lUpperTube1,				//mother  volume
                                     false,			//no boolean operation
                                     0);			//copy number
// Вакуум перед фольгой

         G4VSolid* sFoilGap = new G4Tubs("FoilGap",				//name
                 0.,				// min radius
                 Gap1R,		// max radius
                 FoilGapZ/2.,		// Height
                   0.0*deg,			// start angle
                   360.0*deg);		// segment size

         G4LogicalVolume*
         lFoilGap = new G4LogicalVolume(sFoilGap,			//solid
                 gapMaterial,		//material
                              "FoilGap");		//name
         G4VPhysicalVolume*
         pFoilGap = new G4PVPlacement(0,				//no rotation
                                    G4ThreeVector(0.,0.,-UpperTube2Z/2.+FoilGapZ/2.),		//position
                                    lFoilGap,			//logical volume
                                   "FoilGap",			//name
                                    lUpperTube2,				//mother  volume
                                    false,			//no boolean operation
                                    0);			//copy number

  //фольга перед 1-м детектором - в растворе коллиматора

    G4VSolid* sFoil = new G4Tubs("Foil",				//name
              0.0,				// min radius
              FoilR,		// max radius
              FoilZ/2.,		// Height
              0.0*deg,			// start angle
              360.0*deg);		// segment size

    G4LogicalVolume*
    lFoil = new G4LogicalVolume(sFoil,			//solid
            fSiMaterial,		//material
                        "Foil");		//name

    G4VPhysicalVolume*
    pFoil = new G4PVPlacement(0,				//no rotation
                               G4ThreeVector(0.,0.,-UpperTube2Z/2.+FoilGapZ+FoilZ/2.), //position
                               lFoil,			//logical volume
                              "Foil",			//name
                               lUpperTube2,				//mother  volume
                               false,			//no boolean operation
                               0);			//copy number
  // Вакуум между фольгой и Д1

           G4VSolid* sGap1 = new G4Tubs("Gap1",				//name
                   0.,				// min radius
                   Gap1R,		// max radius
                   Gap1Z/2.,		// Height
                     0.0*deg,			// start angle
                     360.0*deg);		// segment size

           G4LogicalVolume*
           lGap1 = new G4LogicalVolume(sGap1,			//solid
                   gapMaterial,		//material
                                "Gap1");		//name
           G4VPhysicalVolume*
           pGap1 = new G4PVPlacement(0,				//no rotation
                                      G4ThreeVector(0.,0.,-UpperTube2Z/2.+FoilGapZ+FoilZ+Gap1Z/2.),		//position
                                      lGap1,			//logical volume
                                     "Gap1",			//name
                                      lUpperTube2,				//mother  volume
                                      false,			//no boolean operation
                                      0);			//copy number


  /// внутренний корпус 2 - оргстекло, нижняя часть, дочерний от uppertube1
  G4VSolid*
   sLowTube2 = new G4Tubs("LowTube2",
           LowTube2Rmin,				// min radius
           LowTube2Rmax,		// max radius
           LowTube2Z/2.,		// Height
           0.0*deg,			// start angle
           360.0*deg);		// segment size

  G4LogicalVolume*
    lLowTube2 = new G4LogicalVolume(sLowTube2,			//solid
            Cone_dielectric,		//material
                        "LowTube2");		//name

  G4VPhysicalVolume*
  pLowTube2 = new G4PVPlacement(0,				//no rotation
                               G4ThreeVector(0.,0.,-UpperTube1Z/2.0+UpperTube2Z+LowTube2Z/2.),		//position
                               lLowTube2,			//logical volume
                              "LowTube2",			//name
                               lUpperTube1,				//mother  volume
                               false,			//no boolean operation
                               0);			//copy number

  // First detector - Si - D1,D2, Gaps, D3, D4 - daughters of LowTube 2
  G4Material* material = fSiMaterial;


         G4VSolid* sDet1 = new G4Tubs("Det1",				//name
                 0.,				// min radius
                 Det1R,		// max radius
                 Det1Z/2.,		// Height
                   0.0*deg,			// start angle
                   360.0*deg);		// segment size

         G4LogicalVolume*
         lDet1 = new G4LogicalVolume(sDet1,			//solid
                 material,		//material
                              "Det1");		//name

         pDet1 = new G4PVPlacement(0,				//no rotation
                                    G4ThreeVector(0.,0.,-LowTube2Z/2.+Det1Z/2.),		//position
                                    lDet1,			//logical volume
                                   "Det1",			//name
                                    lLowTube2,				//mother  volume
                                    false,			//no boolean operation
                                    0);			//copy number

// Вакуум между Д1 и Д2

          G4VSolid* sGap2 = new G4Tubs("Gap2",				//name
                 0.,				// min radius
                 Gap2R,		// max radius
                 Gap2Z/2.,		// Height
                    0.0*deg,			// start angle
                    360.0*deg);		// segment size

          G4LogicalVolume*
          lGap2 = new G4LogicalVolume(sGap2,			//solid
                 gapMaterial,		//material
                              "Gap2");		//name
          G4VPhysicalVolume*
          pGap2 = new G4PVPlacement(0,				//no rotation
                                     G4ThreeVector(0.,0.,-LowTube2Z/2.+Det1Z+Gap2Z/2.),		//position
                                     lGap2,			//logical volume
                                    "Gap2",			//name
                                     lLowTube2,				//mother  volume
                                     false,			//no boolean operation
                                     0);			//copy number


// Second detector - Si
       material = fSiMaterial;
       G4VSolid* sDet2 = new G4Tubs("Det2",				//name
             0.,				// min radius
             Det2R,		// max radius
             Det2Z/2.,		// Height
                 0.0*deg,			// start angle
                 360.0*deg);		// segment size

       G4LogicalVolume*
       lDet2 = new G4LogicalVolume(sDet2,			//solid
               material,		//material
                          "Det2");		//name

       pDet2 = new G4PVPlacement(0,				//no rotation
                                  G4ThreeVector(0.,0.,-LowTube2Z/2.+Det1Z+Gap2Z+Det2Z/2.),		//position
                                  lDet2,			//logical volume
                                 "Det2",			//name
                                  lLowTube2,				//mother  volume
                                  false,			//no boolean operation
                                  0);			//copy number

       // Вакуум между Д2 и Д3
       //// Верхний - дочерний от LowTube2

              G4VSolid* sGap3 = new G4Tubs("Gap3",				//name
                    0.,				// min radius
                    Gap3R,		// max radius
                    Gap3Z/2.,		// Height
                        0.0*deg,			// start angle
                        360.0*deg);		// segment size

              G4LogicalVolume*
              lGap3 = new G4LogicalVolume(sGap3,			//solid
                    gapMaterial,		//material
                                 "Gap3");		//name
              G4VPhysicalVolume*
              pGap3 = new G4PVPlacement(0,				//no rotation
                                         G4ThreeVector(0.,0.,-LowTube2Z/2.+Det1Z+Gap2Z+Det2Z+Gap3Z/2.),		//position
                                         lGap3,			//logical volume
                                        "Gap3",			//name
                                         lLowTube2,				//mother  volume
                                         false,			//no boolean operation
                                         0);			//copy number
  ///
  /// внутренний корпус из вольфрама, частично замещает оргстекло, дочерний от lUpperTube1

  G4VSolid*
   sW_shield1 = new G4Tubs("W_shield1",
           W_shield1Rmin,				// min radius
           W_shield1Rmax,		// max radius
           W_shield1Z/2.,		// Height
           0.0*deg,			// start angle
           360.0*deg);		// segment size

  G4LogicalVolume*
    lW_shield1 = new G4LogicalVolume(sW_shield1,			//solid
            Shell_material,		//material
                        "W_shield1");		//name

  G4VPhysicalVolume*
  pW_shield1 = new G4PVPlacement(0,				//no rotation
                               G4ThreeVector(0.,0.,-UpperTube1Z/2.0+UpperTube2Z+W_shield1Z/2.),		//position
                               lW_shield1,			//logical volume
                              "W_shield1",			//name
                              lUpperTube1,				//mother  volume
                               false,			//no boolean operation
                               0);			//copy number

  ////////
  /// Нижняя шайба из вольфрама, дочерний от uppertube1 - Вариант D
  G4VSolid*
   sW_shield2 = new G4Tubs("W_shield2",
           0.,				// min radius
           W_shield2R,		// max radius
           W_shield2Z/2.,		// Height
           0.0*deg,			// start angle
           360.0*deg);		// segment size

  G4LogicalVolume*
    lW_shield2 = new G4LogicalVolume(sW_shield2,			//solid
            Shell_material,		//material
                        "W_shield2");		//name

  G4VPhysicalVolume*
  pW_shield2 = new G4PVPlacement(0,				//no rotation
                               G4ThreeVector(0.,0.,-UpperTube1Z/2.0+UpperTube2Z+LowTube2Z+W_shield2Z/2.),		//position
                               lW_shield2,			//logical volume
                              "W_shield2",			//name
                               lUpperTube1,				//mother  volume
                               false,			//no boolean operation
                               0);			//copy number
  //////////////верхняя шайба - вариант 2
  /// Верхняя внутренняя шайба из вольфрама, дочерний от uppertube2
  G4VSolid*
   sW_shield3 = new G4Tubs("W_shield3",
           W_shield3Rmin,				// min radius
           W_shield3Rmid,		// max radius
           W_shield3Z/2.,		// Height
           0.0*deg,			// start angle
           360.0*deg);		// segment size

  G4LogicalVolume*
    lW_shield3 = new G4LogicalVolume(sW_shield3,			//solid
            Shell_material,		//material
                        "W_shield3");		//name

  G4VPhysicalVolume*
  pW_shield3 = new G4PVPlacement(0,				//no rotation
                               G4ThreeVector(0.,0.,UpperTube2Z/2.0-W_shield3Z/2.),		//position
                               lW_shield3,			//logical volume
                              "W_shield3",			//name
                               lUpperTube2,				//mother  volume
                               false,			//no boolean operation
                               0);			//copy number
  /// Верхняя вннешняя шайба из вольфрама, дочерний от uppertube1
  G4VSolid*
   sW_shield4 = new G4Tubs("W_shield4",
           W_shield3Rmid,				// min radius
           W_shield3Rmax,		// max radius
           W_shield3Z/2.,		// Height
           0.0*deg,			// start angle
           360.0*deg);		// segment size

  G4LogicalVolume*
    lW_shield4 = new G4LogicalVolume(sW_shield4,			//solid
            Shell_material,		//material
                        "W_shield4");		//name

  G4VPhysicalVolume*
  pW_shield4 = new G4PVPlacement(0,				//no rotation
                               G4ThreeVector(0.,0.,-UpperTube1Z/2.0+UpperTube2Z-W_shield3Z/2.),		//position
                               lW_shield4,			//logical volume
                              "W_shield4",			//name
                               lUpperTube1,				//mother  volume
                               false,			//no boolean operation
                               0);			//copy number


  // Third detector - CsJ, дочений от LowTube2
              material = ScintMaterial;

              G4VSolid* sDet3 = new G4Tubs("Det3",				//name
                           0.,				// min radius
                           Det3R,		// max radius
                           Det3Z/2.,		// Height
                               0.0*deg,			// start angle
                               360.0*deg);		// segment size

                     G4LogicalVolume*
                     lDet3 = new G4LogicalVolume(sDet3,			//solid
                             material,		//material
                                        "Det3");		//name

                     pDet3 = new G4PVPlacement(0,				//no rotation
                                                G4ThreeVector(0.,0.,-LowTube2Z/2.+Det1Z+Gap2Z+Det2Z+Gap3Z+Det3Z/2.),		//position
                                                lDet3,			//logical volume
                                               "Det3",			//name
                                                lLowTube2,				//mother  volume
                                                false,			//no boolean operation
                                                0);


   // фотодиод за 3-м детектором - Si

                G4VSolid* sFoto = new G4Tubs("Foto",				//name
                          0.0,				// min radius
                          FotoR,		// max radius
                          FotoZ/2.,		// Height
                          0.0*deg,			// start angle
                          360.0*deg);		// segment size

                G4LogicalVolume*
                lFoto = new G4LogicalVolume(sFoto,			//solid
                        fSiMaterial,		//material
                                    "Foto");		//name

                G4VPhysicalVolume*
                pFoto = new G4PVPlacement(0,				//no rotation
                                           G4ThreeVector(0.,0.,-LowTube2Z/2.+Det1Z+Gap2Z+Det2Z+Gap3Z+Det3Z+FotoZ/2.),		//position
                                           lFoto,			//logical volume
                                          "Foto",			//name
                                           lLowTube2,				//mother  volume
                                           false,			//no boolean operation
                                           0);			//copy number
  // Fourth detector - Si
              material = fSiMaterial;
              G4VSolid* sDet4 = new G4Tubs("Det4",				//name
                        0.,				// min radius
                        Det4R,		// max radius
                        Det4Z/2.,		// Height
                        0.0*deg,			// start angle
                        360.0*deg);		// segment size

              G4LogicalVolume*
                        lDet4 = new G4LogicalVolume(sDet4,			//solid
                        material,		//material
                        "Det4");		//name

              pDet4 = new G4PVPlacement(0,				//no rotation
                        G4ThreeVector(0.,0.,-LowTube2Z/2.+Det1Z+Gap2Z+Det2Z+Gap3Z+Det3Z+FotoZ+Gap4Z+Det4Z/2.),		//position
                        lDet4,			//logical volume
                        "Det4",			//name
                        lLowTube2,				//mother  volume
                        false,			//no boolean operation
                        0);			//copy number


  // Visualization attributes
  G4VisAttributes* worldVisAtt= new G4VisAttributes(G4Colour(0.5,0.5,0.9)); //RGB
  worldVisAtt->SetVisibility(true);
  fLogicWorld->SetVisAttributes(worldVisAtt);

    //MirrorLogic->SetVisAttributes(G4VisAttributes::Invisible);
    ShieldLogic->SetVisAttributes(G4VisAttributes::Invisible);
    // Detector1 - yellow
              Det1VisAtt = new G4VisAttributes(true,                 // visibility: true
                                       G4Colour::Yellow());
              Det1VisAtt->SetForceSolid(true);
        //      lDet1 -> SetVisAttributes(Det1VisAtt);
   // Foil -blue, solid
              FoilVisAtt = new G4VisAttributes(true,                 // visibility: true
                                                   G4Colour::Blue());
              FoilVisAtt->SetForceAuxEdgeVisible(true);
              FoilVisAtt->SetForceSolid(true);
             lFoil -> SetVisAttributes(FoilVisAtt);
             lFoto -> SetVisAttributes(FoilVisAtt);

             WVisAtt = new G4VisAttributes(true,                 // visibility: true
                                                  G4Colour::Green());
             lW_shield1 -> SetVisAttributes(WVisAtt);
             lW_shield2 -> SetVisAttributes(WVisAtt);
             lW_shield3 -> SetVisAttributes(WVisAtt);
             lW_shield4 -> SetVisAttributes(WVisAtt);


   // Detector2,3,4
              Det2VisAtt = new G4VisAttributes(true,                 // visibility: true
                       G4Colour::Yellow());
              Det2VisAtt->SetForceSolid(true);
                        lDet2 -> SetVisAttributes(Det2VisAtt);
                        lDet3 -> SetVisAttributes(Det2VisAtt);
                        lDet4 -> SetVisAttributes(Det2VisAtt);
                        lDet1 -> SetVisAttributes(Det2VisAtt);
    // Tube1
          Tube1VisAtt = new G4VisAttributes(true,   // visibility: true
                     G4Colour::White());
    //    Tube1VisAtt->SetForceSolid(true);
          Tube1VisAtt->SetForceAuxEdgeVisible(true);
              lUpperTube1 -> SetVisAttributes(Tube1VisAtt);
             lLowTube1 -> SetVisAttributes(Tube1VisAtt);
//              lLowTube3 -> SetVisAttributes(Tube1VisAtt);
              // Tube2
                    Tube2VisAtt = new G4VisAttributes(true,   // visibility: true
                               G4Colour::Green());
                    Tube2VisAtt->SetForceAuxEdgeVisible(true);
    //              Tube2VisAtt->SetForceSolid(true);
                        lUpperTube2 -> SetVisAttributes(Tube2VisAtt);
                       lLowTube2 -> SetVisAttributes(Tube2VisAtt);



   // Cone
     //  ConeVisAtt = new G4VisAttributes(true,   // visibility: true
     //               G4Colour::Green());
     //  ConeVisAtt->SetForceAuxEdgeVisible(true);
 //                  lCone -> SetVisAttributes(Tube1VisAtt);

  // Create Target G4Region and add logical volume
  
  fRegion = new G4Region("Detector");
  
  G4ProductionCuts* cuts = new G4ProductionCuts();
  
  G4double defCut = 10.*um;
  cuts->SetProductionCut(defCut,"gamma");
  cuts->SetProductionCut(defCut,"e-");
  cuts->SetProductionCut(defCut,"e+");
  cuts->SetProductionCut(defCut,"proton");
  
  fRegion->SetProductionCuts(cuts);
  fRegion->AddRootLogicalVolume(lDet1);
//  fRegion->AddRootLogicalVolume(LenseLogic);

  return fPhysiWorld;
}
