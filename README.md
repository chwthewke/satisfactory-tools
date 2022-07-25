## Satisfactory Production Calculator prototype

A production planner for the game [Satisfactory](https://satisfactorygame.com/)

* Built from the game-provided `Docs.json` plus a small amount of manual data entry (resource node counts).
* Works with U4 to U6
* Should hopefully resist future updates fairly well.

### Features

* Select the products you want, select the recipes to use – or let the calculator select the best recipes, get a production plan.
* Production plan presented as several convenient lists
  * Production steps, how many buildings for each recipe, which clock speeds
  * Power consumption
  * Total input of raw resources
  * Possible by-products (the solver favors solutions without by-products, but allows them if unavoidable.)
  * Sources and destinations for each intermediate product
* Make groups of production steps to organize your plan prior to building the factory
  * With all the same lists shown for each group
* Manage your plans in a persistent library

### Requirements

#### Dependencies

* Java 11+ (from e.g. [OpenJDK](https://adoptopenjdk.net/))
* [SBT](https://scala-sbt.org)
* Postgresql 13+

Postgres needs to run on the default port localhost:5432, create a database called `factory_data`, 
owned by role `factory` with password `factory` (alternatively, copy
`satisfactory-tools-persistence/src/main/resources/reference.conf` to 
`satisfactory-tools-web-v2/src/main/resources/application.conf` and edit that to your preference)

#### Data setup

The data (docs and icons) for updates 4 to 6 are in the repository, but the database must be initialized.
This is achieved by running any or all of the `InitDatabaseModelU[x]` programs in `satisfactory-tools-dev`, 
which must be run from the root directory of this repo.

Once the requirements are met, run with `sbt run satisfactory-tools-web-v2`.

Point your browser at http://localhost:7282

### Old text-based interface

#### To run

* Copy `satisfactory-production-calculator/src/main/resources/reference.conf` to `satisfactory-production-calculator/src/main/resources/application.conf`, edit and pray
* Run with SBT `sbt satisfactory-production-calculator/run`

#### Sample output

<details>
<summary>Expand</summary>

```
BLOCKS

Recipe                                              | Machines            | Rate                  | Power    
-------------------------------------------------------------------------------------------------------------
 245.740 | Extract Raw Quartz with Miner Mk.2       |   3 Miner Mk.2      |  81.913 / unit @ 69 % |  19.88 MW
1149.285 | Extract Water with Water Extractor       |  10 Water Extractor | 114.928 / unit @ 96 % | 187.35 MW
 288.000 | Extract Limestone with Miner Mk.2        |   3 Miner Mk.2      |  96.000 / unit @ 80 % |  25.19 MW
 192.000 | Wet Concrete                         ALT |   3 Refinery        |  64.000 / unit @ 80 % |  62.98 MW
 577.069 | Extract Coal with Miner Mk.2             |   5 Miner Mk.2      | 115.414 / unit @ 97 % |  57.15 MW
 309.114 | Extract Iron Ore with Miner Mk.2         |   3 Miner Mk.2      | 103.038 / unit @ 86 % |  28.28 MW
 574.069 | Pure Iron Ingot                      ALT |   9 Refinery        |  63.785 / unit @ 99 % | 265.69 MW
 861.103 | Solid Steel Ingot                    ALT |  15 Foundry         |  57.407 / unit @ 96 % | 224.83 MW
 372.939 | Steel Pipe                               |  19 Constructor     |  19.628 / unit @ 99 % |  74.79 MW
  18.333 | Encased Industrial Pipe              ALT |   5 Assembler       |   3.667 / unit @ 92 % |  65.63 MW
 143.590 | Extract Caterium Ore with Miner Mk.2     |   2 Miner Mk.2      |  71.795 / unit @ 60 % |  10.60 MW
  71.795 | Pure Caterium Ingot                  ALT |   6 Refinery        |  11.966 / unit @ 100% | 180.00 MW
 211.031 | Extract Copper Ore with Miner Mk.2       |   2 Miner Mk.2      | 105.515 / unit @ 88 % |  19.56 MW
 527.577 | Pure Copper Ingot                    ALT |  15 Refinery        |  35.172 / unit @ 94 % | 407.58 MW
 368.796 | Fused Wire                           ALT |   5 Assembler       |  73.759 / unit @ 82 % |  54.60 MW
 309.089 | Extract Crude Oil with Oil Extractor     |   3 Oil Extractor   | 103.030 / unit @ 86 % |  94.27 MW
 128.302 | Plastic                                  |   7 Refinery        |  18.329 / unit @ 92 % | 183.77 MW
 140.370 | Steel Coated Plate                   ALT |   4 Assembler       |  35.093 / unit @ 78 % |  40.32 MW
  33.111 | Stitched Iron Plate                  ALT |   6 Assembler       |   5.519 / unit @ 99 % |  88.56 MW
  30.917 | Steeled Frame                        ALT |  11 Assembler       |   2.811 / unit @ 94 % | 149.45 MW
   5.500 | Heavy Encased Frame                  ALT |   2 Manufacturer    |   2.750 / unit @ 98 % | 106.50 MW
  12.500 | Steel Rotor                          ALT |   3 Assembler       |   4.167 / unit @ 84 % |  34.05 MW
 104.667 | Pure Quartz Crystal                  ALT |   2 Refinery        |  52.333 / unit @ 100% |  60.00 MW
 180.896 | Steamed Copper Sheet                 ALT |   9 Refinery        |  20.100 / unit @ 90 % | 228.11 MW
 185.281 | Silica                                   |   5 Constructor     |  37.056 / unit @ 99 % |  19.68 MW
  33.438 | Silicone Circuit Board               ALT |   3 Assembler       |  11.146 / unit @ 90 % |  38.02 MW
 714.021 | Fused Quckwire                       ALT |   8 Assembler       |  89.253 / unit @ 100% | 120.00 MW
   8.938 | Silicone High-Speed Connector        ALT |   3 Manufacturer    |   2.979 / unit @ 100% | 165.00 MW
  17.467 | A.I. Limiter                             |   4 Assembler       |   4.367 / unit @ 88 % |  48.90 MW
 252.713 | Polymer Resin                        ALT |   2 Refinery        | 126.357 / unit @ 98 % |  58.09 MW
  46.357 | Residual Rubber                          |   3 Refinery        |  15.452 / unit @ 78 % |  60.48 MW
  67.205 | Residual Fuel                            |   2 Refinery        |  33.603 / unit @ 85 % |  46.26 MW
 134.410 | Recycled Rubber                      ALT |   3 Refinery        |  44.803 / unit @ 75 % |  56.80 MW
  10.467 | Insulated Crystal Oscillator         ALT |   6 Manufacturer    |   1.744 / unit @ 94 % | 298.89 MW
   7.500 | Crystal Computer                     ALT |   3 Assembler       |   2.500 / unit @ 89 % |  37.35 MW
   1.000 | Supercomputer                            |   1 Manufacturer    |   1.000 / unit @ 54 % |  20.52 MW
   8.875 | Quickwire Stator                     ALT |   2 Assembler       |   4.438 / unit @ 56 % |  11.86 MW
  67.700 | Steel Beam                               |   5 Constructor     |  13.540 / unit @ 91 % |  17.20 MW
  12.500 | Flexible Framework                   ALT |   2 Manufacturer    |   6.250 / unit @ 84 % |  83.22 MW
  10.000 | Polyester Fabric                     ALT |   2 Refinery        |   5.000 / unit @ 100% |  60.00 MW
   1.000 | Crystal Beacon                       ALT |   1 Manufacturer    |   1.000 / unit @ 10 % |   1.38 MW
  10.000 | Coated Cable                         ALT |   1 Refinery        |  10.000 / unit @ 15 % |   1.44 MW
   9.000 | Extract Sulfur with Miner Mk.2           |   1 Miner Mk.2      |   9.000 / unit @ 8  % |   0.21 MW
   3.750 | Automated Speed Wiring               ALT |   1 Manufacturer    |   3.750 / unit @ 50 % |  18.14 MW
   0.500 | Adaptive Control Unit                    |   1 Manufacturer    |   0.500 / unit @ 50 % |  18.14 MW
   3.000 | Compacted Coal                       ALT |   1 Assembler       |   3.000 / unit @ 12 % |   0.50 MW
  12.000 | Fine Black Powder                    ALT |   1 Assembler       |  12.000 / unit @ 80 % |  10.50 MW
  30.000 | Steel Rod                            ALT |   1 Constructor     |  30.000 / unit @ 63 % |   1.91 MW
   5.000 | Plastic Smart Plating                ALT |   1 Manufacturer    |   5.000 / unit @ 100% |  55.00 MW
  10.000 | Rigour Motor                         ALT |   2 Manufacturer    |   5.000 / unit @ 67 % |  57.96 MW
   2.500 | Modular Engine                           |   3 Manufacturer    |   0.833 / unit @ 84 % | 124.83 MW
   5.000 | Cartridge                                |   1 Manufacturer    |   5.000 / unit @ 34 % |   9.79 MW
   1.000 | Seismic Nobelisk                     ALT |   1 Manufacturer    |   1.000 / unit @ 17 % |   3.23 MW

TOTAL POWER                                                                                            4114.46 MW


INGREDIENTS

High-Speed Connector
  3.000 -> Supercomputer
  0.938 -> Alternate: Automated Speed Wiring
  5.000 -> STORAGE

Fuel
  67.205 -> Alternate: Recycled Rubber

Water
  240.000 -> Alternate: Wet Concrete
  176.637 -> Alternate: Pure Iron Ingot
  143.590 -> Alternate: Pure Caterium Ingot
  140.687 -> Alternate: Pure Copper Ingot
  74.762 -> Alternate: Pure Quartz Crystal
  180.896 -> Alternate: Steamed Copper Sheet
  92.713 -> Residual Rubber
  100.000 -> Alternate: Polyester Fabric

Steel Ingot
  559.408 -> Steel Pipe
  23.395 -> Alternate: Steel Coated Plate
  270.800 -> Steel Beam
  7.500 -> Alternate: Steel Rod

Quickwire
  268.125 -> Alternate: Silicone High-Speed Connector
  349.333 -> A.I. Limiter
  66.563 -> Alternate: Quickwire Stator
  30.000 -> STORAGE

Encased Industrial Beam
  18.333 -> Alternate: Heavy Encased Frame

Quartz Crystal
  104.667 -> Alternate: Insulated Crystal Oscillator

Raw Quartz
  134.571 -> Alternate: Pure Quartz Crystal
  111.169 -> Silica

A.I. Limiter
  10.467 -> Alternate: Insulated Crystal Oscillator
  2.000 -> Supercomputer
  5.000 -> STORAGE

Stator
  1.875 -> Alternate: Automated Speed Wiring
  5.000 -> Alternate: Rigour Motor
  2.000 -> STORAGE

Steel Beam
  37.500 -> Alternate: Flexible Framework
  0.200 -> Alternate: Crystal Beacon
  30.000 -> STORAGE

Iron Ore
  309.114 -> Alternate: Pure Iron Ingot

Copper Sheet
  73.563 -> Alternate: Silicone Circuit Board
  87.333 -> A.I. Limiter
  20.000 -> STORAGE

Modular Frame
  14.667 -> Alternate: Heavy Encased Frame
  6.250 -> Alternate: Flexible Framework
  10.000 -> STORAGE

Caterium Ore
  143.590 -> Alternate: Pure Caterium Ingot

Concrete
  91.667 -> Alternate: Encased Industrial Pipe
  40.333 -> Alternate: Heavy Encased Frame
  60.000 -> STORAGE

Copper Ingot
  49.173 -> Alternate: Fused Wire
  180.896 -> Alternate: Steamed Copper Sheet
  297.509 -> Alternate: Fused Quckwire

Sulfur
  3.000 -> Alternate: Compacted Coal
  6.000 -> Alternate: Fine Black Powder

Adaptive Control Unit
  0.500 -> STORAGE

Modular Engine
  2.500 -> STORAGE

Black Powder
  10.000 -> Cartridge
  2.000 -> Alternate: Seismic Nobelisk

Computer
  2.000 -> Supercomputer
  0.500 -> Adaptive Control Unit
  5.000 -> STORAGE

Iron Plate
  110.370 -> Alternate: Stitched Iron Plate
  30.000 -> STORAGE

Automated Wiring
  3.750 -> Adaptive Control Unit

Silica
  73.563 -> Alternate: Silicone Circuit Board
  111.719 -> Alternate: Silicone High-Speed Connector

Beacon
  1.000 -> Cartridge

Caterium Ingot
  12.293 -> Alternate: Fused Wire
  59.502 -> Alternate: Fused Quckwire

Supercomputer
  1.000 -> STORAGE

Heavy Oil Residue
  100.808 -> Residual Fuel
  2.222 -> Alternate: Coated Cable

Circuit Board
  8.938 -> Alternate: Silicone High-Speed Connector
  20.000 -> Alternate: Crystal Computer
  2.500 -> Adaptive Control Unit
  2.000 -> STORAGE

Iron Ingot
  574.069 -> Alternate: Solid Steel Ingot

Fabric
  10.000 -> STORAGE

Reinforced Iron Plate
  20.611 -> Alternate: Steeled Frame
  2.500 -> Alternate: Plastic Smart Plating
  10.000 -> STORAGE

Rotor
  2.500 -> Alternate: Plastic Smart Plating
  5.000 -> Alternate: Rigour Motor
  5.000 -> STORAGE

Compacted Coal
  3.000 -> Alternate: Fine Black Powder

Motor
  5.000 -> Modular Engine
  5.000 -> STORAGE

Wire
  220.741 -> Alternate: Stitched Iron Plate
  75.000 -> Alternate: Steel Rotor
  5.556 -> Alternate: Coated Cable
  37.500 -> Alternate: Automated Speed Wiring
  30.000 -> STORAGE

Smart Plating
  5.000 -> Modular Engine

Nobelisk
  1.000 -> STORAGE

Cable
  10.000 -> STORAGE

Limestone
  288.000 -> Alternate: Wet Concrete

Versatile Framework
  12.500 -> STORAGE

Plastic
  15.597 -> Alternate: Steel Coated Plate
  67.205 -> Alternate: Recycled Rubber
  28.000 -> Supercomputer
  7.500 -> Alternate: Plastic Smart Plating
  10.000 -> STORAGE

Iron Rod
  30.000 -> STORAGE

Steel Pipe
  128.333 -> Alternate: Encased Industrial Pipe
  103.056 -> Alternate: Steeled Frame
  66.000 -> Alternate: Heavy Encased Frame
  25.000 -> Alternate: Steel Rotor
  17.750 -> Alternate: Quickwire Stator
  0.800 -> Alternate: Crystal Beacon
  10.000 -> Cartridge
  2.000 -> Alternate: Seismic Nobelisk
  20.000 -> STORAGE

Rubber
  73.267 -> Alternate: Insulated Crystal Oscillator
  50.000 -> Alternate: Flexible Framework
  37.500 -> Modular Engine
  10.000 -> Cartridge
  10.000 -> STORAGE

Heavy Modular Frame
  0.500 -> Adaptive Control Unit
  5.000 -> STORAGE

Crystal Oscillator
  7.500 -> Alternate: Crystal Computer
  0.050 -> Alternate: Crystal Beacon
  1.667 -> Alternate: Rigour Motor
  0.250 -> Alternate: Seismic Nobelisk
  1.000 -> STORAGE

Polymer Resin
  92.713 -> Residual Rubber
  160.000 -> Alternate: Polyester Fabric

Crude Oil
  192.453 -> Plastic
  116.637 -> Alternate: Polymer Resin

Copper Ore
  211.031 -> Alternate: Pure Copper Ingot

Cartridge
  5.000 -> STORAGE

Coal
  574.069 -> Alternate: Solid Steel Ingot
  3.000 -> Alternate: Compacted Coal
```
</details>

## Development notes

### Import data for new update

- Adjust source entries in `DataVersionStorage`
- Create new `GrabDocs`, `GrabIcons` and `InitDatabaseModel` programs for the new entry
- Run these programs, as well as `IndexIcons`
- Commit the new docs/icon files

Note: hardcoded paths in `DataVersionStorage` (Satisfactory installs), `GrabIcons` (UE Viewer).
