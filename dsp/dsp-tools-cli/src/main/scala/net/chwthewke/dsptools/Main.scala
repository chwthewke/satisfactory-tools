package net.chwthewke.dsptools

import cats.Semigroup
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp

import loader.Loader
import model.Item

object Main extends IOApp {
  val input =
    Vector(
      1001 -> 1, // Iron ore
      1002 -> 1, // Copper ore
//      1003 -> 1, // Silicon ore
//      1004 -> 1, // Titanium ore
      1005 -> 1, // Stone
      1006 -> 1, // Coal
//      1030 -> 2, // Log
//      1031 -> 2, // Plant fuel
//      1011 -> 2, // Fire ice
//      1012 -> 2, // Kimberlite ore
//      1013 -> 2, // Fractal silicon
//      1014 -> 2, // Optical grating crystal
//      1015 -> 2, // Spiniform stalagmite crystal
//      1016 -> 2, // Unipolar magnet
//      1101 -> 4, // Iron ingot
//      1104 -> 4, // Copper ingot
      1105 -> 4, // High-purity silicon
      1106 -> 4, // Titanium ingot
//      1108 -> 4, // Stone brick
//      1109 -> 4, // Energetic graphite
//      1103 -> 4, // Steel
//      1107 -> 4, // Titanium alloy
//      1110 -> 4, // Glass
//      1119 -> 4, // Titanium glass
//      1111 -> 4, // Prism
//      1112 -> 4, // Diamond
//      1113 -> 4, // Crystal silicon
//      1201 -> 4, // Gear
//      1102 -> 4, // Magnet
//      1202 -> 4, // Magnetic coil
//      1203 -> 4, // Electric motor
//      1204 -> 4, // Electromagnetic turbine
//      1205 -> 4, // Super-magnetic ring
//      1206 -> 4, // Particle container
//      1127 -> 4, // Strange matter
//      1301 -> 4, // Circuit board
//      1303 -> 4, // Processor
//      1305 -> 4, // Quantum chip
//      1302 -> 4, // Microcrystalline component
//      1304 -> 4, // Plane filter
//      1402 -> 4, // Particle broadband
//      1401 -> 4, // Plasma exciter
//      1404 -> 4, // Photon combiner
//      1501 -> 4, // Solar sail
      1000 -> 1, // Water
      1007 -> 2  // Crude oil
//      1114 -> 4, // Refined oil
//      1116 -> 4, // Sulfuric acid
//      1120 -> 1, // Hydrogen
//      1121 -> 10, // Deuterium
//      1122 -> 4, // Antimatter
//      1208 -> 4, // Critical photon
//      1801 -> 4, // Hydrogen fuel rod
//      1802 -> 4, // Deuteron fuel rod
//      1803 -> 4, // Antimatter fuel rod
//      1115 -> 4, // Plastic
//      1123 -> 4, // Graphene
//      1124 -> 4, // Carbon nanotube
//      1117 -> 4, // Organic crystal
//      1118 -> 4, // Titanium crystal
//      1126 -> 4, // Casimir crystal
//      1209 -> 4, // Graviton lens
//      1210 -> 4, // Space warper
//      1403 -> 4, // Annihilation constraint sphere
//      1405 -> 4, // Thruster
//      1406 -> 4, // Reinforced thruster
//      5001 -> 4, // Logistics drone
//      5002 -> 4, // Logistics vessel
//      1125 -> 4, // Frame material
//      1502 -> 4, // Dyson sphere component
//      1503 -> 4, // Small carrier rocket
//      1131 -> 4, // Foundation
//      1141 -> 4, // Proliferator Mk.I
//      1142 -> 4, // Proliferator Mk.II
//      1143 -> 4, // Proliferator Mk.III
//      2001 -> 4, // Conveyor belt MK.I
//      2002 -> 4, // Conveyor belt MK.II
//      2003 -> 4, // Conveyor belt MK.III
//      2011 -> 4, // Sorter MK.I
//      2012 -> 4, // Sorter MK.II
//      2013 -> 4, // Sorter MK.III
//      2020 -> 4, // Splitter
//      2040 -> 4, // Automatic piler
//      2030 -> 4, // 流速监测器
//      2313 -> 4, // Spray coater
//      2101 -> 4, // Storage MK.I
//      2102 -> 4, // Storage MK.II
//      2106 -> 4, // Storage tank
//      2303 -> 4, // Assembling machine Mk.I
//      2304 -> 4, // Assembling machine Mk.II
//      2305 -> 4, // Assembling machine Mk.III
//      2201 -> 4, // Tesla tower
//      2202 -> 4, // Wireless power tower
//      2212 -> 4, // Satellite substation
//      2203 -> 4, // Wind turbine
//      2204 -> 4, // Thermal power plant
//      2211 -> 4, // Mini fusion power plant
//      2213 -> 4, // Geothermal power station
//      2301 -> 4, // Mining machine
//      2316 -> 4, // Advanced mining machine
//      2306 -> 4, // Water pump
//      2302 -> 4, // Arc Smelter
//      2315 -> 4, // Plane Smelter
//      2307 -> 4, // Oil extractor
//      2308 -> 4, // Oil refinery
//      2309 -> 4, // Chemical plant
//      2314 -> 4, // Fractionator
//      2205 -> 4, // Solar panel
//      2206 -> 4, // Accumulator
//      2207 -> 4, // Accumulator(full)
//      2311 -> 4, // EM-Rail Ejector
//      2208 -> 4, // Ray receiver
//      2312 -> 4, // Vertical launching silo
//      2209 -> 4, // Energy exchanger
//      2310 -> 4, // Miniature particle collider
//      2210 -> 4, // Artificial star
//      2103 -> 4, // Planetary Logistics Station
//      2104 -> 4, // Interstellar Logistics Station
//      2105 -> 4, // Orbital Collector
//      2901 -> 4, // Matrix lab
//      6001 -> 4, // Electromagnetic matrix
//      6002 -> 4, // Energy matrix
//      6003 -> 4, // Structure matrix
//      6004 -> 4, // Information matrix
//      6005 -> 4, // Gravity matrix
//      6006 -> 4, // Universe matrix
    )

  val recipes =
    Vector(
      1,  // Iron ingot
      2,  // Magnet
      3,  // Copper ingot
      4,  // Stone brick
      5,  // Gear
      6,  // Magnetic coil
      7,  // Wind turbine
      8,  // Tesla tower
      9,  // Electromagnetic matrix
      10, // Matrix lab
      11, // Prism
      12, // Plasma exciter
      13, // Wireless power tower
      14, // Oil extractor
      15, // Oil refinery
      16, // Plasma refining
      17, // Energetic graphite
      18, // Energy matrix
      19, // Hydrogen fuel rod
      20, //  - Thruster
      21, // Reinforced thruster
      22, // Chemical plant
      23, // Plastic
      24, // Sulfuric acid
      25, // Organic crystal
      26, // Titanium crystal
      27, // Structure matrix
      28, // Casimir crystal
      29, // Casimir crystal (advanced)
      30, // Titanium glass
      31, // Graphene
      32, // Graphene (advanced)
      33, // Carbon nanotube
      34, // Silicon ore
      35, // Carbon nanotube (advanced)
      36, // Particle broadband
      37, // Crystal silicon
      38, // Plane filter
      39, // Miniature particle collider
//      40, // Deuterium
      41, // Deuteron fuel rod
      42, // Annihilation constraint sphere
      43, // Artificial star
      44, // Antimatter fuel rod
      45, // Assembling machine Mk.I
      46, // Assembling machine Mk.II
      47, // Assembling machine Mk.III
      48, // Mining machine
      49, // Water pump
      50, // Circuit board
      51, // Processor
      52, // Quantum chip
      53, // Microcrystalline component
      54, // Organic crystal (original)
      55, // Information matrix
      56, // Arc Smelter
      57, // Glass
//      58,  // X-ray cracking
      59,  // High-purity silicon
      60,  // Diamond
      61,  // Diamond (advanced)
      62,  // Crystal silicon (advanced)
      63,  // Steel
      64,  // Thermal power plant
      65,  // Titanium ingot
      66,  // Titanium alloy
      67,  // Solar panel
      68,  // Photon combiner
      69,  // Photon combiner (advanced)
      70,  // Solar sail
      71,  // EM-Rail Ejector
      72,  // Ray receiver
      73,  // Satellite substation
      74,  // Mass-energy storage
      75,  // Universe matrix
      76,  // Accumulator
      77,  // Energy exchanger
      78,  // Space warper
      79,  // Space warper (advanced)
      80,  // Frame material
      81,  // Dyson sphere component
      82,  // Vertical launching silo
      83,  // Small carrier rocket
      84,  // Conveyor belt MK.I
      85,  // Sorter MK.I
      86,  // Storage MK.I
      87,  // Splitter
      88,  // Sorter MK.II
      89,  // Conveyor belt MK.II
      90,  // Sorter MK.III
      91,  // Storage MK.II
      92,  // Conveyor belt MK.III
      93,  // Planetary Logistics Station
      94,  // Logistics drone
      95,  // Interstellar Logistics Station
      96,  // Logistics vessel
      97,  // Electric motor
      98,  // Electromagnetic turbine
      99,  // Particle container
      100, // Particle container (advanced)
      101, // Graviton lens
      102, // Gravity matrix
      103, // Super-magnetic ring
      104, // Strange matter
      106, // Proliferator Mk.I
      107, // Proliferator Mk.II
      108, // Proliferator Mk.III
      109, // Spray coater
      110, // Fractionator
      111, // Orbital Collector
      112, // Foundation
      113, // Mini fusion power plant
      114, // Storage tank
      115, // Deuterium fractionation
      116, // Plane Smelter
      117, // 流速监测器
      118, // Geothermal power station
      119, // Advanced mining machine
      120  // Automatic piler
    )

  import Plan._

  override def run( args: List[String] ): IO[ExitCode] =
    Loader.io.loadModel
      .map(
        model =>
          MainSolve
            .solve( model, plan14BigScience )
            .map {
              case ( requested, solution ) =>
                SolutionTable( model, requested, solution )
            }
            .merge
      )
      .flatTap( IO.println )
      .as( ExitCode.Success )

  case class ProdItem( item: Item ) extends AnyVal
  object ProdItem {
    implicit val prodItemSemigroup: Semigroup[ProdItem] =
      ( x, y ) => Vector( x, y ).minBy( _.item.productivityLevel )
  }

}
