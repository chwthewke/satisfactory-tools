package net.chwthewke.dsptools

object Bills {
  val bill1 = // 60/min Small carrier rocket
    Vector(
      ( 1503, 60d )
    )

  val bill1a =
    Vector(
      ( 1501, 1800d ), // solar sail
      ( 1503, 360d ),  // small carrier rocket
      ( 1143, 60d ),   // extra prol
      ( 1210, 20d ),   // warpers
      ( 1209, 20d )    // Grav lens
    )

  val bill2 = // 6/min Graviton lens
    Vector(
      ( 1290, 6d )
    )

  val bill3 = // Prol mk3 for 7 blue belts
    Vector(
      ( 1143, 240d )
    )

  val bill3a =
    Vector( ( 1142, 7d * 60 * 3 / 24 ) )

  val bill3b =
    Vector( ( 1143, 1800d ) )

  val bill4 = // Structure matrix 180/min
    Vector(
      ( 6003, 180d )
    )

  val bill5 = // Mall tier3
    Vector(
      ( 2105, 0.2d ), // orb coll
      ( 2104, 0.1d ), // IP log st
      ( 2103, 0.1d ), // P log st
      ( 2206, 1d ),   // Accu
      ( 5002, 1d ),   // Log vessel
      ( 5001, 3d ),   // Log drone
      ( 2310, 0.2d ), // MPC
      ( 2211, 0.2d ), // Mini fusion
      ( 2209, 0.1d ), // EExch
      ( 2314, 0.5d ), // Fractio
      ( 2212, 0.5d ), // Satellite Subst
      ( 2202, 1d ),   // Wireless Pow tow
      ( 2201, 4d ),   // Tesla
      ( 2003, 6d ),   // convey 3
      ( 2003, 10d ),  // convey 2
      ( 2003, 20d ),  // convey 1
      ( 2013, 2d ),   // sorter 3
      ( 2012, 4d ),   // sorter 2
      ( 2011, 5d ),   // sorter 1
      ( 2304, 1d ),   // assmach 2
      ( 2303, 1d ),   // assmach 1
      ( 2040, 1d ),   // piler
      ( 2313, 1d ),   // spray
      ( 2030, 1d ),   // traffic mon
      ( 2205, 8d ),   // solar
      ( 2309, 1d ),   // chem pl
      ( 2308, 1d ),   // oil refinery
      ( 2307, 1d ),   // oil extr
      ( 2901, 1d ),   // mat lab
      ( 2213, 1d ),   // geo power
      ( 2203, 4d ),   // wind turb
      ( 2204, 2d ),   // thermal power
      ( 2106, 1d ),   // stor tank
      ( 2102, 1d ),   // stor 2
      ( 2101, 1d ),   // stor 1
      ( 2020, 1d ),   // splitter
      ( 2301, 2d ),   // miner
      ( 2306, 1d ),   // water pump
      ( 2302, 4d ),   // arc smelter
      ( 1125, 2d ),   // frame mat
      ( 1205, 12d ),  // extra procs
      ( 1303, 12d ),  // extra supermags
      ( 1142, 1d )    // prol mk2 for the extras
    )

  val bill5a = // Mall +
    bill5 ++ Vector(
      ( 2316, 0.2d ),
      ( 2315, 1d ),
      ( 2305, 1d )
    )

  val bill5b = // Mall ++
    bill5a ++ Vector(
      ( 2312, 0.1d ),
      ( 2210, 0.2d )
    )

  val bill6 = // Solar sails
    Vector(
      ( 1501, 1440d )
    )

  val bill7 = // Info matrix, 3/s
    Vector(
      ( 6004, 180d )
    )

  val bill8 = // Grav matrix, 3/s etc
    Vector(
      ( 6005, 180d ),
      ( 1209, 12d ), // grav lens
      ( 1210, 60d ), // warpers
      ( 1802, 10d )  // deut fuel rod
    )

  val bill9 = // 720 foundations
    Vector(
      ( 1131, 720d )
    )

  val bill10 = // 60 AM fuel rods
    Vector(
      ( 1803, 60d )
    )

  val bill11 = // lens+warper
    Vector(
      ( 1209, 720d ),
      ( 1210, 720d ),
      ( 1120, 4792.32d ),
      ( 1143, 12d ) // proliferator for lenses
    )

  val bill11a = // lens only
    Vector(
      ( 1209, 720d ),
      ( 1120, 4608d ),
      ( 1143, 12d ) // proliferator for lenses
    )

  val bill11b = // warpers only
    Vector(
      ( 1210, 720d ),
      ( 1120, 184.32d )
    )

  val bill12 = // Mall planet
    Vector(
      ( 5001, 20d ),  // Logistics drone
      ( 5002, 5d ),   // Logistics vessel
      ( 1131, 120d ), // Foundation
      ( 2001, 30d ),  // Conveyor belt MK.I
      ( 2002, 30d ),  // Conveyor belt MK.II
      ( 2003, 30d ),  // Conveyor belt MK.III
      ( 2011, 10d ),  // Sorter MK.I
      ( 2012, 10d ),  // Sorter MK.II
      ( 2013, 6d ),   // Sorter MK.III
      ( 2020, 4d ),   // Splitter
      ( 2040, 2d ),   // Automatic piler
      ( 2030, 1d ),   // 流速监测器
      ( 2313, 4d ),   // Spray coater
      ( 2101, 2d ),   // Storage MK.I
      ( 2102, 2d ),   // Storage MK.II
      ( 2106, 2d ),   // Storage tank
      ( 2303, 1d ),   // Assembling machine Mk.I
      ( 2304, 1d ),   // Assembling machine Mk.II
      ( 2305, 4d ),   // Assembling machine Mk.III
      ( 2201, 8d ),   // Tesla tower
      ( 2202, 2d ),   // Wireless power tower
      ( 2212, 2d ),   // Satellite substation
      ( 2203, 5d ),   // Wind turbine
      ( 2204, 2d ),   // Thermal power plant
      ( 2211, 2d ),   // Mini fusion power plant
      ( 2213, 4d ),   // Geothermal power station
      ( 2301, 1d ),   // Mining machine
      ( 2316, 2d ),   // Advanced mining machine
      ( 2306, 1d ),   // Water pump
      ( 2302, 1d ),   // Arc Smelter
      ( 2315, 4d ),   // Plane Smelter
      ( 2307, 2d ),   // Oil extractor
      ( 2308, 4d ),   // Oil refinery
      ( 2309, 4d ),   // Chemical plant
      ( 2314, 2d ),   // Fractionator
      ( 2205, 40d ),  // Solar panel
      ( 2206, 10d ),  // Accumulator
      ( 2311, 4d ),   // EM-Rail Ejector
      ( 2208, 2d ),   // Ray receiver
      ( 2312, 1d ),   // Vertical launching silo
      ( 2209, 1d ),   // Energy exchanger
      ( 2310, 2d ),   // Miniature particle collider
      ( 2210, 1d ),   // Artificial star
      ( 2103, 1d ),   // Planetary Logistics Station
      ( 2104, 1d ),   // Interstellar Logistics Station
      ( 2105, 4d ),   // Orbital Collector
      ( 2901, 2d )    // Matrix lab
    )
}
