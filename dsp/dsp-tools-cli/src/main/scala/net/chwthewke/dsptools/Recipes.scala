package net.chwthewke.dsptools

object Recipes {
  val allRecipes: Vector[Int] =
    Vector(
      1,   // Iron ingot
      2,   // Magnet
      3,   // Copper ingot
      4,   // Stone brick
      5,   // Gear
      6,   // Magnetic coil
      7,   // Wind turbine
      8,   // Tesla tower
      9,   // Electromagnetic matrix
      10,  // Matrix lab
      11,  // Prism
      12,  // Plasma exciter
      13,  // Wireless power tower
      14,  // Oil extractor
      15,  // Oil refinery
      16,  // Plasma refining
      17,  // Energetic graphite
      18,  // Energy matrix
      19,  // Hydrogen fuel rod
      20,  //  - Thruster
      21,  // Reinforced thruster
      22,  // Chemical plant
      23,  // Plastic
      24,  // Sulfuric acid
      25,  // Organic crystal
      26,  // Titanium crystal
      27,  // Structure matrix
      28,  // Casimir crystal
      29,  // Casimir crystal (advanced)
      30,  // Titanium glass
      31,  // Graphene
      32,  // Graphene (advanced)
      33,  // Carbon nanotube
      34,  // Silicon ore
      35,  // Carbon nanotube (advanced)
      36,  // Particle broadband
      37,  // Crystal silicon
      38,  // Plane filter
      39,  // Miniature particle collider
      40,  // Deuterium
      41,  // Deuteron fuel rod
      42,  // Annihilation constraint sphere
      43,  // Artificial star
      44,  // Antimatter fuel rod
      45,  // Assembling machine Mk.I
      46,  // Assembling machine Mk.II
      47,  // Assembling machine Mk.III
      48,  // Mining machine
      49,  // Water pump
      50,  // Circuit board
      51,  // Processor
      52,  // Quantum chip
      53,  // Microcrystalline component
      54,  // Organic crystal (original)
      55,  // Information matrix
      56,  // Arc Smelter
      57,  // Glass
      58,  // X-ray cracking
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

  val defaultRecipes: Vector[Int] = allRecipes.except( 40, 58, 115 )

  val recipes1Rocket2: Vector[Int]    = defaultRecipes.except( 28, 99 )
  val recipes2LensWarper: Vector[Int] = allRecipes.except( 40, 115, 29, 100 )

  implicit class RecipesOps( private val self: Vector[Int] ) extends AnyVal {
    def except( ids: Int* ): Vector[Int] = self.filterNot( ids.toSet )

    def noXRay: Vector[Int]      = except( 58 )
    def noDeuterium: Vector[Int] = except( 40, 115 )
  }

}
