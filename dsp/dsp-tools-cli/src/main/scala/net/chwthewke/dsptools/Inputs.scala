package net.chwthewke.dsptools

object Inputs {

  implicit class InputOps( private val self: Vector[( Int, Int )] ) {
    def except( ids: Int* ): Vector[( Int, Int )] =
      self.filterNot( kv => ids.contains( kv._1 ) )
  }

  val inputs1Simple: Vector[( Int, Int )] =
    Vector(
      1001 -> 1, // Iron ore
      1002 -> 1, // Copper ore
      1005 -> 1, // Stone
      1006 -> 1, // Coal
      1105 -> 4, // High-purity silicon
      1106 -> 4, // Titanium ingot
      1000 -> 1, // Water
      1007 -> 2  // Crude oil
    )

  val inputs2FireIceNoFluids: Vector[( Int, Int )] =
    Vector(
      1001 -> 1, // Iron ore
      1002 -> 1, // Copper ore
      //      1003 -> 1, // Silicon ore
      //      1004 -> 1, // Titanium ore
      1005 -> 1, // Stone
      1006 -> 1, // Coal
      1011 -> 2, // Fire ice
      1105 -> 4, // High-purity silicon
      1106 -> 4  // Titanium ingot
//      1000 -> 1, // Water
//      1007 -> 2  // Crude oil
      //      1116 -> 4, // Sulfuric acid
      //      1120 -> 1, // Hydrogen
      //      1121 -> 10, // Deuterium
      //      1208 -> 4, // Critical photon
    )

  val inputs3FireIceHydrogen: Vector[( Int, Int )] =
    Vector(
      1001 -> 1, // Iron ore
      1002 -> 1, // Copper ore
      //      1003 -> 1, // Silicon ore
      //      1004 -> 1, // Titanium ore
      1005 -> 1, // Stone
      1006 -> 1, // Coal
      1011 -> 2, // Fire ice
      1105 -> 4, // High-purity silicon
      1106 -> 4, // Titanium ingot
      1000 -> 1, // Water
      1007 -> 2, // Crude oil
      //      1116 -> 4, // Sulfuric acid
      1120 -> 1 // Hydrogen
      //      1121 -> 10, // Deuterium
      //      1208 -> 4, // Critical photon
    )

  val inputs4FireIceGases: Vector[( Int, Int )] =
    Vector(
      1001 -> 1, // Iron ore
      1002 -> 1, // Copper ore
      //      1003 -> 1, // Silicon ore
      //      1004 -> 1, // Titanium ore
      1005 -> 1, // Stone
      1006 -> 1, // Coal
      1011 -> 2, // Fire ice
//      1012 -> 2, // Kimberlite ore
      1014 -> 2, // Optical grating crystal
      1105 -> 4, // High-purity silicon
      1106 -> 4, // Titanium ingot
      1000 -> 1, // Water
      1007 -> 2, // Crude oil
      1116 -> 4, // Sulfuric acid
      1120 -> 1, // Hydrogen
      1121 -> 1  // Deuterium
      //      1208 -> 4, // Critical photon
    )

  val inputs5AllSpecials: Vector[( Int, Int )] =
    Vector(
      1001 -> 1, // Iron ore
      1002 -> 1, // Copper ore
      1005 -> 1, // Stone
      1006 -> 1, // Coal
      1011 -> 2, // Fire ice
      1012 -> 2, // Kimberlite ore
      1013 -> 2, // Fractal silicon
      1014 -> 2, // Optical grating crystal
      1015 -> 2, // Spiniform stalagmite crystal
      1016 -> 2, // Unipolar magnet
      1105 -> 4, // High-purity silicon
      1106 -> 4, // Titanium ingot
      1000 -> 1, // Water
      1007 -> 2, // Crude oil
      1116 -> 2, // Sulfuric acid
      1120 -> 1, // Hydrogen
      1121 -> 1, // Deuterium
      1117 -> 2  // Organic crystal
    )

  val inputs6AllSpecialsProl: Vector[( Int, Int )] =
    inputs5AllSpecials ++
      Vector( (1143 -> 2) )

  val inputs7AllSpecialsRaw: Vector[( Int, Int )] =
    Vector(
      1001 -> 1, // Iron ore
      1002 -> 1, // Copper ore
      1003 -> 1, // Silicon ore
      1004 -> 1, // Titanium ore
      1005 -> 1, // Stone
      1006 -> 1, // Coal
      1011 -> 2, // Fire ice
      1012 -> 2, // Kimberlite ore
      1013 -> 2, // Fractal silicon
      1014 -> 2, // Optical grating crystal
      1015 -> 2, // Spiniform stalagmite crystal
      1016 -> 2, // Unipolar magnet
      1000 -> 1, // Water
      1007 -> 2, // Crude oil
      1116 -> 2, // Sulfuric acid
      1120 -> 1, // Hydrogen
      1121 -> 1, // Deuterium
      1117 -> 2  // Organic crystal
    )

  val inputs8AllSpecialsPhotons: Vector[( Int, Int )] =
    inputs6AllSpecialsProl :+ ((1208 -> 10))

  val inputs9AllSpecialsRawPhotons: Vector[( Int, Int )] =
    inputs7AllSpecialsRaw :+ ((1208 -> 10))

}
