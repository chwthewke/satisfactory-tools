package net.chwthewke.dsptools

case class Plan(
    inputs: Vector[( Int, Int )],
    bill: Vector[( Int, Double )],
    recipes: Vector[Int],
    maxProductivity: Int
)

object Plan {
  import Bills._
  import Inputs._
  import Recipes._

  val plan1Lenses: Plan =
    Plan( inputs1Simple, bill2, defaultRecipes, 2 )

  val plan2StructureMatrix: Plan =
    Plan( inputs1Simple, bill4, defaultRecipes, 2 )

  val plan3Mall: Plan =
    Plan( inputs1Simple, bill5, defaultRecipes, 2 )

  val plan4SolarSails: Plan =
    Plan( inputs2FireIceNoFluids, bill6, defaultRecipes, 2 )

  val plan5InfoMatrix: Plan =
    Plan( inputs3FireIceHydrogen, bill7, defaultRecipes, 4 )

  val plan6GravityMatrix: Plan =
    Plan( inputs4FireIceGases, bill8, defaultRecipes, 4 )

  val plan7Proliferator: Plan =
    Plan( inputs5AllSpecials, bill3b, defaultRecipes, 4 )

  val plan8MallWithAdditions: Plan =
    Plan( inputs5AllSpecials, bill5b, defaultRecipes, 4 )

  val plan9Foundations: Plan =
    Plan( inputs6AllSpecialsProl, bill9, defaultRecipes, 4 )

  val plan10AntimatterFuel: Plan =
    Plan( inputs8AllSpecialsPhotons, bill10, defaultRecipes.except( 99 ), 4 )

  val plan11Rockets2: Plan =
    Plan( inputs7AllSpecialsRaw, bill1a, recipes1Rocket2, 4 )

  val plan12LensesWarpers: Plan =
    Plan( inputs7AllSpecialsRaw.except( 1012, 1120 ), bill11, recipes2LensWarper, 4 )

}
