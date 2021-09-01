package net.chwthewke.satisfactorytools
package web.view

trait StyleClass {
  def name: String

  protected def variant( suffix: String ): StyleClass = StyleClass( s"$name-$suffix" )
}

object StyleClass {
  def apply( name0: String ): StyleClass = new StyleClass {
    override def name: String = name0
  }

  object text extends ColorContext {
    override def prefix: String = "text"
  }

  object bg extends ColorContext {
    override def prefix: String = "bg"
  }

  trait ColorContext {
    def prefix: String

    val black: StyleClass = StyleClass( s"$prefix-black" )
    val white: StyleClass = StyleClass( s"$prefix-white" )

    val gray: BaseColor   = new BaseColor( s"$prefix-gray" )
    val red: BaseColor    = new BaseColor( s"$prefix-red" )
    val yellow: BaseColor = new BaseColor( s"$prefix-yellow" )
    val green: BaseColor  = new BaseColor( s"$prefix-green" )
    val blue: BaseColor   = new BaseColor( s"$prefix-blue" )
    val indigo: BaseColor = new BaseColor( s"$prefix-indigo" )
    val purple: BaseColor = new BaseColor( s"$prefix-purple" )
    val pink: BaseColor   = new BaseColor( s"$prefix-pink" )
  }

  class BaseColor( val color: String ) {
    private def variant( suffix: String ): StyleClass = StyleClass( s"$color-$suffix" )

    val _50: StyleClass  = variant( "50" )
    val _100: StyleClass = variant( "100" )
    val _200: StyleClass = variant( "200" )
    val _300: StyleClass = variant( "300" )
    val _400: StyleClass = variant( "400" )
    val _500: StyleClass = variant( "500" )
    val _600: StyleClass = variant( "600" )
    val _700: StyleClass = variant( "700" )
    val _800: StyleClass = variant( "800" )
    val _900: StyleClass = variant( "900" )

  }

  sealed abstract class BasePadding( val prefix: String ) {
    def _0: StyleClass   = StyleClass( s"$prefix-0" )
    def _0_5: StyleClass = StyleClass( s"$prefix-0.5" )
    def _1: StyleClass   = StyleClass( s"$prefix-1" )
    def _1_5: StyleClass = StyleClass( s"$prefix-1.5" )
    def _2: StyleClass   = StyleClass( s"$prefix-2" )
    def _2_5: StyleClass = StyleClass( s"$prefix-2.5" )
    def _3: StyleClass   = StyleClass( s"$prefix-3" )
    def _3_5: StyleClass = StyleClass( s"$prefix-3.5" )
    def _4: StyleClass   = StyleClass( s"$prefix-4" )
    def _5: StyleClass   = StyleClass( s"$prefix-5" )
    def _6: StyleClass   = StyleClass( s"$prefix-6" )
    def _7: StyleClass   = StyleClass( s"$prefix-7" )
    def _8: StyleClass   = StyleClass( s"$prefix-8" )
    def _9: StyleClass   = StyleClass( s"$prefix-9" )
    def _10: StyleClass  = StyleClass( s"$prefix-10" )
    def _11: StyleClass  = StyleClass( s"$prefix-11" )
    def _12: StyleClass  = StyleClass( s"$prefix-12" )
    def _14: StyleClass  = StyleClass( s"$prefix-14" )
    def _16: StyleClass  = StyleClass( s"$prefix-16" )
    def _20: StyleClass  = StyleClass( s"$prefix-20" )
    def _24: StyleClass  = StyleClass( s"$prefix-24" )
    def _28: StyleClass  = StyleClass( s"$prefix-28" )
    def _32: StyleClass  = StyleClass( s"$prefix-32" )
    def _36: StyleClass  = StyleClass( s"$prefix-36" )
    def _40: StyleClass  = StyleClass( s"$prefix-40" )
    def _44: StyleClass  = StyleClass( s"$prefix-44" )
    def _48: StyleClass  = StyleClass( s"$prefix-48" )
    def _52: StyleClass  = StyleClass( s"$prefix-52" )
    def _56: StyleClass  = StyleClass( s"$prefix-56" )
    def _60: StyleClass  = StyleClass( s"$prefix-60" )
    def _72: StyleClass  = StyleClass( s"$prefix-72" )
    def _80: StyleClass  = StyleClass( s"$prefix-80" )
    def _96: StyleClass  = StyleClass( s"$prefix-96" )
  }

  object pad extends BasePadding( "p" ) {
    val horizontal: BasePadding = new BasePadding( "px" ) {}
    val vertical: BasePadding   = new BasePadding( "py" ) {}

    val bottom: BasePadding = new BasePadding( "pb" ) {}
    val top: BasePadding    = new BasePadding( "pt" ) {}
    val left: BasePadding   = new BasePadding( "pl" ) {}
    val right: BasePadding  = new BasePadding( "pr" ) {}
  }

}
