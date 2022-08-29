package net.chwthewke.satisfactorytools
package web
package front

import cats.data.OptionT
import cats.effect.Async
import cats.syntax.functor._
import colibri.Observable
import colibri.Subject
import colibri.effect.RunEffect
import outwatch._
import outwatch.dsl._
import model.Model
import web.api.DefsApi
import web.front.http.DefsClient
import web.front.view.InitView
import web.front.vm.InitVm

class App[F[_]: Async: RunEffect] {
  val client: DefsApi[F]  = DefsClient[F]
  val vm: Subject[InitVm] = Subject.behavior( InitVm.init )

  def getModel: OptionT[F, Model] =
    OptionT
      .liftF( client.getVersions )
      .flatMap(
        versions => client.getModel( versions.maxBy( _._2.version )._1 )
      )

//  val model: Stream[F, InitVm] =
//    Stream
//      .eval( getModel.value )
//      .unNone
//      .scan[InitVm]( InitVm.init )( _.withModel( _ ) )

  val element: HtmlVNode =
    div(
      EmitterBuilder
        .fromSource( Observable.fromEffect( getModel.value ) )
        .map( modelOpt => InitVm.Loading( modelOpt ).loaded ) --> vm,
      vm.map( InitView( _ ) )
    )

}

object App {
  def apply[F[_]: RunEffect]( implicit F: Async[F] ): F[HtmlVNode] =
    F.delay( new App[F] ).map( _.element )
}
