package net.chwthewke.satisfactorytools
package persistence

import cats.effect.IO
import cats.effect.Resource
import cats.effect.unsafe.IORuntime
import cats.syntax.functor._
import doobie._
import doobie.implicits._
import doobie.scalatest.IOChecker
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import pureconfig.ConfigSource
import pureconfig.module.catseffect.syntax._

abstract class DatabaseSpec
    extends AnyWordSpec
    with Matchers
    with TypeCheckedTripleEquals
    with ScalaCheckDrivenPropertyChecks
    with BeforeAndAfterAll
    with IOChecker {

  protected implicit val ioRuntime: IORuntime = IORuntime.global

  private val resetP: ConnectionIO[Unit] =
    sql"""TRUNCATE TABLE "items", "machines"
         |  RESTART IDENTITY
         |  CASCADE
         |""".stripMargin.update.run.void

  private val testTransactor: Resource[IO, Transactor[IO]] =
    for {
      config <- Resource.eval( ConfigSource.default.at( "db" ).loadF[IO, Config]() )
      xa     <- Resources.managedTransactor[IO]( config )
      _      <- Resource.onFinalize( resetP.transact( xa ) )
    } yield xa

  private var _transactor: Transactor[IO] = _
  private var _tearDown: IO[Unit]         = _

  override protected def beforeAll(): Unit = {
    val ( t, r ) = testTransactor.allocated.unsafeRunSync()

    _transactor = t
    _tearDown = r

    super.beforeAll()
  }

  override protected def afterAll(): Unit = {
    _tearDown.unsafeRunSync()

    super.afterAll()
  }

  override def transactor: Transactor[IO] = _transactor
  def reset: IO[Unit]                     = resetP.transact( transactor )
}
