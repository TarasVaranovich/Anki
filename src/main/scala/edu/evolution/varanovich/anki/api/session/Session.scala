package edu.evolution.varanovich.anki.api.session

import cats.Monad
import cats.effect.concurrent.Ref
import cats.effect.{Clock, Concurrent, Timer}
import cats.implicits._

import scala.concurrent.duration.{FiniteDuration, MILLISECONDS}

/**
 * Cache implementation is borrowed from homework "Shared state"
 */
object Session {
  trait Cache[F[_], K, V] {
    def get(key: K): F[Option[V]]

    def put(key: K, value: V): F[Unit]

    def clear: F[Unit]
  }

  class RefCache[F[_] : Clock : Monad, K, V](state: Ref[F, Map[K, (Long, V)]], expiresIn: FiniteDuration)
    extends Cache[F, K, V] {

    def get(key: K): F[Option[V]] = state.get.map(_.get(key).map { case (_, value) => value })

    def put(key: K, value: V): F[Unit] = Clock[F].realTime(MILLISECONDS)
      .flatMap(time => state.update(_.++(Map(key -> ((time + expiresIn.toMillis), value)))))

    def clear: F[Unit] = Clock[F].realTime(MILLISECONDS)
      .flatMap(time => state.update(_.filter { case (_, (expired, _)) => expired >= time }))
  }

  object Cache {

    def of[F[_] : Clock, K, V](expiresIn: FiniteDuration, checkOnExpirationsEvery: FiniteDuration)
                              (implicit T: Timer[F], C: Concurrent[F]): F[Cache[F, K, V]] = {

      def clearCache(cache: Cache[F, K, V]): F[Unit] = for {
        _ <- T.sleep(checkOnExpirationsEvery)
        _ <- cache.clear
      } yield ()

      for {
        map <- C.delay(Map[K, (Long, V)]())
        reference <- Ref.of(map)
        refCache <- C.delay(new RefCache[F, K, V](reference, expiresIn))
        _ <- C.start(clearCache(refCache).foreverM.void)
        cache <- C.delay(refCache)
      } yield cache
    }
  }
}