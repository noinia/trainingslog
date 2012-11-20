package net.fstaals.tl.helpers

object Helpers {
  def opt[T](t : => T) = try { Some(t) } catch { case _  => None }
}
