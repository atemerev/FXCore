package ai.reactivity.fxcore

import java.util.UUID

trait Id {
  val id: UUID = UUID.randomUUID()
}
