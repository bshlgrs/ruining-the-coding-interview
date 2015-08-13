package finatra_server

case class HiRequest(
  id: Long,
  name: String)

case class CompilationRequest(contents: String)
