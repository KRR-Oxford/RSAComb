package rsacomb

sealed trait RSASuffix {
  def getSuffix: String;
}

object RSASuffix {

  case object None extends RSASuffix {
    def getSuffix: String = ""
  }

  case object Forward extends RSASuffix {
    def getSuffix: String = "_f"
  }

  case object Backward extends RSASuffix {
    def getSuffix: String = "_b"
  }
}
